{-# LANGUAGE TemplateHaskell #-}
module Finlog.Backend.Verilog.Translate where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import           Data.List
import qualified Data.Text as T
import qualified Finlog.Backend.Verilog.AST as V
import           Finlog.Framework.DAG
import           Finlog.Frontend.AST
import           Finlog.IR.Analysis.Symbolic
import           Finlog.IR.Node
import           Finlog.Utils.Unique
import           Lens.Micro.Platform

data TranslationInput
    = TranslationInput
    { _tiSymbolic :: HM.HashMap YieldId SymState
    , _tiYieldIdSet :: HS.HashSet YieldId
    , _tiResetId :: YieldId
    , _tiFwdMap :: HM.HashMap IName (Item ExprF)
    , _tiRegTypeMap :: HM.HashMap Reg Typ
    , _tiItemTypeMap :: HM.HashMap IName Typ
    }
    deriving (Show)

$(makeLenses ''TranslationInput)

mkTyp :: Typ -> V.Typ
mkTyp (IntType Bit) = V.Bit
mkTyp (IntType (Unsigned s)) = V.Unsigned s
mkTyp (IntType (Signed s)) = V.Signed s

mkLit :: Literal -> V.Literal
mkLit (IntLitL (IntLit int ty)) = V.Literal int (mkTyp (IntType ty))

stateReg :: V.VerilogVar
stateReg = V.VerilogVar "$state"

mkReg :: Reg -> V.VerilogVar
mkReg (Reg name (Unique n)) =
    V.VerilogVar $ "$reg$" <> name <> "$" <> T.pack (show n)

mkRegD :: Reg -> Typ -> V.Decl
mkRegD reg typ = V.TypeDecl V.Reg (mkTyp typ) (mkReg reg)

mkIName :: IName -> V.VerilogVar
mkIName (IName (Unique n)) =
    V.VerilogVar $ "$_" <> T.pack (show n)

mkINameD :: IName -> Typ -> V.Decl
mkINameD iname typ = V.TypeDecl V.Wire (mkTyp typ) (mkIName iname)

mkItem :: (IName, Typ, Item ExprF) -> ([V.Decl], [V.Decl])
mkItem (iname, typ, RegItem reg) =
    ( [mkINameD iname typ]
    , [V.Assign (V.VarE $ mkIName iname) (V.VarE $ mkReg reg)]
    )

mkItem (iname, typ, ComplexItem exprf) =
    ( [mkINameD iname typ]
    , [ V.Assign
            (V.VarE $ mkIName iname)
            (mkExprF $ V.VarE . mkIName <$> exprf)
      ]
    )

mkExprF :: ExprF V.Expr -> V.Expr
mkExprF (LitE lit) = V.LitE (mkLit lit)
mkExprF (BinE op lhs rhs) = V.BinE (mkBinOp op) lhs rhs
mkExprF (CondE cond t e) = V.CondE cond t e

mkBinOp :: BinOp -> V.BinOp
mkBinOp Add = V.Add

generateRegs :: TranslationInput -> [V.Decl]
generateRegs tin =
    let regList = sortOn fst . HM.toList $ tin ^. tiRegTypeMap
        go (reg, typ) = V.TypeDecl V.Reg (mkTyp typ) (mkReg reg)
    in go <$> regList

generateMatrix :: TranslationInput -> [V.Decl]
generateMatrix tin =
    let common =
            fmap (\(k, (v1, v2)) -> (k, v1, v2))
            . sortOn fst
            . HM.toList
            $ HM.intersectionWith (,)
                (tin ^. tiItemTypeMap)
                (tin ^. tiFwdMap)
        itemDefs = mkItem <$> common
    in (itemDefs >>= fst) ++ (itemDefs >>= snd)

generateGate :: TranslationInput -> [V.Decl]
generateGate tin =
    let resetId = tin ^. tiResetId
        usualStates = HS.delete resetId (tin ^. tiYieldIdSet)
        numStates = HS.size usualStates
        numBits :: Int
        numBits = max 1 $ ceiling (logBase 2 $ fromIntegral numStates :: Double)
        stateLit n = V.Literal n (V.Unsigned numBits)
        stateMap =
            HM.fromList
            $ zip
                (sort $ HS.toList usualStates)
                (stateLit <$> [0 ..])
        symbolicList =
            sortOn fst
            . HM.toList
            . HM.delete resetId
            $ tin ^. tiSymbolic

        genYT (YieldYT yid) = V.LitE $ stateMap HM.! yid
        genYT (CondYT iname t e) =
            V.CondE (V.VarE $ mkIName iname) (genYT t) (genYT e)

        genSym (SymState _ yt as) = V.Block $
            (V.VarE stateReg V.:<=: genYT yt)
            : ((\(r, i) -> (V.VarE $ mkReg r) V.:<=: (V.VarE $ mkIName i))
                <$> sortOn fst (HM.toList as))
        genBranch (yid, sym) = (stateMap HM.! yid, genSym sym)
    in  [ V.TypeDecl V.Reg (V.Unsigned numBits) stateReg
        , V.AlwaysFF V.Posedge "clk"
            (V.If (V.VarE "rst")
                (genSym $ tin ^?! tiSymbolic . at resetId. _Just)
                (Just $ V.Case (V.VarE stateReg)
                    $ genBranch <$> symbolicList))
        ]

generateDecls :: T.Text -> TranslationInput -> V.Module
generateDecls name tin =
    V.Module (V.VerilogVar name) ["clk", "rst"] $
        [ V.DirDecl V.Input V.Bit "clk"
        , V.DirDecl V.Input V.Bit "rst"
        ]
        ++ generateRegs tin
        ++ generateMatrix tin
        ++ generateGate tin
