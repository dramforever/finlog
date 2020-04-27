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
    { _tiName :: Var
    , _tiInputVars :: HM.HashMap Var Typ
    , _tiOutputVars :: HM.HashMap Var Reg
    , _tiSymbolic :: HM.HashMap YieldId (CondTree YieldId, RegState)
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
stateReg = V.VerilogVar "state$"

mkInput :: Var -> V.VerilogVar
mkInput (Var name) = V.VerilogVar $ "in$" <> name

mkOutput :: Var -> V.VerilogVar
mkOutput (Var name) = V.VerilogVar $ "out$" <> name

mkReg :: Reg -> V.VerilogVar
mkReg (Reg name (Unique n)) =
    V.VerilogVar $ "reg$" <> name <> "$" <> T.pack (show n)

mkRegD :: Reg -> Typ -> V.Decl
mkRegD reg typ = V.Reg (mkTyp typ) (mkReg reg)

mkIName :: IName -> V.VerilogVar
mkIName (IName (Unique n)) =
    V.VerilogVar $ "_$" <> T.pack (show n)

mkINameD :: IName -> Typ -> V.Expr -> V.Decl
mkINameD iname typ expr = V.Wire (mkTyp typ) (mkIName iname) expr

mkItem :: IName -> Typ -> Item ExprF -> V.Decl
mkItem iname typ (RegItem reg) =
    mkINameD iname typ (V.VarE $ mkReg reg)
mkItem iname typ (ComplexItem exprf) =
    mkINameD iname typ (mkExprF $ V.VarE . mkIName <$> exprf)

mkExprF :: ExprF V.Expr -> V.Expr
mkExprF (LitE lit) = V.LitE (mkLit lit)
mkExprF (InputE var _) = V.VarE (mkInput var)
mkExprF (UnaryE op rhs) = V.UnaryE op rhs
mkExprF (BinE op lhs rhs) = V.BinE op lhs rhs
mkExprF (CondE cond t e) = V.CondE cond t e

generateRegs :: TranslationInput -> [V.Decl]
generateRegs tin =
    let regList = sortOn fst . HM.toList $ tin ^. tiRegTypeMap
        go (reg, typ) = V.Reg (mkTyp typ) (mkReg reg)
    in go <$> regList

generateMatrix :: TranslationInput -> [V.Decl]
generateMatrix tin =
    fmap (\(k, (v1, v2)) -> mkItem k v1 v2)
    . sortOn fst
    . HM.toList
    $ HM.intersectionWith (,)
        (tin ^. tiItemTypeMap)
        (tin ^. tiFwdMap)

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

        genYT (LeafCT yid) = V.LitE $ stateMap HM.! yid
        genYT (CondCT iname t e) =
            V.CondE (V.VarE $ mkIName iname) (genYT t) (genYT e)

        genRegs regs =
            ((\(r, i) -> (V.VarE $ mkReg r) V.:<=: (V.VarE $ mkIName i))
                <$> sortOn fst (HM.toList regs))

        genBranch (yid, (yt, regs)) =
            ( stateMap HM.! yid
            , V.Block $
                (V.VarE stateReg V.:<=: genYT yt)
                : genRegs regs
            )

    in  [ V.Reg (V.Unsigned numBits) stateReg
        , V.AlwaysFF V.Posedge "clk"
            (V.If (V.VarE "rst")
                (snd . genBranch $ (resetId, tin ^?! tiSymbolic . at resetId . _Just))
                (Just $ V.Case (V.VarE stateReg)
                    $ genBranch <$> symbolicList))
        ]

generateInputs :: TranslationInput -> [V.Decl]
generateInputs tin = gen <$> HM.toList (tin ^. tiInputVars)
    where
        gen (var, typ) =
            V.DirDecl V.Input (mkTyp typ) (mkInput var)

generateOutputs :: TranslationInput -> ([V.Decl], [V.Decl])
generateOutputs tin = unzip $ gen <$> HM.toList (tin ^. tiOutputVars)
    where
        gen (var, reg) =
            let typ = tin ^?! tiRegTypeMap . at reg . _Just
            in  ( V.DirDecl V.Output (mkTyp typ) (mkOutput var)
                , V.Assign (V.VarE $ mkOutput var) (V.VarE $ mkReg reg)
                )

generateDecls :: TranslationInput -> V.Module
generateDecls tin =
    V.Module (V.VerilogVar name) ports $
        [ V.DirDecl V.Input V.Bit "clk"
        , V.DirDecl V.Input V.Bit "rst"
        ]
        ++ generateInputs tin
        ++ outputDecls
        ++ generateRegs tin
        ++ outputAssigns
        ++ generateMatrix tin
        ++ generateGate tin
    where
        (outputDecls, outputAssigns) = generateOutputs tin
        Var name = tin ^. tiName
        ports =
            ["clk",  "rst"]
            ++ (mkInput <$> HM.keys (tin ^. tiInputVars))
            ++ (mkOutput <$> HM.keys (tin ^. tiOutputVars))
