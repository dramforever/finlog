{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import           Data.List
import           Data.Maybe
import qualified Data.Text.IO as T
import           Data.Text.Prettyprint.Doc.Render.Terminal
import qualified Finlog.Backend.Verilog.AST as V
import           Finlog.Backend.Verilog.Codegen
import           Finlog.Backend.Verilog.Translate
import           Finlog.Framework.DAG
import           Finlog.Frontend.AST
import qualified Finlog.Frontend.Parser as Parser
import           Finlog.Frontend.Type
import           Finlog.IR.Analysis.Symbolic
import           Finlog.IR.Build
import           Finlog.Utils.Mark
import           Finlog.Utils.Unique
import           Lens.Micro.Platform
import           System.Environment
import           System.Exit
import           Text.Megaparsec

genProcess :: Process -> IO V.Module
genProcess process@(Process name _ _) = run $ do
    build <- buildProcess process

    symbolic <- symbolicAnalysis build
    allINames <- HM.keys <$> use fwdMap
    allINames `forM_` infer

    mergedSym <- mergeSymMaps . catMaybes $
        (`HM.lookup` symbolic) <$> HM.elems (build ^. pbYieldLabels)

    let recode (yid, yl) = case HM.lookup yl symbolic of
            Nothing -> HM.empty
            Just m -> fmap go m
            where go ss = (ss ^. ssCond, yid)
        combine =
            foldl' (HM.unionWith (++)) HM.empty
            . (fmap . fmap) (: [])
        combined = combine $ recode <$> HM.toList (build ^. pbYieldLabels)
        genTreeError lbl tree = case genTree tree of
            Nothing ->
                compilerError $ "Could not generate tree for" <+> codeAnn (viaShow lbl)
            Just (_, val) -> pure val

    mergedYield <- HM.traverseWithKey genTreeError combined

    fm <- use fwdMap
    sortOn fst (HM.toList fm) `forM_` \(k, _) -> infer k

    rtym <- use regTypeMap
    itym <- use inameTypeMap

    let tin = TranslationInput
            { _tiName = name
            , _tiInputVars = build ^. pbInputVars
            , _tiOutputVars = build ^. pbOutputVars
            , _tiSymbolic =
                HM.intersectionWith (,)
                    mergedYield
                    ((^. ssRegs) <$> mergedSym)
            , _tiYieldIdSet = HS.fromMap (() <$ build ^. pbYieldLabels)
            , _tiResetId = build ^. pbResetId
            , _tiFwdMap = fm
            , _tiRegTypeMap = rtym
            , _tiItemTypeMap = itym
            }

    pure $ generateDecls tin

main :: IO ()
main = getArgs >>= \case
    [] -> usage
    (_ : _ : _) -> usage
    [fileName] -> do
        programText <- T.readFile fileName
        Program parsed <-
            case parse Parser.program fileName programText of
                Right x -> pure x
                Left err -> do
                    putStrLn $ errorBundlePretty err
                    exitFailure
        verilog <- V.Verilog <$> parsed `forM` genProcess
        putDoc $ gen verilog
        putStrLn ""

usage :: IO ()
usage = do
    prog <- getProgName
    putStrLn $ "Usage: " ++ prog ++ " <file>"

data ProgState f
    = ProgState
    { _psUniqueSupply :: UniqueSupply
    , _psItemMap :: ItemMap f
    , _psBuildState :: BuildState
    , _psTypeMap :: TypeMap
    }

run :: ExceptT CompilerError (StateT (ProgState ExprF) IO) a -> IO a
run act = evalStateT (runExceptT act) initial >>= \case
    Left err -> do
        putDoc (toAnsi $ prettyCompilerError err)
        putStrLn ""
        exitFailure
    Right result -> pure result
    where initial = ProgState initialSupply emptyItemMap initialBuildState initialTypeMap

$(makeLenses ''ProgState)

instance HasUniqueSupply (ProgState f) where uniqueSupply = psUniqueSupply
instance HasItemMap (ProgState f) f where itemMap = psItemMap
instance HasBuildState (ProgState f) where buildState = psBuildState
instance HasTypeMap (ProgState f) where typeMap = psTypeMap
