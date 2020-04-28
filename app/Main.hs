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
import           Finlog.Framework.Graph
import           Finlog.Frontend.AST
import qualified Finlog.Frontend.Parser as Parser
import           Finlog.Frontend.Type
import           Finlog.IR.Analysis.Symbolic
import           Finlog.IR.Build
import           Finlog.Utils.Mark
import           Finlog.Utils.Pretty
import           Finlog.Utils.Unique
import           Lens.Micro.Platform
import           System.Environment
import           System.Exit
import           System.IO
import           Text.Megaparsec


genProcess :: Process -> IO V.Module
genProcess process@(Process name _ _) = run $ do
    build <- buildProcess process

    sortOn fst (HM.toList (build ^. pbGraph . blockMap)) `forM_` \(l, g) ->
        liftIO . hPutStrLn stderr $ show l ++ " => " ++ show g

    symbolic <- symbolicAnalysis build
    allINames <- HM.keys <$> use fwdMap
    allINames `forM_` infer

    liftIO . hPutStrLn stderr $ "=== symbolic done ==="

    let regs = catMaybes $
            (`HM.lookup` symbolic)
            <$> HM.elems (build ^. pbYieldLabels)

    sort (HM.elems (build ^. pbYieldLabels)) `forM_` \yl ->
        liftIO . hPutStrLn stderr $ show yl ++ " => " ++ show (HM.lookup yl symbolic)

    mergedSym <- mergeSymMaps regs

    let recode (yid, yl) = case HM.lookup yl symbolic of
            Nothing -> HM.empty
            Just m -> fmap go m
            where go ss = (ss ^. ssCond, yid)
        combine =
            foldl' (HM.unionWith (++)) HM.empty
            . (fmap . fmap) (: [])
        combined = combine $ recode <$> HM.toList (build ^. pbYieldLabels)

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
                    combined
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

prettyLabelTree :: ProgState f -> Doc Ann
prettyLabelTree sta = vsep $ go' <$> (reverse . map reverse) (sta ^. labelTree)
    where
        go' = vsep . fmap go
        go (LabelTree lbl []) =
            heading lbl
        go (LabelTree lbl ch) = vsep
            [ heading lbl
            , indent 4 $ go' ch
            ]
        heading lbl =
            "-" <+> codeAnn (viaShow lbl) <+> markPart
            where
                markPart = case sta ^. labelMarks . at lbl of
                    Just (h : _) -> prettyMark h
                    _ -> "<unknown>"

run :: ExceptT CompilerError (StateT (ProgState ExprF) IO) a -> IO a
run act = runStateT (runExceptT act) initial >>= \case
    (Left err, sta) -> do
        hPutDoc stderr (toAnsi $ prettyCompilerError err)
        hPutStrLn stderr ""
        hPutStrLn stderr ""
        hPutStrLn stderr "Label tree:"
        hPutDoc stderr (toAnsi $ prettyLabelTree sta)
        hPutStrLn stderr ""
        exitFailure
    (Right result, _sta) -> pure result
    where initial = ProgState initialSupply emptyItemMap initialBuildState initialTypeMap

$(makeLenses ''ProgState)

instance HasUniqueSupply (ProgState f) where uniqueSupply = psUniqueSupply
instance HasItemMap (ProgState f) f where itemMap = psItemMap
instance HasBuildState (ProgState f) where buildState = psBuildState
instance HasTypeMap (ProgState f) where typeMap = psTypeMap
