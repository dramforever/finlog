{-# LANGUAGE TemplateHaskell #-}
module Finlog.Play.Build where

import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import           Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Text.Prettyprint.Doc.Render.Terminal
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
import           Text.Megaparsec

buildAndAnalyze :: _ => String -> m ()
buildAndAnalyze fileName = do
    programText <- liftIO $ T.readFile fileName
    let parsed = case parse Parser.program "" programText of
            Left err -> error $ errorBundlePretty err
            Right result -> result
    use fwdMap >>= liftIO . printMap
    case parsed of
        Program procs -> forM_ procs $ \process@(Process (Var name) _) -> do
            build <- buildProcess process
            let entry = build ^. pbEntry
                graph = build ^. pbGraph
            let hline = liftIO $ T.putStrLn (T.replicate 70 "=")
            liftIO . putStrLn $ "Entrypoint is " ++ show entry
            liftIO $ printMap (graph ^. blockMap)

            hline
            liftIO $ putStrLn "=== Marks ==="
            lm <- use labelMarks
            liftIO $ printMap lm
            hline

            -- liftIO $ putStrLn "=== Liveness ==="
            -- liveness <- livenessAnalysis graph
            -- liftIO $ printMap liveness
            -- hline
            -- forM_ (sortOn fst $ HM.toList lm) $ \(lbl, stmt) ->
            --     liftIO $ putStrLn (show (listToMaybe stmt) ++ " => " ++ show (liveness HM.! lbl))
            -- hline

            liftIO $ putStrLn "=== Symbolic ==="
            symbolic <- symbolicAnalysis build
            allINames <- HM.keys <$> use fwdMap
            allINames `forM_` infer
            -- liftIO . printMap $ symbolic
            -- hline
            HM.toList (build ^. pbYieldLabels) `forM_` \(yid, lbl) ->
                let ysym = HM.lookupDefault HM.empty lbl symbolic
                    ysym' = ysym & traversed . ssYield .~ (YieldYT yid)
                in liftIO . print $ (yid, lbl, ysym')
            hline
            let recode (yid, lbl) =
                    HM.lookupDefault HM.empty lbl symbolic
                    & traversed . ssYield .~ YieldYT yid
            merged <- foldM mergeSymMap HM.empty . map recode $ HM.toList (build ^. pbYieldLabels)
            liftIO . printMap $ merged
            hline
            liftIO . putStrLn $ "rst = " ++ show (build ^. pbResetId)
            hline

            fm <- use fwdMap
            sortOn fst (HM.toList fm) `forM_` \(k, v) -> do
                let showItem (RegItem reg) = show reg
                    showItem (ComplexItem citem) = show citem
                ty <- infer k
                liftIO . putStrLn $ show k ++ " : " ++ show ty ++ " => " ++ showItem v
                expr <- report k
                liftIO . putStrLn $ "    = " ++ showCleanFree expr
            hline

            rtym <- use regTypeMap
            itym <- use inameTypeMap

            let tin = TranslationInput
                    { _tiSymbolic = merged
                    , _tiYieldIdSet = HS.fromMap (() <$ build ^. pbYieldLabels)
                    , _tiResetId = build ^. pbResetId
                    , _tiFwdMap = fm
                    , _tiRegTypeMap = rtym
                    , _tiItemTypeMap = itym
                    }

            let verilog = generateDecls name tin
            liftIO . print $ verilog
            hline
            liftIO . print $ genModule verilog
            hline

printMap :: _ => HM.HashMap k v -> IO ()
printMap = mapM_ printKV . sortOn fst . HM.toList
    where printKV (k, v) = putStrLn $ show k ++ " => " ++ show v

prettyMap :: _ => HM.HashMap k v -> IO ()
prettyMap = print . vsep . map printKV . sortOn fst . HM.toList
    where printKV (k, v) = viaShow k <> " => " <> pretty v

data ProgState f
    = ProgState
    { _psUniqueSupply :: UniqueSupply
    , _psItemMap :: ItemMap f
    , _psBuildState :: BuildState
    , _psTypeMap :: TypeMap
    }

deriving instance Show (f IName) => Show (ProgState f)

$(makeLenses ''ProgState)

instance HasUniqueSupply (ProgState f) where uniqueSupply = psUniqueSupply
instance HasItemMap (ProgState f) f where itemMap = psItemMap
instance HasBuildState (ProgState f) where buildState = psBuildState
instance HasTypeMap (ProgState f) where typeMap = psTypeMap

run :: _ => ExceptT CompilerError (StateT (ProgState ExprF) m) a -> m a
run act = evalStateT (runExceptT act) initial >>= \case
    Left err -> do
        liftIO $ putDoc (toAnsi $ prettyCompilerError err)
        liftIO $ putStrLn ""
        error "Error occured"
    Right result -> pure result
    where initial = ProgState initialSupply emptyItemMap initialBuildState initialTypeMap

playBuild :: IO ()
playBuild = run (buildAndAnalyze "examples/play.finlog")
