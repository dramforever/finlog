{-# LANGUAGE TemplateHaskell #-}
module Finlog.Play.Build where

import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.HashMap.Strict as HM
import           Data.List
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Finlog.Framework.DAG
import           Finlog.Framework.Graph
import           Finlog.Frontend.AST
import qualified Finlog.Frontend.Parser as Parser
import           Finlog.IR.Analysis.Liveness
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
        Program procs -> forM_ procs $ \process -> do
            build <- buildProcess process
            let entry = build ^. pbEntry
                graph = build ^. pbGraph
            let hline = liftIO $ T.putStrLn (T.replicate 40 "=")
            liftIO $ print entry
            liftIO $ printMap (graph ^. blockMap)
            hline
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

            fm <- HM.toList <$> use fwdMap
            sortOn fst fm `forM_` \(k, v) -> do
                liftIO . putStrLn $ show k ++ " => " ++ show v
                expr <- report k
                liftIO . putStrLn $ "    = " ++ showCleanFree expr
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
    }

deriving instance Show (f IName) => Show (ProgState f)

$(makeLenses ''ProgState)

instance HasUniqueSupply (ProgState f) where uniqueSupply = psUniqueSupply
instance HasItemMap (ProgState f) f where itemMap = psItemMap
instance HasBuildState (ProgState f) where buildState = psBuildState

run :: _ => ExceptT CompilerError (StateT (ProgState ExprF) m) a -> m a
run act = evalStateT (runExceptT act) initial >>= \case
    Left err -> do
        liftIO $ putDoc (toAnsi $ prettyCompilerError err)
        liftIO $ putStrLn ""
        error "Error occured"
    Right result -> pure result
    where initial = ProgState initialSupply emptyItemMap initialBuildState

playBuild :: IO ()
playBuild = run (buildAndAnalyze "examples/play.finlog")
