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
import           Finlog.Framework.Analysis
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
            let hline = liftIO $ T.putStrLn (T.replicate 10 "=")
            liftIO $ print entry
            liftIO $ printMap (graph ^. blockMap)
            hline
            lm <- use labelMarks
            liftIO $ printMap lm
            hline
            ym <- use yieldLabels
            liftIO $ printMap ym
            hline

            liftIO $ T.putStrLn (T.replicate 3 "\n")
            liftIO $ putStrLn "=== Liveness ==="
            liveness <- livenessAnalysis graph
            liftIO $ printMap liveness
            hline
            forM_ (sortOn fst $ HM.toList lm) $ \(lbl, stmt) ->
                liftIO $ putStrLn (show (listToMaybe stmt) ++ " => " ++ show (liveness HM.! lbl))
            hline

            liftIO $ T.putStrLn (T.replicate 3 "\n")
            liftIO $ putStrLn "=== Symbolic ==="
            symbolic <- symbolicAnalysis graph
            liftIO $ prettyMap symbolic
            hline
            forM_ (sortOn fst $ HM.toList lm) $ \(lbl, stmt) -> do
                liftIO . putStrLn $ show (listToMaybe stmt) ++ " => "
                liftIO $ print . pretty $ symbolic HM.! lbl
            hline

            ks <- HM.keys <$> use fwdMap
            forM_ ks $ \k -> do
                expr <- report k
                liftIO . putStrLn $ show k ++ " => " ++ showCleanFree expr
            hline

            yls <- use yieldLabels
            let mk yid lbl =
                    case symbolic HM.! lbl of
                        Symbolic sym -> Symbolic $ sym
                            & traversed . traversed . symState . ssYield .~ YieldYT yid
                            & traversed . traversed . symBranches . traversed . ssYield .~ YieldYT yid
            f <- joinFacts $ uncurry mk <$> HM.toList yls
            liftIO . print . pretty $ f
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
