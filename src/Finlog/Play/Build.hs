{-# LANGUAGE TemplateHaskell #-}
module Finlog.Play.Build where

import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.HashMap.Strict as HM
import           Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Finlog.Framework.DAG
import           Finlog.Framework.Graph
import           Finlog.Frontend.AST
import qualified Finlog.Frontend.Parser as Parser
import           Finlog.IR.Build
import           Finlog.Utils.Error
import           Finlog.Utils.Unique
import           Lens.Micro.Platform
import           Text.Megaparsec

buildSomething :: _ => String -> m ()
buildSomething fileName = do
    programText <- liftIO $ T.readFile fileName
    let parsed = case parse Parser.program fileName programText of
            Left err -> error $ errorBundlePretty err
            Right result -> result
    use fwdMap >>= liftIO . printMap
    case parsed of
        Program procs -> forM_ procs $ \process -> do
            (entry, graph) <- buildProcess process
            liftIO $ print entry
            liftIO $ printMap (graph ^. blockMap)
            liftIO $ T.putStrLn (T.replicate 10 "=")
    use fwdMap >>= liftIO . printMap
    liftIO $ T.putStrLn (T.replicate 10 "=")

printMap :: _ => HM.HashMap k v -> IO ()
printMap = mapM_ print . sortOn fst . HM.toList

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

run :: _ => ExceptT CompilerError (StateT (ProgState ExprF) m) a -> m ()
run act = evalStateT (runExceptT act) initial >>= \case
    Left err -> do
        liftIO $ putDoc (toAnsi $ prettyCompilerError err)
        liftIO $ putStrLn ""
    Right result -> liftIO $ print result
    where initial = ProgState initialSupply emptyItemMap initialBuildState
