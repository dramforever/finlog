{-# LANGUAGE TemplateHaskell #-}
module Finlog.IR.Build where

import qualified Data.HashMap.Strict as HM
import           Finlog.Framework.DAG
import           Finlog.Framework.Graph
import           Finlog.Frontend.AST
import           Finlog.IR.Node
import           Finlog.Utils.Error
import           Finlog.Utils.Unique
import           GHC.Stack
import           Lens.Micro.Platform

data VarInfo
    = VarInfo
    { _varReg :: Reg
    , _varType :: Typ
    }
    deriving (Show, Eq)

newtype BuildState = BuildState
    { _varMap :: HM.HashMap Var VarInfo
    }
    deriving (Show, Eq)

$(makeLenses ''VarInfo)
$(makeClassy ''BuildState)

initialBuildState :: BuildState
initialBuildState = BuildState HM.empty

lookupVar :: _ => Getting a VarInfo a -> Var -> m a
lookupVar getting var@(Var name) = use (varMap . at var) >>= \case
    Just vinfo -> pure (vinfo ^. getting)
    Nothing -> compilerError $
        "Variable not in scope:" <+> codeAnn (pretty name)

recordLookup :: _ => Expr -> m IName
recordLookup expr = traverse (lookupVar varReg) expr >>= record

loop :: (HasCallStack, _) => Graph (Node m) 'O 'O -> m (Label, Graph (Node m) 'C 'C)
loop gr = (\lbl -> (lbl, mkLoop lbl)) <$> freshLabel "loop"
    where mkLoop lbl = mkLabel lbl >< gr >< mkFinal (JumpN lbl)

buildProcess :: (HasCallStack, _) => Process -> m (Label, Graph (Node m) 'C 'C)
buildProcess (Process (Var name) stmts) = do
    entry <- freshLabel $ "entry_" <> name
    graph <- buildBlock stmts
    pure $ (entry, mkLabel entry >< graph >< mkFinal UndefinedN)

buildBlock :: (HasCallStack, _) => StmtBlock -> m (Graph (Node m) 'O 'O)
buildBlock stmts = do
    origVarMap <- use varMap
    gr <- catGraphs <$> traverse buildStmt stmts
    varMap .= origVarMap
    pure gr

buildStmt :: (HasCallStack, _) => Stmt -> m (Graph (Node m) 'O 'O)
buildStmt (Stmt pos stmt) = catchPosition pos $ buildStmtRaw stmt

buildStmtRaw :: (HasCallStack, _) => StmtRaw -> m (Graph (Node m) 'O 'O)
buildStmtRaw (BlockS stmts) = buildBlock stmts
buildStmtRaw (DeclareS var@(Var name) typ initial) = do
    reg <- freshReg name
    let info = VarInfo
            { _varReg = reg
            , _varType = typ
            }
    varMap . at var ?= info
    initialName <- recordLookup initial
    pure $ mkNode (StoreN reg initialName)
buildStmtRaw (AssignS var expr) = do
    reg <- lookupVar varReg var
    exprName <- recordLookup expr
    pure $ mkNode (StoreN reg exprName)
buildStmtRaw YieldS = mkNode <$> (YieldN <$> freshUnique)
buildStmtRaw (LoopS body) = do
    unreachable <- freshLabel "unreachable"
    (loopLabel, loopGraph) <- buildBlock body >>= loop
    pure $
        mkFinal (JumpN loopLabel)
        >|< loopGraph
        >|< mkLabel unreachable
buildStmtRaw (WhileS cond body) = do
    condName <- recordLookup cond
    enter <- freshLabel "enter"
    out <- freshLabel "out"
    bodyGraph <- buildBlock body
    (loopLabel, loopGraph) <- loop $
        mkFinal (CondN condName enter out)
        >|< (mkLabel enter >< bodyGraph)
    pure $
        mkFinal (JumpN loopLabel)
        >|< loopGraph
        >|< mkLabel out
buildStmtRaw (IfS cond tblk eblkMay) = do
    thenL <- freshLabel "then"
    mergeL <- freshLabel "merge"
    condName <- recordLookup cond
    thenGraph <- buildBlock tblk
    let thenGraph' =
            mkLabel thenL
            >< thenGraph
            >< mkFinal (JumpN mergeL)
    case eblkMay of
        Nothing -> pure $
            mkFinal (CondN condName thenL mergeL)
            >|< thenGraph'
            >|< mkLabel mergeL
        Just eblk -> do
            elseL <- freshLabel "else"
            elseGraph <- buildBlock eblk
            let elseGraph' =
                    mkLabel elseL
                    >< elseGraph
                    >< mkFinal (JumpN mergeL)
            pure $
                mkFinal (CondN condName thenL elseL)
                >|< thenGraph' >|< elseGraph'
                >|< mkLabel mergeL
