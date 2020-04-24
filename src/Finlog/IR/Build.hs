{-# LANGUAGE TemplateHaskell #-}
module Finlog.IR.Build where

import           Control.Monad
import qualified Data.HashMap.Strict as HM
import           Data.Maybe
import qualified Data.Text as T
import           Finlog.Framework.DAG
import           Finlog.Framework.Graph
import           Finlog.Frontend.AST
import           Finlog.Frontend.Type
import           Finlog.IR.Node
import           Finlog.Utils.Mark
import           GHC.Stack
import           Lens.Micro.Platform

data VarInfo
    = VarInfo
    { _varReg :: Reg
    , _varType :: Typ
    }
    deriving (Show, Eq)

data BuildState = BuildState
    { _varMap :: HM.HashMap Var VarInfo
    , _labelMarks :: HM.HashMap Label [Mark]
    , _yieldLabels :: HM.HashMap YieldId Label
    , _yieldPostLabels :: HM.HashMap YieldId Label
    , _markStack :: [Mark]
    }
    deriving (Show)

$(makeLenses ''VarInfo)
$(makeClassy ''BuildState)

withMark :: _ => Mark -> m a -> m a
withMark mark act = do
    oldMarks <- use markStack
    markStack %= (mark :)
    res <- catchMark mark act
    markStack .= oldMarks
    pure res

freshLabelMark :: _ => T.Text -> m Label
freshLabelMark name = do
    label <- freshLabel name
    use markStack >>= (labelMarks . at label ?=)
    pure label

initialBuildState :: BuildState
initialBuildState =
    BuildState
    { _varMap = HM.empty
    , _labelMarks = HM.empty
    , _yieldLabels = HM.empty
    , _yieldPostLabels = HM.empty
    , _markStack = []
    }

lookupVar :: _ => Getting a VarInfo a -> Var -> m a
lookupVar getting var@(Var name) = use (varMap . at var) >>= \case
    Just vinfo -> pure (vinfo ^. getting)
    Nothing -> compilerError $
        "Variable not in scope:" <+> codeAnn (pretty name)

recordLookup :: _ => Expr -> m IName
recordLookup expr = traverse (lookupVar varReg) expr >>= record

loop :: (HasCallStack, _) => Graph (Node m) 'O 'O -> m (Label, Graph (Node m) 'C 'C)
loop gr = (\lbl -> (lbl, mkLoop lbl)) <$> freshLabelMark "loop"
    where mkLoop lbl = mkLabel lbl >< gr >< mkFinal (JumpN lbl)

data ProcessBuild m
    = ProcessBuild
    { _pbGraph :: Graph (Node m) 'C 'C
    , _pbEntry :: Label
    , _pbResetId :: YieldId
    , _pbYieldLabels :: HM.HashMap YieldId Label
    , _pbYieldPostLabels :: HM.HashMap YieldId Label
    }
    deriving (Show)

$(makeLenses ''ProcessBuild)

buildProcess :: (HasCallStack, _) => Process -> m (ProcessBuild m)
buildProcess (Process (Var name) stmts) = do
    oldYieldLabels <- use yieldLabels
    oldYieldPostLabels <- use yieldPostLabels

    entry <- freshLabelMark $ "entry_" <> name
    entryPost <- freshLabelMark $ "entrypost_" <> name
    resetId <- freshYieldId
    yieldLabels . at resetId ?= entry
    yieldPostLabels . at resetId ?= entryPost

    graph <- buildBlock stmts
    let graph' = mkLabel entry
            >< mkNode (YieldN resetId)
            >< mkFinal (JumpN entryPost)
            >|< mkLabel entryPost
            >< graph
            >< mkFinal UndefinedN

    yl <- use yieldLabels
    ypol <- use yieldPostLabels
    yieldLabels .= oldYieldLabels
    yieldPostLabels .= oldYieldPostLabels

    pure $ ProcessBuild
        { _pbGraph = graph'
        , _pbEntry = entry
        , _pbResetId = resetId
        , _pbYieldLabels = yl
        , _pbYieldPostLabels = ypol
        }

buildBlock :: (HasCallStack, _) => StmtBlock -> m (Graph (Node m) 'O 'O)
buildBlock stmts = do
    origVarMap <- use varMap
    gr <- catGraphs <$> traverse buildStmt stmts
    varMap .= origVarMap
    pure gr

nonPred :: a -> (a -> Bool) -> Lens' (Maybe a) a
nonPred x pr afb s = f <$> afb (fromMaybe x s)
  where f y = if pr y then Nothing else Just y

buildStmt :: (HasCallStack, _) => Stmt -> m (Graph (Node m) 'O 'O)
buildStmt (Stmt pos stmt) = withMark (Mark "statement" pos) $ do
    label <- freshLabelMark "stmt"
    labelMarks . at label . nonPred [] null %= (Mark "statement" pos :)
    graph <- buildStmtRaw stmt
    pure $ mkFinal (JumpN label) >|< mkLabel label >< graph

checkAssignable :: _ => Typ -> Typ -> m ()
checkAssignable dst src =
    unless (assignable dst src)
        (compilerError $
            "Type" <+> codeAnn (viaShow src)
            <+> "not assignable to" <+> codeAnn (viaShow dst))


buildStmtRaw :: (HasCallStack, _) => StmtRaw -> m (Graph (Node m) 'O 'O)
buildStmtRaw (BlockS stmts) = buildBlock stmts
buildStmtRaw (DeclareS var@(Var name) typ initial) = do
    reg <- freshReg name
    regType reg ?= typ
    let info = VarInfo
            { _varReg = reg
            , _varType = typ
            }
    varMap . at var ?= info
    initialName <- recordLookup initial
    initialType <- infer initialName
    checkAssignable typ initialType
    pure $ mkNode (StoreN reg initialName)
buildStmtRaw (AssignS var expr) = do
    reg <- lookupVar varReg var
    rType <- fromJust <$> use (regType reg)
    exprName <- recordLookup expr
    exprType <- infer exprName
    checkAssignable rType exprType
    pure $ mkNode (StoreN reg exprName)
buildStmtRaw YieldS = do
    yid <- freshYieldId
    lbl <- freshLabelMark "yield"
    lblPost <- freshLabelMark "yieldpost"
    yieldLabels . at yid ?= lbl
    yieldPostLabels . at yid ?= lblPost
    pure $
        mkFinal (JumpN lbl)
        >|< mkLabel lbl >< mkNode (YieldN yid) >< mkFinal (JumpN lblPost)
        >|< mkLabel lblPost

buildStmtRaw (LoopS body) = do
    unreachable <- freshLabelMark "unreachable"
    (loopLabel, loopGraph) <- buildBlock body >>= loop
    pure $
        mkFinal (JumpN loopLabel)
        >|< loopGraph
        >|< mkLabel unreachable
buildStmtRaw (WhileS cond body) = do
    condName <- recordLookup cond
    enter <- freshLabelMark "enter"
    out <- freshLabelMark "out"
    bodyGraph <- buildBlock body
    (loopLabel, loopGraph) <- loop $
        mkFinal (CondN condName enter out)
        >|< (mkLabel enter >< bodyGraph)
    pure $
        mkFinal (JumpN loopLabel)
        >|< loopGraph
        >|< mkLabel out
buildStmtRaw (IfS cond tblk eblkMay) = do
    thenL <- freshLabelMark "then"
    mergeL <- freshLabelMark "merge"
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
            elseL <- freshLabelMark "else"
            elseGraph <- buildBlock eblk
            let elseGraph' =
                    mkLabel elseL
                    >< elseGraph
                    >< mkFinal (JumpN mergeL)
            pure $
                mkFinal (CondN condName thenL elseL)
                >|< thenGraph' >|< elseGraph'
                >|< mkLabel mergeL
