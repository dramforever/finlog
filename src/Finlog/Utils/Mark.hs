module Finlog.Utils.Mark
    ( module Finlog.Utils.Mark
    , module Data.Text.Prettyprint.Doc
    ) where

import           Control.Monad.Except
import qualified Data.Text as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Text.Megaparsec

data Ann = HeadWordAnn | PositionAnn | CodeAnn
    deriving (Show, Eq)

annStyle :: Ann -> AnsiStyle
annStyle HeadWordAnn = bold <> color Red
annStyle PositionAnn = underlined
annStyle CodeAnn = bold

toAnsi :: Doc Ann -> Doc AnsiStyle
toAnsi = reAnnotate annStyle

headAnn, posAnn, codeAnn :: Doc Ann -> Doc Ann
headAnn = annotate HeadWordAnn
posAnn = annotate PositionAnn
codeAnn = annotate CodeAnn

prettyPos :: SourcePos -> Doc Ann
prettyPos = posAnn . pretty . T.pack . sourcePosPretty

data Mark = Mark (Doc Ann) SourcePos

instance Show Mark where
    show = show . prettyMark

prettyMark :: Mark -> Doc Ann
prettyMark (Mark doc pos) = doc <> ":" <+> prettyPos pos

data CompilerError
    = CompilerError (Doc Ann) [Mark]
    deriving (Show)

prettyCompilerError :: CompilerError -> Doc Ann
prettyCompilerError (CompilerError err []) = headAnn "Error:" <+> err
prettyCompilerError (CompilerError err marks) =
    headAnn "Error:" <+> (align . vsep)
        [ err
        , "at" <+> (align . vsep . map prettyMark $ reverse marks)
        ]

compilerError :: MonadError CompilerError m => Doc Ann -> m a
compilerError err = throwError (CompilerError err [])

catchMark :: MonadError CompilerError m => Mark -> m a -> m a
catchMark mark act =
    act `catchError` \(CompilerError err rest) ->
        throwError $ CompilerError err (mark : rest)
