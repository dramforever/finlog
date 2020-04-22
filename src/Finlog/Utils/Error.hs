module Finlog.Utils.Error
    ( module Finlog.Utils.Error
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

data CompilerError
    = CompilerError (Doc Ann) [SourcePos]
    deriving (Show)

prettyCompilerError :: CompilerError -> Doc Ann
prettyCompilerError (CompilerError err []) = headAnn "Error:" <+> err
prettyCompilerError (CompilerError err pos) =
    headAnn "Error:" <+> (align . vsep)
        [ err
        , "at" <+> (align . vsep . map prettyPos $ reverse pos)
        ]
    where prettyPos = posAnn . pretty . T.pack . sourcePosPretty

compilerError :: MonadError CompilerError m => Doc Ann -> m a
compilerError err = throwError (CompilerError err [])

catchPosition :: MonadError CompilerError m => SourcePos -> m a -> m a
catchPosition pos act =
    act `catchError` \(CompilerError err rest) ->
        throwError $ CompilerError err (pos : rest)
