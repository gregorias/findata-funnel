-- | This module handles using pdf to text utilities.
module PdfToText (
  pdf2text,
  pdftotext,
) where

import Control.Exception (try)
import Control.Monad.Except (MonadError (throwError))
import Data.Either.Combinators (whenLeft)
import Relude hiding (whenLeft)
import qualified Turtle

-- | Runs 'pdftotext' utility.
--
-- https://en.wikipedia.org/wiki/Pdftotext
pdftotext ::
  (MonadError e m, MonadIO m, e ~ Text) =>
  -- | Input PDF path
  Text ->
  -- | Output text path
  Text ->
  m ()
pdftotext pdfFile txtFile = do
  result <-
    liftIO $
      try @Turtle.ExitCode . Turtle.sh . void $
        Turtle.inproc "pdftotext" ["-raw", pdfFile, txtFile] mempty
  whenLeft result (const . throwError $ "pdftotext has failed.")

-- | Runs 'pdf2text' utility.
pdf2text ::
  -- | Input PDF content
  Turtle.Shell Turtle.Line ->
  -- | Output text lines
  Turtle.Shell Turtle.Line
pdf2text = Turtle.inproc "pdf2text" []
