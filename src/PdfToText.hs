-- | This module handles using pdftotext.
module PdfToText (
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
  whenLeft result (const . throwError $ "The pdftotext has failed.")
