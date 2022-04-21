-- | This module handles using pdf to text utilities.
module PdfToText (
  pdf2txt,
  pdftotext,
) where

import Control.Exception (try)
import Control.Monad.Except (MonadError (throwError), liftEither)
import Data.Either.Combinators (whenLeft)
import Relude hiding (whenLeft)
import Turtle ((</>))
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

-- | Runs 'pdf2txt' utility.
pdf2txt ::
  (MonadError e io, MonadIO io, e ~ Text) =>
  -- | Input PDF path
  Text ->
  -- | Output text
  io Text
pdf2txt pdf = do
  homeDir <- Turtle.home
  pdf2txtexe <- liftEither <$> Turtle.toText $ homeDir </> ".local/bin/pdf2txt"
  (exitCode, txt) <- Turtle.procStrict pdf2txtexe [pdf] mempty
  case exitCode of
    Turtle.ExitSuccess -> return txt
    Turtle.ExitFailure _ -> throwError "pdf2txt has failed.\n"
