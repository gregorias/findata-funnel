-- | This module handles using pdf to text utilities.
module PdfToText (
  pdftotext,
  PdfToTextMode (..),
) where

import Control.Monad.Except (MonadError (throwError))
import Relude hiding (whenLeft)
import Turtle (ExitCode (ExitFailure, ExitSuccess))
import qualified Turtle

data PdfToTextMode = Raw | Layout

-- | Runs 'pdftotext' utility.
--
-- https://en.wikipedia.org/wiki/Pdftotext
--
-- This function returns an in-memory text, which makes this function
-- unsuitable for large files, but allows callers to decide whether they want
-- to interact with the filesystem. In the context of findata-funnel, I don't
-- think large files will happen, so the return type makes this function more
-- composable.
pdftotext ::
  (MonadError e m, MonadIO m, e ~ Text) =>
  PdfToTextMode ->
  -- | Input PDF path
  Turtle.FilePath ->
  -- | Output txt file path
  Turtle.FilePath ->
  -- | The transcoded PDF
  m ()
pdftotext mode pdfFile txtFile = do
  let modeArg = case mode of
        Raw -> "-raw"
        Layout -> "-layout"
  (exitCode, _, stderr') <-
    liftIO $
      Turtle.procStrictWithErr
        "pdftotext"
        [modeArg, fpToText pdfFile, fpToText txtFile]
        mempty
  case exitCode of
    ExitFailure _ -> throwError $ "pdftotext has failed.\n" <> stderr'
    ExitSuccess -> return ()
 where
  fromEither :: Either a a -> a
  fromEither = either id id

  fpToText :: Turtle.FilePath -> Text
  fpToText = fromEither . Turtle.toText
