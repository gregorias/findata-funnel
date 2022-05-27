-- | This module handles using pdf to text utilities.
module PdfToText (
  pdf2txt,
  pdftotext,
  PdfToTextMode (..),
) where

import Control.Monad.Except (MonadError (throwError), liftEither)
import Control.Monad.Managed (Managed, with)
import qualified Data.Text.IO as T
import Relude hiding (whenLeft)
import Turtle (ExitCode (ExitFailure, ExitSuccess), (</>))
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
  -- | The transcoded PDF
  m Text
pdftotext mode pdfFile = do
  let modeArg = case mode of
        Raw -> "-raw"
        Layout -> "-layout"
  maybeTxt :: Either Text Text <- liftIO $
    unsafeRunManaged $ do
      txtFile <- Turtle.mktempfile (Turtle.decodeString "/tmp") ""
      (exitCode, _, stderr') <-
        liftIO $
          Turtle.procStrictWithErr
            "pdftotext"
            [modeArg, fpToText pdfFile, fpToText txtFile]
            mempty
      case exitCode of
        ExitFailure _ -> return $ Left ("pdftotext has failed.\n" <> stderr')
        ExitSuccess -> liftIO $ Right <$> T.readFile (Turtle.encodeString txtFile)
  either throwError return maybeTxt
 where
  fromEither :: Either a a -> a
  fromEither = either id id

  fpToText :: Turtle.FilePath -> Text
  fpToText = fromEither . Turtle.toText

  unsafeRunManaged :: Managed a -> IO a
  unsafeRunManaged = flip with return

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
