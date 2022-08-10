-- | This module handles using pdf to text utilities.
module PdfToText (
  pdftotext,
  PdfToTextMode (..),
  PdfToTextOutputMode (..),
) where

import Control.Foldl.ByteString (ByteString)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Either.Extra (fromEither)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Turtle (ExitCode (ExitFailure, ExitSuccess))
import qualified Turtle
import qualified Turtle.Bytes

data PdfToTextMode = Raw | Layout

data PdfToTextOutputMode outputType where
  PttOutputModeFilePath :: Turtle.FilePath -> PdfToTextOutputMode ()
  PttOutputModeStdOut :: PdfToTextOutputMode ByteString

-- | Runs 'pdftotext' utility.
--
-- https://en.wikipedia.org/wiki/Pdftotext
pdftotext ::
  (MonadError e m, MonadIO m, e ~ Text) =>
  PdfToTextMode ->
  -- | Input PDF path
  Turtle.FilePath ->
  -- | Output mode
  PdfToTextOutputMode outputType ->
  -- | The transcoded PDF
  m outputType
pdftotext mode pdfFile outputMode = do
  let modeArg = case mode of
        Raw -> "-raw"
        Layout -> "-layout"
  (exitCode, stdout', stderr') <-
    liftIO $
      Turtle.Bytes.procStrictWithErr
        "pdftotext"
        [modeArg, fpToText pdfFile, outputModeToArgument outputMode]
        mempty
  case exitCode of
    ExitFailure _ -> throwError $ "pdftotext has failed.\n" <> decodeUtf8 stderr'
    ExitSuccess -> return $ outputModeToReturnType outputMode stdout'
 where
  fpToText :: Turtle.FilePath -> Text
  fpToText = fromEither . Turtle.toText

  outputModeToArgument :: PdfToTextOutputMode o -> Text
  outputModeToArgument (PttOutputModeFilePath txtFile) = fpToText txtFile
  outputModeToArgument PttOutputModeStdOut = "-"

  outputModeToReturnType :: PdfToTextOutputMode outputType -> ByteString -> outputType
  outputModeToReturnType (PttOutputModeFilePath _) = const ()
  outputModeToReturnType PttOutputModeStdOut = id
