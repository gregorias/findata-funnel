-- | This module handles using pdf to text utilities.
module PdfToText (
  pdftotext,
  PdfToTextMode (..),
  PdfToTextInputMode (..),
  PdfToTextOutputMode (..),
) where

import Control.Exception.Extra (failIO)
import Control.Foldl.ByteString (ByteString)
import Control.Monad.IO.Class (MonadIO)
import Data.Either.Extra (fromEither)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Turtle (ExitCode (ExitFailure, ExitSuccess))
import qualified Turtle
import qualified Turtle.Bytes

data PdfToTextMode = Raw | Layout

data PdfToTextInputMode
  = PttInputModeFilePath Turtle.FilePath
  | PttInputModeStdIn (Turtle.Shell ByteString)

data PdfToTextOutputMode outputType where
  PttOutputModeFilePath :: Turtle.FilePath -> PdfToTextOutputMode ()
  PttOutputModeStdOut :: PdfToTextOutputMode Text

-- | Runs 'pdftotext' utility.
--
-- https://en.wikipedia.org/wiki/Pdftotext
pdftotext ::
  (MonadIO m) =>
  PdfToTextMode ->
  -- | Input mode
  PdfToTextInputMode ->
  -- | Output mode
  PdfToTextOutputMode outputType ->
  -- | The transcoded PDF
  m outputType
pdftotext mode inputMode outputMode = do
  let modeArg = case mode of
        Raw -> "-raw"
        Layout -> "-layout"
      (inputArg, stdin') = inputModeToInput inputMode
  (exitCode, stdout', stderr') <-
    Turtle.Bytes.procStrictWithErr
      "pdftotext"
      [modeArg, inputArg, outputModeToArgument outputMode]
      stdin'
  case exitCode of
    ExitFailure _ -> failIO $ "pdftotext has failed.\n" <> decodeUtf8 stderr'
    ExitSuccess -> return $ outputModeToReturnType outputMode stdout'
 where
  fpToText :: Turtle.FilePath -> Text
  fpToText = fromEither . Turtle.toText

  inputModeToInput :: PdfToTextInputMode -> (Text, Turtle.Shell ByteString)
  inputModeToInput (PttInputModeFilePath fp) = (fpToText fp, mempty)
  inputModeToInput (PttInputModeStdIn bs) = ("-", bs)

  outputModeToArgument :: PdfToTextOutputMode o -> Text
  outputModeToArgument (PttOutputModeFilePath txtFile) = fpToText txtFile
  outputModeToArgument PttOutputModeStdOut = "-"

  outputModeToReturnType :: PdfToTextOutputMode outputType -> ByteString -> outputType
  outputModeToReturnType (PttOutputModeFilePath _) = const ()
  outputModeToReturnType PttOutputModeStdOut = decodeUtf8
