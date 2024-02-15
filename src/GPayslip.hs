module GPayslip (
  pullGooglePayslips,
  moveGPayslipToWallet,
) where

import Control.Foldl qualified as Foldl
import Control.Monad.Except (MonadError (catchError, throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed qualified as Managed
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import FindataTranscoder
import PdfToText (PdfToTextInputMode (..), PdfToTextMode (..), PdfToTextOutputMode (PttOutputModeFilePath, PttOutputModeStdOut), pdftotext)
import Turtle (Line, Shell, home, ls, (</>))
import Turtle qualified
import Turtle.Extra (emptyLine)
import Turtle.Extra qualified as Turtle
import Wallet (appendTransactionToWallet, getWallet)

-- | Moves Google Payslip PDF to the main wallet file.
moveGPayslipToWallet ::
  (MonadError e m, MonadIO m, e ~ Text) =>
  -- | The ledger text file.
  Turtle.FilePath ->
  -- | The Google payslip PDF.
  Turtle.FilePath ->
  m ()
moveGPayslipToWallet wallet pdf = flip catchError prependContext $ do
  gpayslipTxt :: Text <- transcode pdf
  let gpayslipContent :: Shell Line = Turtle.select $ Turtle.textToLines gpayslipTxt
  ledgerTransaction :: Text <- findataTranscoder FindataTranscoderGPayslip gpayslipContent
  Turtle.append wallet (Turtle.select $ emptyLine <> Turtle.textToLines ledgerTransaction)
  Turtle.rm pdf
 where
  prependContext errMsg = do
    let pdfPath = T.pack pdf
    throwError $
      "Could not move Google Payslip (" <> pdfPath <> ") to the wallet file.\n" <> errMsg

  unsafeRunManaged :: Managed.Managed a -> IO a
  unsafeRunManaged = flip Managed.with return

  transcode :: (MonadError e m, MonadIO m, e ~ Text) => Turtle.FilePath -> m Text
  transcode pdf' = do
    maybeContent <- liftIO . unsafeRunManaged $ do
      tmpTxt <- Turtle.mktempfile "/tmp" ""
      maybeSuccess :: (Either Text ()) <-
        runExceptT $
          pdftotext Layout (PttInputModeFilePath pdf') (PttOutputModeFilePath tmpTxt)
      liftIO $
        sequence $
          T.readFile tmpTxt <$ maybeSuccess
    either throwError return maybeContent

-- | Pulls Google Payslips to the wallet.
--
-- Throws an IO exception on failure.
pullGooglePayslips :: (MonadIO io) => io ()
pullGooglePayslips = do
  homeDir <- home
  let payslipSourceDir = homeDir </> T.unpack "Google Drive/My Drive/Payslips"
  let payslipTargetDir = homeDir </> T.unpack "Documents/Job/Google/Payslips"
  Turtle.reduce Foldl.mconcat $ do
    payslipSource <- ls payslipSourceDir
    let payslipTarget = payslipTargetDir </> Turtle.filename payslipSource
    Turtle.mv payslipSource payslipTarget
    payslipTxt <-
      Turtle.select . Turtle.textToLines
        <$> pdftotext Layout (PttInputModeFilePath payslipTarget) PttOutputModeStdOut
    transaction <- findataTranscoder FindataTranscoderGPayslip payslipTxt
    wallet <- getWallet
    appendTransactionToWallet wallet (Turtle.select $ Turtle.textToPosixLines transaction)
