module Coop (
  pullCoopSupercardReceipts,
  parseReceiptPdfsToWallet,
) where

import Control.Concurrent.Extra (threadDelay)
import Control.Foldl qualified as Foldl
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bool (bool)
import Data.Time.Clock (secondsToDiffTime)
import Findata (ledgersort)
import FindataFetcher qualified as FF
import FindataTranscoder qualified as FT
import PdfToText (
  PdfToTextInputMode (PttInputModeFilePath),
  PdfToTextMode (..),
  PdfToTextOutputMode (PttOutputModeStdOut),
  pdftotext,
 )
import Relude (Text)
import System.FilePath.Glob (compile, match)
import Turtle (
  cd,
  ls,
  rm,
  (</>),
 )
import Turtle qualified
import Turtle.Extra qualified as Turtle
import Wallet (appendTransactionToWallet, getWallet)

-- | Pulls Coop Supercard receipts.
--
-- Throws an IO exception on failure.
pullCoopSupercardReceipts :: (MonadIO m) => m ()
pullCoopSupercardReceipts = do
  FF.run FF.SourceCoopSupercard
  -- Let me click on all the download links before proceeding with processing.
  liftIO $ threadDelay (secondsToDiffTime 25)
  parseReceiptPdfsToWallet

parseReceiptPdfsToWallet ::
  (MonadIO m) =>
  m ()
parseReceiptPdfsToWallet = do
  wallet <- getWallet
  textifyCoopPdfReceipts wallet
  ledgersort
 where
  textifyCoopPdfReceipts :: (MonadIO m) => Turtle.FilePath -> m ()
  textifyCoopPdfReceipts wallet = Turtle.reduce Foldl.mconcat $ do
    cdDownloads
    file <- ls "."
    pdf <- bool Turtle.empty (return file) (match (compile "receipt_*.pdf") file)
    receiptText <- textifyPdf pdf
    (ledgerTransaction :: Text) <- transcodeReceiptText receiptText
    appendTransactionToWallet wallet (Turtle.textToShell ledgerTransaction)
    rm pdf

  cdDownloads :: (MonadIO io) => io ()
  cdDownloads = downloads >>= cd

  downloads :: (MonadIO io) => io Turtle.FilePath
  downloads = do
    homeDir <- Turtle.home
    return $ homeDir </> "Downloads"

-- | Textifies a Coop receipt PDF.
textifyPdf ::
  (MonadIO m) =>
  -- | The path to the PDF.
  Turtle.FilePath ->
  m Text
textifyPdf pdf = do
  pdftotext Raw (PttInputModeFilePath pdf) PttOutputModeStdOut

-- | Transcodes receipt text into a Ledger transaction.
transcodeReceiptText :: (MonadIO m) => Text -> m Text
transcodeReceiptText receiptText = do
  FT.findataTranscoder FT.FindataTranscoderCoop $
    Turtle.posixLineToLine <$> Turtle.textToShell receiptText
