module Coop (
  pullCoopSupercardReceipts,
  HeadlessMode (..),
  VerboseMode (..),
  FetchConfig (..),
  defaultConfig,
) where

import Control.Exception.Base
import Control.Foldl qualified as Foldl
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bool (bool)
import Data.Either.Combinators (leftToMaybe)
import FindataFetcher qualified as FF
import PdfToText (
  PdfToTextInputMode (PttInputModeFilePath),
  PdfToTextMode (..),
  PdfToTextOutputMode (PttOutputModeFilePath),
  pdftotext,
 )
import System.FilePath.Glob (compile, match)
import Turtle (
  cd,
  ls,
  rm,
  (<.>),
  (</>),
 )
import Turtle qualified
import Wallet (getWalletDir)

data HeadlessMode = Headless | NoHeadless

data VerboseMode = Verbose | Quiet

data FetchConfig = FetchConfig
  { fetchConfigHeadlessMode :: HeadlessMode
  , fetchConfigVerboseMode :: VerboseMode
  }

defaultConfig :: FetchConfig
defaultConfig = FetchConfig Headless Quiet

textifyPdf ::
  (MonadIO m) =>
  -- | The target path in the wallet dir.
  Turtle.FilePath ->
  -- | The path to the PDF.
  Turtle.FilePath ->
  m ()
textifyPdf subdir pdf = do
  walletDir <- getWalletDir
  let txtFile = walletDir </> subdir </> (pdf <.> "txt")
  pdftotext Raw (PttInputModeFilePath pdf) (PttOutputModeFilePath txtFile)

-- | Pulls Coop Supercard receipts.
--
-- Throws an IO exception on failure.
pullCoopSupercardReceipts :: (MonadIO m) => FF.CoopSupercardParameters -> m ()
pullCoopSupercardReceipts params = do
  -- The Coop fetcher may fail due to occasional CAPTCHA checks, so let's not
  -- block textification if that happens.
  ioException :: (Maybe IOException) <-
    liftIO $ fmap leftToMaybe . try @IOException $ FF.run (FF.SourceCoopSupercard params)
  textifyCoopPdfReceipts
  maybe (return ()) (liftIO . throwIO) ioException
 where
  downloads :: (MonadIO io) => io Turtle.FilePath
  downloads = do
    homeDir <- Turtle.home
    return $ homeDir </> "Downloads"

  cdDownloads :: (MonadIO io) => io ()
  cdDownloads = downloads >>= cd

  textifyCoopPdfReceipts :: (MonadIO m) => m ()
  textifyCoopPdfReceipts = Turtle.reduce Foldl.mconcat $ do
    cdDownloads
    file <- ls "."
    pdf <- bool Turtle.empty (return file) (match (compile "Coop *.pdf") file)
    textifyPdf "updates/coop-receipts" pdf
    rm pdf
