{-# LANGUAGE PartialTypeSignatures #-}

-- | This module contains fully-automatic dataflows.
--
-- A fully-automatic dataflow is one that is triggerred by an external event
-- and can be completed without my involvement. Usually these are tasks that
-- don't require my input to solve a 2FA challenge.
module Auto (
  pullAuto,
  -- | Exports of individual handlers for testing purposes.
  moveGPayslipToWallet,
) where

import Control.Concurrent.ParallelIO.Global (parallel)
import Control.Exception (IOException, catch, throwIO, try)
import qualified Control.Foldl as Foldl
import Control.Monad (when)
import Control.Monad.Except (MonadError (catchError, throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Monad.Managed as Managed
import Data.Bool (bool)
import Data.Either (isRight)
import Data.Either.Combinators (leftToMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified FindataFetcher as FF
import FindataTranscoder (
  FindataTranscoderSource (..),
  findataTranscoder,
 )
import PdfToText (
  PdfToTextInputMode (PttInputModeFilePath),
  PdfToTextMode (..),
  PdfToTextOutputMode (PttOutputModeFilePath, PttOutputModeStdOut),
  pdftotext,
 )
import System.FilePath.Glob (compile, match)
import System.IO (hPutStr, hPutStrLn, stderr)
import Turtle (
  ExitCode (ExitFailure),
  Line,
  Shell,
  cd,
  exit,
  home,
  ls,
  rm,
  (<.>),
  (</>),
 )
import qualified Turtle
import Turtle.Extra (decodePathM, emptyLine)
import qualified Turtle.Extra as Turtle
import Wallet (appendTransactionToWallet, getWallet, getWalletDir)

downloads :: (MonadIO io) => io Turtle.FilePath
downloads = do
  homeDir <- home
  return $ homeDir </> Turtle.fromText "Downloads"

cdDownloads :: (MonadIO io) => io ()
cdDownloads = downloads >>= cd

printIOExceptionOnStderr :: String -> IO a -> IO a
printIOExceptionOnStderr name action =
  catch
    action
    ( \(ioe :: IOException) -> do
        hPutStrLn stderr $ "Could not execute " <> name
        hPutStr stderr (show ioe)
        throwIO ioe
    )

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

-- | Parses all text statements in a directory, appends them to a wallet, and deletes the source.
parseTextStatements ::
  -- | Source directory
  Turtle.FilePath ->
  -- | Statement filename glob pattern
  String ->
  FindataTranscoderSource ->
  IO ()
parseTextStatements sourceDir stmtGlobPattern ftSource = Turtle.reduce Foldl.mconcat $ do
  file <- ls sourceDir
  stmt <- bool Turtle.empty (return file) (match (compile stmtGlobPattern) (Turtle.encodeString $ Turtle.filename file))
  transaction :: Text <- findataTranscoder ftSource (Turtle.input stmt)
  wallet <- getWallet
  appendTransactionToWallet wallet (Turtle.select $ Turtle.textToPosixLines transaction)
  rm stmt

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
  rm pdf
 where
  prependContext errMsg = do
    pdfPath <- decodePathM pdf
    throwError $
      "Could not move Google Payslip (" <> pdfPath <> ") to the wallet file.\n" <> errMsg

  unsafeRunManaged :: Managed.Managed a -> IO a
  unsafeRunManaged = flip Managed.with return

  transcode :: (MonadError e m, MonadIO m, e ~ Text) => Turtle.FilePath -> m Text
  transcode pdf' = do
    maybeContent <- liftIO . unsafeRunManaged $ do
      tmpTxt <- Turtle.mktempfile (Turtle.decodeString "/tmp") ""
      maybeSuccess :: (Either Text ()) <-
        runExceptT $
          pdftotext Layout (PttInputModeFilePath pdf') (PttOutputModeFilePath tmpTxt)
      liftIO $
        sequence $
          T.readFile (Turtle.encodeString tmpTxt) <$ maybeSuccess
    either throwError return maybeContent

-- | Pulls coop receipts to the wallet.
--
-- Throws an IO exception on failure.
pullCoopReceipts :: IO ()
pullCoopReceipts = do
  -- The coop fetcher often fails, so let's not block textification if that happens.
  ioException :: (Maybe IOException) <-
    fmap leftToMaybe . try @IOException $ FF.runFindataFetcher FF.FFSourceCoopSupercard
  textifyCoopPdfReceipts
  maybe (return ()) throwIO ioException
 where
  textifyCoopPdfReceipts :: IO ()
  textifyCoopPdfReceipts = Turtle.reduce Foldl.mconcat $ do
    cdDownloads
    file <- ls $ Turtle.fromText "."
    pdf <- bool Turtle.empty (return file) (match (compile "Coop *.pdf") (Turtle.encodeString file))
    textifyPdf "updates/coop-receipts" pdf
    rm pdf

-- | Pulls EasyRide receipts to the wallet.
--
-- Throws an IO exception on failure.
pullEasyRideReceipts :: IO ()
pullEasyRideReceipts = do
  FF.runFindataFetcher FF.FFSourceEasyRide

-- | Pulls Galaxus receipts to the wallet.
--
-- Throws an IO exception on failure.
pullGalaxusReceipts :: IO ()
pullGalaxusReceipts = do
  FF.runFindataFetcher FF.FFSourceGalaxus
  downloadsDir :: Turtle.FilePath <- downloads
  parseTextStatements downloadsDir "*.galaxus" FindataTranscoderGalaxus

-- | Pulls Google Payslips to the wallet.
--
-- Throws an IO exception on failure.
pullGooglePayslips :: IO ()
pullGooglePayslips = do
  homeDir <- home
  let payslipSourceDir = homeDir </> Turtle.fromText "Google Drive/My Drive/Payslips"
  let payslipTargetDir = homeDir </> Turtle.fromText "Documents/Job/Google/Payslips"
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

-- | Pulls Patreon receipts to the wallet.
--
-- Throws an IO exception on failure.
pullPatreonReceipts :: IO ()
pullPatreonReceipts = do
  FF.runFindataFetcher FF.FFSourcePatreon
  downloadsDir :: Turtle.FilePath <- downloads
  parseTextStatements downloadsDir "patreon_*.txt" FindataTranscoderPatreon

-- | Pulls Revolut receipts to the wallet.
--
-- Throws an IO exception on failure.
pullRevolutReceipts :: IO ()
pullRevolutReceipts = do
  FF.runFindataFetcher FF.FFSourceRevolutMail
  downloadsDir :: Turtle.FilePath <- downloads
  parseTextStatements downloadsDir "revolut-account-statement*.csv" FindataTranscoderRevolut

-- | Pulls Uber Eats receipts to the wallet.
--
-- Throws an IO exception on failure.
pullUberEatsReceipts :: IO ()
pullUberEatsReceipts = do
  FF.runFindataFetcher FF.FFSourceUberEats
  downloadsDir :: Turtle.FilePath <- downloads
  parseTextStatements downloadsDir "*.ubereats" FindataTranscoderUberEats

-- | Pulls data fully automatically.
pullAuto :: IO ()
pullAuto = do
  pullSuccesses :: [Bool] <-
    parallel $
      isIOSuccessful . uncurry printIOExceptionOnStderr
        <$> [ ("Coop pull", pullCoopReceipts)
            , ("EasyRide pull", pullEasyRideReceipts)
            , ("Galaxus pull", pullGalaxusReceipts)
            , ("Google payslip pull", pullGooglePayslips)
            , ("Patreon pull", pullPatreonReceipts)
            , ("Revolut pull", pullRevolutReceipts)
            , ("Uber Eats pull", pullUberEatsReceipts)
            ]
  when (False `elem` pullSuccesses) (exit (ExitFailure 1))
 where
  isIOSuccessful :: IO () -> IO Bool
  isIOSuccessful = fmap isRight . try @IOException
