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
import Control.Exception (IOException, catch, throwIO, try, tryJust)
import qualified Control.Foldl as Foldl
import Control.Monad (void, when)
import Control.Monad.Except (ExceptT, MonadError (catchError, throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Managed (MonadManaged)
import qualified Control.Monad.Managed as Managed
import Data.Bool (bool)
import Data.Either (isRight)
import Data.Either.Combinators (leftToMaybe)
import Data.Either.Extra (fromEither)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified FindataFetcher as FF
import FindataTranscoder (
  FindataTranscoderSource (..),
  findataTranscoder,
 )
import PdfToText (
  PdfToTextInputMode (PttInputModeFilePath),
  PdfToTextMode (..),
  PdfToTextOutputMode (PttOutputModeFilePath),
  pdftotext,
 )
import System.FilePath.Glob (compile, match)
import System.IO (hPutStr, hPutStrLn, stderr)
import System.IO.Error (isUserError)
import Turtle (
  ExitCode (ExitFailure, ExitSuccess),
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

fpToText :: Turtle.FilePath -> Text
fpToText = fromEither . Turtle.toText

cdDownloads :: (MonadIO io) => io ()
cdDownloads = do
  homeDir <- home
  cd $ homeDir </> Turtle.fromText "Downloads"

reportErrors :: (MonadIO io) => Text -> ExceptT Text io () -> io ExitCode
reportErrors name action = do
  eitherErrorOrValue <- runExceptT action
  either
    (\e -> liftIO $ T.hPutStr stderr (name <> " has failed.\n" <> e) >> return (ExitFailure 1))
    (const $ return ExitSuccess)
    eitherErrorOrValue

reportExceptions :: (MonadIO io) => Text -> IO () -> io ExitCode
reportExceptions name action = do
  (result :: Either _ _) <- liftIO $ tryJust (\e -> if isUserError e then Just e else Nothing) action
  either
    (\e -> liftIO $ T.hPutStr stderr (name <> " has failed.\n" <> T.pack (show e)) >> return (ExitFailure 1))
    (const $ return ExitSuccess)
    result

-- | Runs the action for each matching file in '~/Downloads'.
forFileInDls :: String -> (Turtle.FilePath -> ExceptT Text Shell ()) -> Shell ExitCode
forFileInDls globStr action = do
  cdDownloads
  file :: Turtle.FilePath <- ls $ Turtle.fromText "."
  let glob = compile globStr
  if match glob (Turtle.encodeString file)
    then reportErrors ("Processing " <> fpToText file) $ action file
    else return ExitSuccess

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

parseAndMoveStatement ::
  (MonadIO m) =>
  FindataTranscoderSource ->
  Turtle.FilePath ->
  m Turtle.FilePath
parseAndMoveStatement findataTranscoderSource stmt = do
  walletDir <- getWalletDir
  ledgerOutput <- findataTranscoder findataTranscoderSource (Turtle.input stmt)
  let stmtLedger = walletDir </> Turtle.fromText "updates" </> (stmt <.> "ledger")
  Turtle.output stmtLedger (Turtle.select $ Turtle.textToLines ledgerOutput)
  rm stmt
  return stmtLedger

parseAndAppendStatement :: (MonadManaged m) => FindataTranscoderSource -> Turtle.FilePath -> m ()
parseAndAppendStatement findataTranscoderSource stmt = do
  stmtTxt <- findataTranscoder findataTranscoderSource (Turtle.input stmt)
  wallet <- getWallet
  appendTransactionToWallet wallet (Turtle.select $ Turtle.textToPosixLines stmtTxt)

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

parseAndMoveRevolutCsvStatement :: (MonadIO m) => Turtle.FilePath -> m ()
parseAndMoveRevolutCsvStatement stmt = void $ parseAndMoveStatement FindataTranscoderRevolut stmt

parseAndMoveRevolutCsvStatements :: Shell ExitCode
parseAndMoveRevolutCsvStatements = do
  cdDownloads
  file <- ls $ Turtle.fromText "."
  bool
    (return ExitSuccess)
    (reportErrors ("Parsing " <> fpToText file) $ parseAndMoveRevolutCsvStatement file)
    (match (compile "revolut-account-statement*.csv") (Turtle.encodeString file))

moveUberEatsBill :: (MonadManaged m) => Turtle.FilePath -> m ()
moveUberEatsBill = parseAndAppendStatement FindataTranscoderUberEats

moveUberEatsBills :: Shell ExitCode
moveUberEatsBills = do
  cdDownloads
  file <- ls $ Turtle.fromText "."
  bool
    (return ExitSuccess)
    (reportErrors ("Moving " <> fpToText file) $ void (moveUberEatsBill file >> rm file))
    (match (compile "*.ubereats") (Turtle.encodeString file))

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
  moveGalaxusReceipts
 where
  moveGalaxusReceipts :: IO ()
  moveGalaxusReceipts = Turtle.reduce Foldl.mconcat $ do
    cdDownloads
    file <- ls $ Turtle.fromText "."
    receipt <- bool Turtle.empty (return file) (match (compile "*.galaxus") (Turtle.encodeString file))
    transaction :: Text <- findataTranscoder FindataTranscoderGalaxus (Turtle.input receipt)
    wallet <- getWallet
    appendTransactionToWallet wallet (Turtle.select $ Turtle.textToPosixLines transaction)
    rm receipt

-- | Pulls Patreon receipts to the wallet.
--
-- Throws an IO exception on failure.
pullPatreonReceipts :: IO ()
pullPatreonReceipts = do
  FF.runFindataFetcher FF.FFSourcePatreon
  movePatreonReceipts
 where
  movePatreonReceipts :: IO ()
  movePatreonReceipts = Turtle.reduce Foldl.mconcat $ do
    cdDownloads
    file <- ls $ Turtle.fromText "."
    receipt <- bool Turtle.empty (return file) (match (compile "patreon_*.txt") (Turtle.encodeString file))
    transaction :: Text <- findataTranscoder FindataTranscoderPatreon (Turtle.input receipt)
    wallet <- getWallet
    appendTransactionToWallet wallet (Turtle.select $ Turtle.textToPosixLines transaction)
    rm receipt

-- | Pulls data fully automatically.
pullAuto :: IO ()
pullAuto = do
  pullSuccesses :: [Bool] <-
    parallel $
      isIOSuccessful . uncurry printIOExceptionOnStderr
        <$> [ ("Coop pull", pullCoopReceipts)
            , ("EasyRide pull", pullEasyRideReceipts)
            , ("Galaxus pull", pullGalaxusReceipts)
            , ("Patreon pull", pullPatreonReceipts)
            ]
  fetchingExitCodes :: [ExitCode] <-
    parallel $
      fmap
        ( \(sourceName, ffSource) ->
            reportExceptions ("Fetching " <> sourceName) $
              FF.runFindataFetcher ffSource
        )
        [ ("Revolut statements", FF.FFSourceRevolutMail)
        , ("Uber Eats bills", FF.FFSourceUberEats)
        ]
  wallet <- getWallet
  anyGPayslipFailure <-
    Turtle.fold
      (forFileInDls "gpayslip*.pdf" (moveGPayslipToWallet wallet))
      (Foldl.any isExitFailure)
  anyRevolutParseAndMoveFailure <- Turtle.fold parseAndMoveRevolutCsvStatements (Foldl.any isExitFailure)
  anyUberEatsFailure <- Turtle.fold moveUberEatsBills (Foldl.any isExitFailure)
  when
    ( any isExitFailure fetchingExitCodes
        || anyGPayslipFailure
        || anyRevolutParseAndMoveFailure
        || anyUberEatsFailure
        || elem False pullSuccesses
    )
    (exit (ExitFailure 1))
 where
  isExitFailure ExitSuccess = False
  isExitFailure (ExitFailure _) = True
  isIOSuccessful :: IO () -> IO Bool
  isIOSuccessful = fmap isRight . try @IOException
