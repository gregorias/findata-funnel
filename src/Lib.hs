{-# LANGUAGE PartialTypeSignatures #-}

module Lib (
  main,
  -- | Exports of individual handlers for testing purposes.
  moveGPayslipToWallet,
) where

import Control.Concurrent.ParallelIO.Global (parallel)
import Control.Exception (tryJust)
import qualified Control.Foldl as Foldl
import Control.Monad (void, when)
import Control.Monad.Except (ExceptT, MonadError (catchError, throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Managed (MonadManaged)
import qualified Control.Monad.Managed as Managed
import Data.Bool (bool)
import Data.Either.Extra (fromEither)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (hPutStr)
import qualified Data.Text.IO as T
import qualified FindataFetcher as FF
import FindataTranscoder (
  FindataTranscoderSource (..),
  findataTranscoder,
 )
import PdfToText (PdfToTextMode (..), pdftotext)
import System.FilePath.Glob (compile, match)
import System.IO (stderr)
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
    (\e -> liftIO $ hPutStr stderr (name <> " has failed.\n" <> e) >> return (ExitFailure 1))
    (const $ return ExitSuccess)
    eitherErrorOrValue

reportExceptions :: (MonadIO io) => Text -> IO () -> io ExitCode
reportExceptions name action = do
  (result :: Either _ _) <- liftIO $ tryJust (\e -> if isUserError e then Just e else Nothing) action
  either
    (\e -> liftIO $ hPutStr stderr (name <> " has failed.\n" <> T.pack (show e)) >> return (ExitFailure 1))
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

textifyAndMovePdf ::
  (MonadError e m, MonadIO m, e ~ Text) =>
  -- | The target path in the wallet dir.
  Turtle.FilePath ->
  -- | The path to the PDF.
  Turtle.FilePath ->
  m ()
textifyAndMovePdf subdir pdf = do
  walletDir <- getWalletDir
  let txtFile = walletDir </> subdir </> (pdf <.> "txt")
  pdftotext Raw pdf txtFile
  rm pdf

textifyAndMoveBcgeCcPdfStatement :: Shell ExitCode
textifyAndMoveBcgeCcPdfStatement = do
  cdDownloads
  file <- ls $ Turtle.fromText "."
  bool
    (return ExitSuccess)
    (reportErrors ("Textifying " <> fpToText file) $ textifyAndMovePdf "updates" file)
    (match (compile "bcgecc.pdf") (Turtle.encodeString file))

parseAndMoveBcgeCcPdfStatement :: Shell ExitCode
parseAndMoveBcgeCcPdfStatement = do
  walletDir <- getWalletDir
  let targetDir = walletDir </> "updates"
  cd targetDir
  file <- ls $ Turtle.fromText "."
  bool
    (return ExitSuccess)
    (reportErrors ("Parsing " <> fpToText file) $ void (parseAndMoveStatement FindataTranscoderBcgeCc file))
    (match (compile "bcgecc*.txt") (Turtle.encodeString file))

textifyAndMoveCoopPdfReceipt ::
  (MonadError e m, MonadIO m, e ~ Text) =>
  -- | The path to the PDF receipt.
  Turtle.FilePath ->
  m ()
textifyAndMoveCoopPdfReceipt = textifyAndMovePdf "updates/coop-receipts"

parseAndMoveCoopPdfReceipts :: Shell ExitCode
parseAndMoveCoopPdfReceipts = do
  cdDownloads
  file <- ls $ Turtle.fromText "."
  bool
    (return ExitSuccess)
    (reportErrors ("Textifying " <> fpToText file) $ textifyAndMoveCoopPdfReceipt file)
    (match (compile "Coop *.pdf") (Turtle.encodeString file))

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

parseAndAppendDegiroPortfolioStatement :: Shell ExitCode
parseAndAppendDegiroPortfolioStatement = do
  cdDownloads
  file <- ls $ Turtle.fromText "."
  bool
    (return ExitSuccess)
    ( reportErrors ("Parsing " <> fpToText file) $
        void (parseAndAppendStatement FindataTranscoderDegiroPortfolio file >> rm file)
    )
    (match (compile "degiro-portfolio.csv") (Turtle.encodeString file))

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
      maybeSuccess :: (Either Text ()) <- runExceptT $ pdftotext Layout pdf' tmpTxt
      liftIO $
        sequence $
          T.readFile (Turtle.encodeString tmpTxt) <$ maybeSuccess
    either throwError return maybeContent

movePatreonReceipt :: (MonadManaged m) => Turtle.FilePath -> m ()
movePatreonReceipt stmt = void $ parseAndAppendStatement FindataTranscoderPatreon stmt

movePatreonReceipts :: Shell ExitCode
movePatreonReceipts = do
  cdDownloads
  file <- ls $ Turtle.fromText "."
  bool
    (return ExitSuccess)
    (reportErrors ("Parsing " <> fpToText file) $ void (movePatreonReceipt file >> rm file))
    (match (compile "patreon_*.txt") (Turtle.encodeString file))

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

moveGalaxusReceipts :: Shell ExitCode
moveGalaxusReceipts = do
  cdDownloads
  file <- ls $ Turtle.fromText "."
  bool
    (return ExitSuccess)
    (reportErrors ("Moving " <> fpToText file) $ void (moveGalaxusBill file >> rm file))
    (match (compile "*.galaxus") (Turtle.encodeString file))
 where
  moveGalaxusBill :: (MonadManaged m) => Turtle.FilePath -> m ()
  moveGalaxusBill = parseAndAppendStatement FindataTranscoderGalaxus

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

main :: IO ()
main = do
  fetchingExitCodes :: [ExitCode] <-
    parallel $
      fmap
        ( \(sourceName, ffSource) ->
            reportExceptions ("Fetching " <> sourceName) $
              void (FF.runFindataFetcher ffSource)
        )
        [ ("Coop receipts", FF.FFSourceCoopSupercard)
        , ("EasyRide receipts", FF.FFSourceEasyRide)
        , ("Galaxus receipts", FF.FFSourceGalaxus)
        , ("Patreon receipts", FF.FFSourcePatreon)
        , ("Revolut statements", FF.FFSourceRevolutMail)
        , ("Uber Eats bills", FF.FFSourceUberEats)
        ]
  anyBcgeCcTextifyAndMovePdfStatementFailure <-
    Turtle.fold
      textifyAndMoveBcgeCcPdfStatement
      (Foldl.any isExitFailure)
  anyBcgeCcParseAndMovePdfStatementFailure <-
    Turtle.fold
      parseAndMoveBcgeCcPdfStatement
      (Foldl.any isExitFailure)
  anyCoopParseAndMoveFailure <- Turtle.fold parseAndMoveCoopPdfReceipts (Foldl.any isExitFailure)
  anyDegiroPortfolioParseAndAppendFailure <-
    Turtle.fold
      parseAndAppendDegiroPortfolioStatement
      (Foldl.any isExitFailure)
  anyGalaxusFailure <- Turtle.fold moveGalaxusReceipts (Foldl.any isExitFailure)
  wallet <- getWallet
  anyGPayslipFailure <-
    Turtle.fold
      (forFileInDls "gpayslip*.pdf" (moveGPayslipToWallet wallet))
      (Foldl.any isExitFailure)
  anyPatreonParseAndMoveFailure <- Turtle.fold movePatreonReceipts (Foldl.any isExitFailure)
  anyRevolutParseAndMoveFailure <- Turtle.fold parseAndMoveRevolutCsvStatements (Foldl.any isExitFailure)
  anyUberEatsFailure <- Turtle.fold moveUberEatsBills (Foldl.any isExitFailure)
  when
    ( any isExitFailure fetchingExitCodes
        || anyBcgeCcTextifyAndMovePdfStatementFailure
        || anyBcgeCcParseAndMovePdfStatementFailure
        || anyCoopParseAndMoveFailure
        || anyDegiroPortfolioParseAndAppendFailure
        || anyGalaxusFailure
        || anyGPayslipFailure
        || anyPatreonParseAndMoveFailure
        || anyRevolutParseAndMoveFailure
        || anyUberEatsFailure
    )
    (exit (ExitFailure 1))
 where
  isExitFailure ExitSuccess = False
  isExitFailure (ExitFailure _) = True
