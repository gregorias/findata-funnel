module Lib (
  main,
) where

import Control.Concurrent.ParallelIO.Global (parallel)
import Control.Exception (try)
import qualified Control.Foldl as Foldl
import Control.Monad.Except (MonadError (throwError))
import Data.Text.IO (hPutStr)
import qualified FindataFetcher as FF
import Hledupt (
  HleduptSource (..),
  hledupt,
 )
import Relude
import System.FilePath.Glob (compile, match)
import Turtle (
  ExitCode (ExitFailure, ExitSuccess),
  Shell,
  cd,
  exit,
  home,
  inproc,
  ls,
  rm,
  sh,
  (<.>),
  (</>),
 )
import qualified Turtle

fromEither :: Either a a -> a
fromEither = either id id

fpToText :: Turtle.FilePath -> Text
fpToText = fromEither . Turtle.toText

getWalletDir :: (MonadIO m) => m Turtle.FilePath
getWalletDir = do
  homeDir <- home
  return $ homeDir </> Turtle.fromText "Documents/Finance/Wallet"

reportErrors :: (MonadIO io) => Text -> ExceptT Text io () -> io ExitCode
reportErrors name action = do
  eitherErrorOrValue <- runExceptT action
  either
    (\e -> liftIO $ hPutStr stderr (name <> " has failed.\n" <> e) >> return (ExitFailure 1))
    (const $ return ExitSuccess)
    eitherErrorOrValue

-- | Runs pdftotext utility.
--
-- https://en.wikipedia.org/wiki/Pdftotext
pdftotext :: (MonadError e m, MonadIO m, e ~ Text) => Turtle.FilePath -> Turtle.FilePath -> m ()
pdftotext pdfFile txtFile = do
  eitherErrorOrUnit <-
    liftIO $
      try @SomeException . sh $
        void $ inproc "pdftotext" ["-raw", fpToText pdfFile, fpToText txtFile] mempty
  whenLeft () eitherErrorOrUnit (\e -> throwError $ "The pdftotext has failed: " <> show e)

cdDownloads :: (MonadIO io) => io ()
cdDownloads = do
  homeDir <- home
  cd $ homeDir </> Turtle.fromText "Downloads"

parseAndMoveCoopPdfReceipt :: (MonadError e m, MonadIO m, e ~ Text) => Turtle.FilePath -> m ()
parseAndMoveCoopPdfReceipt receiptPdf = do
  walletDir <- getWalletDir
  let receiptTxt =
        walletDir
          </> Turtle.fromText "updates/coop-receipts"
          </> (receiptPdf <.> "txt")
  pdftotext receiptPdf receiptTxt
  rm receiptPdf

parseAndMoveCoopPdfReceipts :: Shell ExitCode
parseAndMoveCoopPdfReceipts = do
  cdDownloads
  file <- ls $ Turtle.fromText "."
  bool
    (return ExitSuccess)
    (reportErrors ("Parsing " <> fpToText file) $ parseAndMoveCoopPdfReceipt file)
    (match (compile "Coop *.pdf") (Turtle.encodeString file))

parseAndMoveStatement ::
  (MonadError e m, MonadIO m, e ~ Text) =>
  HleduptSource ->
  Turtle.FilePath ->
  m Turtle.FilePath
parseAndMoveStatement hleduptSource stmt = do
  walletDir <- getWalletDir
  let stmtLedger = walletDir </> Turtle.fromText "updates" </> (stmt <.> "ledger")
  hledupt hleduptSource stmt stmtLedger
  rm stmt
  return stmtLedger

parseAndMoveDegiroPortfolioStatement :: Shell ExitCode
parseAndMoveDegiroPortfolioStatement = do
  cdDownloads
  file <- ls $ Turtle.fromText "."
  bool
    (return ExitSuccess)
    (reportErrors ("Parsing " <> fpToText file) $ void (parseAndMoveStatement HleduptDegiroPortfolio file))
    (match (compile "degiro-account.csv") (Turtle.encodeString file))

parseAndMovePatreonReceipt :: (MonadError e m, MonadIO m, e ~ Text) => Turtle.FilePath -> m ()
parseAndMovePatreonReceipt stmt = void $ parseAndMoveStatement HleduptPatreon stmt

parseAndMovePatreonReceipts :: Shell ExitCode
parseAndMovePatreonReceipts = do
  cdDownloads
  file <- ls $ Turtle.fromText "."
  bool
    (return ExitSuccess)
    (reportErrors ("Parsing " <> fpToText file) $ parseAndMovePatreonReceipt file)
    (match (compile "patreon_*.txt") (Turtle.encodeString file))

parseAndMoveRevolutCsvStatement :: (MonadError e m, MonadIO m, e ~ Text) => Turtle.FilePath -> m ()
parseAndMoveRevolutCsvStatement stmt = void $ parseAndMoveStatement HleduptRevolut stmt

parseAndMoveRevolutCsvStatements :: Shell ExitCode
parseAndMoveRevolutCsvStatements = do
  cdDownloads
  file <- ls $ Turtle.fromText "."
  bool
    (return ExitSuccess)
    (reportErrors ("Parsing " <> fpToText file) $ parseAndMoveRevolutCsvStatement file)
    (match (compile "revolut-account-statement*.csv") (Turtle.encodeString file))

main :: IO ()
main = do
  fetchingExitCodes :: [ExitCode] <-
    parallel $
      fmap
        (\(sourceName, ffSource) -> reportErrors ("Fetching " <> sourceName) $ FF.runFindataFetcher ffSource)
        [ ("Coop receipts", FF.FFSourceCoop)
        , ("EasyRide receipts", FF.FFSourceEasyRide)
        , ("Patreon receipts", FF.FFSourcePatreon)
        , ("Revolut statements", FF.FFSourceRevolutMail)
        ]
  anyCoopParseAndMoveFailure <- Turtle.fold parseAndMoveCoopPdfReceipts (Foldl.any isExitFailure)
  anyDegiroPortfolioParseAndMoveFailure <- Turtle.fold parseAndMoveDegiroPortfolioStatement (Foldl.any isExitFailure)
  anyPatreonParseAndMoveFailure <- Turtle.fold parseAndMovePatreonReceipts (Foldl.any isExitFailure)
  anyRevolutParseAndMoveFailure <- Turtle.fold parseAndMoveRevolutCsvStatements (Foldl.any isExitFailure)
  when
    ( any isExitFailure fetchingExitCodes || anyCoopParseAndMoveFailure
        || anyDegiroPortfolioParseAndMoveFailure
        || anyPatreonParseAndMoveFailure
        || anyRevolutParseAndMoveFailure
    )
    (exit (ExitFailure 1))
 where
  isExitFailure ExitSuccess = False
  isExitFailure (ExitFailure _) = True
