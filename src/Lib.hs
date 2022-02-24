module Lib (
  main,
) where

import Control.Concurrent.ParallelIO.Global (parallel)
import qualified Control.Foldl as Foldl
import Control.Monad.Except (MonadError)
import Control.Monad.Managed (MonadManaged)
import Data.Text.IO (hPutStr)
import qualified FindataFetcher as FF
import FindataTranscoder (
  FindataTranscoderSource (..),
  findataTranscoder,
 )
import PdfToText (pdftotext)
import Relude
import System.FilePath.Glob (compile, match)
import Turtle (
  ExitCode (ExitFailure, ExitSuccess),
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

fromEither :: Either a a -> a
fromEither = either id id

fpToText :: Turtle.FilePath -> Text
fpToText = fromEither . Turtle.toText

getWalletDir :: (MonadIO m) => m Turtle.FilePath
getWalletDir = do
  homeDir <- home
  return $ homeDir </> Turtle.fromText "Documents/Finance/Wallet"

getWallet :: (MonadIO m) => m Turtle.FilePath
getWallet = getWalletDir <&> (</> "wallet.txt")

reportErrors :: (MonadIO io) => Text -> ExceptT Text io () -> io ExitCode
reportErrors name action = do
  eitherErrorOrValue <- runExceptT action
  either
    (\e -> liftIO $ hPutStr stderr (name <> " has failed.\n" <> e) >> return (ExitFailure 1))
    (const $ return ExitSuccess)
    eitherErrorOrValue

cdDownloads :: (MonadIO io) => io ()
cdDownloads = do
  homeDir <- home
  cd $ homeDir </> Turtle.fromText "Downloads"

parseAndMoveCoopPdfReceipt ::
  (MonadError e m, MonadIO m, e ~ Text) =>
  -- | The path to the receipt.
  Turtle.FilePath ->
  m ()
parseAndMoveCoopPdfReceipt receiptPdf = do
  walletDir <- getWalletDir
  let receiptTxt =
        walletDir
          </> Turtle.fromText "updates/coop-receipts"
          </> (receiptPdf <.> "txt")
  pdftotext (fpToText receiptPdf) (fpToText receiptTxt)
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
  FindataTranscoderSource ->
  Turtle.FilePath ->
  m Turtle.FilePath
parseAndMoveStatement findataTranscoderSource stmt = do
  walletDir <- getWalletDir
  let stmtLedger = walletDir </> Turtle.fromText "updates" </> (stmt <.> "ledger")
  findataTranscoder findataTranscoderSource stmt stmtLedger
  rm stmt
  return stmtLedger

parseAndAppendStatement ::
  (MonadError e m, e ~ Text, MonadManaged m) =>
  FindataTranscoderSource ->
  Turtle.FilePath ->
  m ()
parseAndAppendStatement findataTranscoderSource stmt = do
  tmpStmt <- Turtle.mktempfile "/tmp" "findata-funnel"
  findataTranscoder findataTranscoderSource stmt tmpStmt
  wallet <- getWallet
  Turtle.append wallet (return $ Turtle.unsafeTextToLine "")
  Turtle.append wallet (Turtle.input tmpStmt)

parseAndAppendDegiroPortfolioStatement :: Shell ExitCode
parseAndAppendDegiroPortfolioStatement = do
  cdDownloads
  file <- ls $ Turtle.fromText "."
  bool
    (return ExitSuccess)
    (reportErrors ("Parsing " <> fpToText file) $ void (parseAndAppendStatement FindataTranscoderDegiroPortfolio file >> rm file))
    (match (compile "degiro-portfolio.csv") (Turtle.encodeString file))

parseAndMovePatreonReceipt :: (MonadError e m, MonadIO m, e ~ Text) => Turtle.FilePath -> m ()
parseAndMovePatreonReceipt stmt = void $ parseAndMoveStatement FindataTranscoderPatreon stmt

parseAndMovePatreonReceipts :: Shell ExitCode
parseAndMovePatreonReceipts = do
  cdDownloads
  file <- ls $ Turtle.fromText "."
  bool
    (return ExitSuccess)
    (reportErrors ("Parsing " <> fpToText file) $ parseAndMovePatreonReceipt file)
    (match (compile "patreon_*.txt") (Turtle.encodeString file))

parseAndMoveRevolutCsvStatement :: (MonadError e m, MonadIO m, e ~ Text) => Turtle.FilePath -> m ()
parseAndMoveRevolutCsvStatement stmt = void $ parseAndMoveStatement FindataTranscoderRevolut stmt

parseAndMoveRevolutCsvStatements :: Shell ExitCode
parseAndMoveRevolutCsvStatements = do
  cdDownloads
  file <- ls $ Turtle.fromText "."
  bool
    (return ExitSuccess)
    (reportErrors ("Parsing " <> fpToText file) $ parseAndMoveRevolutCsvStatement file)
    (match (compile "revolut-account-statement*.csv") (Turtle.encodeString file))

parseAndAppendSplitwise :: Shell ExitCode
parseAndAppendSplitwise = do
  cdDownloads
  file <- ls $ Turtle.fromText "."
  bool
    (return ExitSuccess)
    (reportErrors ("Parsing " <> fpToText file) $ void (parseAndAppendStatement FindataTranscoderSplitwise file >> rm file))
    (match (compile "splitwise.csv") (Turtle.encodeString file))

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
  anyDegiroPortfolioParseAndAppendFailure <- Turtle.fold parseAndAppendDegiroPortfolioStatement (Foldl.any isExitFailure)
  anyPatreonParseAndMoveFailure <- Turtle.fold parseAndMovePatreonReceipts (Foldl.any isExitFailure)
  anyRevolutParseAndMoveFailure <- Turtle.fold parseAndMoveRevolutCsvStatements (Foldl.any isExitFailure)
  anySplitwiseParseAndAppendFailure <- Turtle.fold parseAndAppendSplitwise (Foldl.any isExitFailure)
  when
    ( any isExitFailure fetchingExitCodes || anyCoopParseAndMoveFailure
        || anyDegiroPortfolioParseAndAppendFailure
        || anyPatreonParseAndMoveFailure
        || anyRevolutParseAndMoveFailure
        || anySplitwiseParseAndAppendFailure
    )
    (exit (ExitFailure 1))
 where
  isExitFailure ExitSuccess = False
  isExitFailure (ExitFailure _) = True
