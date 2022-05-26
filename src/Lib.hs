module Lib (
  main,
) where

import Control.Concurrent.ParallelIO.Global (parallel)
import qualified Control.Foldl as Foldl
import Control.Monad.Except (MonadError (catchError, throwError))
import Control.Monad.Managed (MonadManaged)
import Data.Text.IO (hPutStr)
import qualified FindataFetcher as FF
import FindataTranscoder (
  FindataTranscoderSource (..),
  findataTranscoder,
 )
import PdfToText (pdf2txt, pdftotext)
import Relude
import System.FilePath.Glob (compile, match)
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
  let txt = walletDir </> subdir </> (pdf <.> "txt")
  pdftotext (fpToText pdf) (fpToText txt)
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
  (MonadError e m, MonadIO m, e ~ Text) =>
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

shellNewline :: Shell Line
shellNewline = return mempty

-- | Appends the transaction to the wallet.
appendToWallet ::
  (MonadIO io) =>
  -- | Transaction
  Shell Line ->
  io ()
appendToWallet transaction = do
  wallet <- getWallet
  Turtle.append wallet shellNewline
  Turtle.append wallet transaction

parseAndAppendStatement ::
  (MonadError e m, e ~ Text, MonadManaged m) =>
  FindataTranscoderSource ->
  Turtle.FilePath ->
  m ()
parseAndAppendStatement findataTranscoderSource stmt = do
  stmtTxt <- findataTranscoder findataTranscoderSource (Turtle.input stmt)
  appendToWallet (Turtle.select $ Turtle.textToLines stmtTxt)

parseAndAppendDegiroPortfolioStatement :: Shell ExitCode
parseAndAppendDegiroPortfolioStatement = do
  cdDownloads
  file <- ls $ Turtle.fromText "."
  bool
    (return ExitSuccess)
    (reportErrors ("Parsing " <> fpToText file) $ void (parseAndAppendStatement FindataTranscoderDegiroPortfolio file >> rm file))
    (match (compile "degiro-portfolio.csv") (Turtle.encodeString file))

-- | Moves Google Payslip PDF to the main wallet file.
moveGPayslipToWallet ::
  (MonadError e m, MonadIO m, e ~ Text) =>
  -- | The Google payslip PDF.
  Turtle.FilePath ->
  m ()
moveGPayslipToWallet pdf = flip catchError prependContext $ do
  pdfPath :: Text <- decodePathM pdf
  gpayslipTxt :: Text <- pdf2txt pdfPath
  let gpayslipContent :: Shell Line = Turtle.select $ Turtle.textToLines gpayslipTxt
  ledgerTransaction :: Text <- findataTranscoder FindataTranscoderGPayslip gpayslipContent
  wallet <- getWallet
  Turtle.append wallet (Turtle.select $ emptyLine <> Turtle.textToLines ledgerTransaction)
  rm pdf
 where
  prependContext errMsg = do
    pdfPath <- decodePathM pdf
    throwError $
      "Could not move Google Payslip (" <> pdfPath <> ") to the wallet file.\n" <> errMsg

movePatreonReceipt :: (MonadError e m, MonadManaged m, e ~ Text) => Turtle.FilePath -> m ()
movePatreonReceipt stmt = void $ parseAndAppendStatement FindataTranscoderPatreon stmt

movePatreonReceipts :: Shell ExitCode
movePatreonReceipts = do
  cdDownloads
  file <- ls $ Turtle.fromText "."
  bool
    (return ExitSuccess)
    (reportErrors ("Parsing " <> fpToText file) $ void (movePatreonReceipt file >> rm file))
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

moveGalaxusReceipts :: Shell ExitCode
moveGalaxusReceipts = do
  cdDownloads
  file <- ls $ Turtle.fromText "."
  bool
    (return ExitSuccess)
    (reportErrors ("Moving " <> fpToText file) $ void (moveGalaxusBill file >> rm file))
    (match (compile "*.galaxus") (Turtle.encodeString file))
 where
  moveGalaxusBill ::
    (MonadError e m, e ~ Text, MonadManaged m) =>
    Turtle.FilePath ->
    m ()
  moveGalaxusBill = parseAndAppendStatement FindataTranscoderGalaxus

moveUberEatsBill ::
  (MonadError e m, e ~ Text, MonadManaged m) =>
  Turtle.FilePath ->
  m ()
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
        (\(sourceName, ffSource) -> reportErrors ("Fetching " <> sourceName) $ FF.runFindataFetcher ffSource)
        [ ("Coop receipts", FF.FFSourceCoopSupercard)
        , ("EasyRide receipts", FF.FFSourceEasyRide)
        , ("Galaxus receipts", FF.FFSourceGalaxus)
        , ("Patreon receipts", FF.FFSourcePatreon)
        , ("Revolut statements", FF.FFSourceRevolutMail)
        , ("Uber Eats bills", FF.FFSourceUberEats)
        ]
  anyBcgeCcTextifyAndMovePdfStatementFailure <- Turtle.fold textifyAndMoveBcgeCcPdfStatement (Foldl.any isExitFailure)
  anyBcgeCcParseAndMovePdfStatementFailure <- Turtle.fold parseAndMoveBcgeCcPdfStatement (Foldl.any isExitFailure)
  anyCoopParseAndMoveFailure <- Turtle.fold parseAndMoveCoopPdfReceipts (Foldl.any isExitFailure)
  anyDegiroPortfolioParseAndAppendFailure <- Turtle.fold parseAndAppendDegiroPortfolioStatement (Foldl.any isExitFailure)
  anyGalaxusFailure <- Turtle.fold moveGalaxusReceipts (Foldl.any isExitFailure)
  anyGPayslipFailure <- Turtle.fold (forFileInDls "gpayslip*.pdf" moveGPayslipToWallet) (Foldl.any isExitFailure)
  anyPatreonParseAndMoveFailure <- Turtle.fold movePatreonReceipts (Foldl.any isExitFailure)
  anyRevolutParseAndMoveFailure <- Turtle.fold parseAndMoveRevolutCsvStatements (Foldl.any isExitFailure)
  anySplitwiseParseAndAppendFailure <- Turtle.fold parseAndAppendSplitwise (Foldl.any isExitFailure)
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
        || anySplitwiseParseAndAppendFailure
        || anyUberEatsFailure
    )
    (exit (ExitFailure 1))
 where
  isExitFailure ExitSuccess = False
  isExitFailure (ExitFailure _) = True
