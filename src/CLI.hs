-- | A collection of commands to run an e2e update for a specific source.
module CLI (individualPipesP) where

import Auto (pullAuto)
import Bcge (pullBcge)
import Control.Foldl qualified as Foldl
import Control.Funnel (fetchTranscodeAppend)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Coop qualified
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO qualified as T
import Degiro (
  pullDegiroAccountStatement,
  pullDegiroPortfolio,
 )
import FindataFetcher qualified as FF
import FindataTranscoder (
  FindataTranscoderSource (..),
  findataTranscoder,
 )
import GPayslip (pullGooglePayslips)
import Galaxus (pullGalaxusReceipts)
import GooglePlay (pullGooglePlayReceipts)
import Options.Applicative (
  Parser,
  command,
  helper,
  info,
  progDesc,
  subparser,
  (<**>),
 )
import PdfToText (
  PdfToTextInputMode (PttInputModeStdIn),
  PdfToTextMode (Raw),
  PdfToTextOutputMode (PttOutputModeStdOut),
  pdftotext,
 )
import Splitwise (pullSplitwise)
import System.FilePath.Glob (compile, match)
import Turtle (cd, ls, select, (</>))
import Turtle qualified
import Turtle.Extra (posixLineToLine, textToPosixLines, textToShell)
import Turtle.Line (textToLines)
import Turtle.Prelude (rm)
import Wallet (appendTransactionToWallet, getWallet, getWalletDir)

pullBcgeCc :: (MonadIO m) => m ()
pullBcgeCc = do
  bcgeCcPdfStatement :: ByteString <- FF.run FF.SourceBcgeCc
  textStatement :: Text <-
    pdftotext Raw (PttInputModeStdIn (return bcgeCcPdfStatement)) PttOutputModeStdOut
  ledger :: Text <-
    findataTranscoder FindataTranscoderBcgeCc $
      posixLineToLine
        <$> textToShell textStatement
  walletDir <- getWalletDir
  let bcgeCcLedger :: FilePath = walletDir </> "updates/bcge-cc.ledger"
  liftIO $ T.appendFile bcgeCcLedger ledger

pullCsBrokerageAccount :: (MonadIO m) => m ()
pullCsBrokerageAccount = do
  csStatement :: Text <- decodeUtf8 <$> FF.run FF.SourceCsBrokerageHistory
  ledger :: Text <-
    findataTranscoder FindataTranscoderCsBrokerageAccount $
      posixLineToLine
        <$> textToShell csStatement
  walletDir <- getWalletDir
  let csLedger :: FilePath = walletDir </> "updates/charles-schwab-brokerage.ledger"
  liftIO $ T.appendFile csLedger ledger

pullCsEacAccount :: (MonadIO m) => m ()
pullCsEacAccount = do
  csStatement :: Text <- decodeUtf8 <$> FF.run FF.SourceCsEacHistory
  ledger :: Text <-
    findataTranscoder FindataTranscoderCsEacAccount $
      posixLineToLine
        <$> textToShell csStatement
  walletDir <- getWalletDir
  let csLedger :: FilePath = walletDir </> "updates/charles-schwab-eac.ledger"
  liftIO $ T.appendFile csLedger ledger

pullFinpension :: (MonadIO m) => m ()
pullFinpension = do
  wallet <- getWallet
  fetchTranscodeAppend
    (FF.run FF.SourceFinpension)
    transcodeFinpension
    (appendToWallet wallet)
 where
  transcodeFinpension =
    findataTranscoder FindataTranscoderFinpension
      . fmap posixLineToLine
      . textToShell
  appendToWallet wallet =
    appendTransactionToWallet wallet
      . Turtle.select
      . textToPosixLines

pullMBank :: (MonadIO m) => m ()
pullMBank = do
  mBankCsv <- FF.run FF.SourceMBank
  mBankLedger <- findataTranscoder FindataTranscoderMBank (textToFindataTranscoderInput mBankCsv)
  mBankLedgerFile <- (</> "updates/mbank.ledger") <$> getWalletDir
  liftIO $ T.writeFile mBankLedgerFile mBankLedger
 where
  textToFindataTranscoderInput :: Text -> Turtle.Shell Turtle.Line
  textToFindataTranscoderInput = fmap posixLineToLine . textToShell

pullIB :: (MonadIO m) => m ()
pullIB = do
  ibCsv :: Text <- FF.run FF.SourceIbActivity
  ledger :: Text <-
    findataTranscoder FindataTranscoderIBActivity $
      posixLineToLine
        <$> textToShell ibCsv
  walletDir <- getWalletDir
  liftIO $ T.appendFile (walletDir </> "updates/ib.ledger") ledger

pullRevolut :: (MonadIO m) => m ()
pullRevolut = do
  walletDir <- getWalletDir
  let revolutDownloadDir = walletDir </> "updates/revolut"
  FF.run (FF.SourceRevolut (FF.RevolutParameters (T.pack revolutDownloadDir)))
  cd revolutDownloadDir
  Turtle.reduce Foldl.mconcat $ do
    file <- ls "."
    csv :: Turtle.FilePath <- bool Turtle.empty (return file) (match (compile "*.csv") file)
    csvText :: Text <- liftIO $ T.readFile csv
    ledger :: Text <- findataTranscoder FindataTranscoderRevolut (Turtle.select $ textToLines csvText)
    liftIO $ T.writeFile (csv <> ".ledger") ledger
    rm csv

pullSplitwiseFull :: IO ()
pullSplitwiseFull = do
  wallet <- getWallet
  pullSplitwise $ appendTransactionToWallet wallet . Turtle.select . textToPosixLines

individualPipesP :: Parser (IO ())
individualPipesP =
  subparser
    ( mconcat
        [ pullCommand
            "auto"
            ( "Pulls financial data from sources that don't require my involvement.\n"
                <> "Currently this entails: Coop, EasyRide, Galaxus, Patreon Uber Eats receipts, and Revolut statements sent to gMail.\n"
            )
            pullAuto
        , pullCommand
            "bcge"
            "Pulls BCGE statement data from Internet and saves it in a Ledger file in the wallet directory."
            pullBcge
        , pullCommand
            "bcge-cc"
            "Pulls BCGE credit card statement data from Internet and saves it in a Ledger file in the wallet directory."
            pullBcgeCc
        , pullCommand
            "coop-supercard"
            "Pulls Coop Supercard receipts and saves it in a Ledger file in the wallet directory."
            ( Coop.pullCoopSupercardReceipts
                (FF.CoopSupercardParameters FF.CoopSupercardNoHeadless FF.CoopSupercardVerbose)
            )
        , pullCommand
            "cs-brokerage-account"
            "Pulls Charles Schwab brokerage account's transaction history into a Ledger file in the wallet directory."
            pullCsBrokerageAccount
        , pullCommand
            "cs-eac-account"
            "Pulls Charles Schwab EAC account's transaction history into a Ledger file in the wallet directory."
            pullCsEacAccount
        , pullCommand
            "degiro-account-statement"
            "Pulls Degiro account statement and appends it to the wallet."
            pullDegiroAccountStatement
        , pullCommand
            "degiro-portfolio"
            "Pulls Degiro portfolio status data from Internet and appends it to the wallet."
            pullDegiroPortfolio
        , pullCommand
            "finpension"
            "Pulls Finpension portfolio status data from Internet and appends it to the wallet."
            pullFinpension
        , pullCommand
            "galaxus"
            "Pulls Galaxus email receipts and appends them to the wallet."
            pullGalaxusReceipts
        , pullCommand
            "google-play"
            "Pulls Google Play email receipts and appends them to the wallet."
            pullGooglePlayReceipts
        , pullCommand
            "gpayslip"
            "Pulls Google Payslips from Internet and appends them to the wallet."
            pullGooglePayslips
        , pullCommand
            "mbank"
            "Pulls mBank's transaction history into a Ledger file in the wallet directory."
            pullMBank
        , pullCommand
            "ib"
            "Pulls Interactive Brokers data from Internet into a Ledger file in the wallet directory."
            pullIB
        , pullCommand
            "revolut"
            "[TODO] Pulls Revolut data from Internet into Ledger files in the wallet directory."
            pullRevolut
        , pullCommand
            "splitwise"
            "Pulls Splitwise data from Internet and appends Splitwise status to the wallet."
            pullSplitwiseFull
        ]
    )
 where
  pullCommand title description action =
    command
      ("pull-" <> title)
      ( info
          (pure action <**> helper)
          (progDesc description)
      )
