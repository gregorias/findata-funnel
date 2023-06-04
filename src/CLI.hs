-- | A collection of commands to run an e2e update for a specific source.
module CLI (individualPipesP) where

import Auto (pullAuto)
import Bcge (pullBcge)
import Control.Foldl qualified as Foldl
import Control.Funnel (fetchTranscodeAppend)
import Control.Monad.Cont (MonadIO, liftIO)
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.Either.Extra (fromEither)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO qualified as T
import Degiro (pullDegiroPortfolio)
import FindataFetcher (
  FindataFetcherRevolutParameters (FindataFetcherRevolutParameters),
  FindataFetcherSource (..),
  runFindataFetcher,
 )
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
import Turtle (cd, encodeString, fromText, ls, select, toText, (</>))
import Turtle qualified
import Turtle.Extra (posixLineToLine, textToPosixLines, textToShell)
import Turtle.Line (textToLines)
import Turtle.Prelude (rm)
import Wallet (appendTransactionToWallet, getWallet, getWalletDir)

pullBcgeCc :: (MonadIO m) => m ()
pullBcgeCc = do
  bcgeCcPdfStatement :: ByteString <- runFindataFetcher FFSourceBcgeCc
  textStatement :: Text <-
    pdftotext Raw (PttInputModeStdIn (return bcgeCcPdfStatement)) PttOutputModeStdOut
  ledger :: Text <-
    findataTranscoder FindataTranscoderBcgeCc $
      posixLineToLine <$> textToShell textStatement
  walletDir <- getWalletDir
  let bcgeCcLedger :: FilePath = encodeString $ walletDir </> "updates/bcge-cc.ledger"
  liftIO $ T.appendFile bcgeCcLedger ledger

pullCsBrokerageAccount :: (MonadIO m) => m ()
pullCsBrokerageAccount = do
  csStatement :: Text <- decodeUtf8 <$> runFindataFetcher FFSourceCs
  ledger :: Text <- findataTranscoder FindataTranscoderCs $ posixLineToLine <$> textToShell csStatement
  walletDir <- getWalletDir
  let csLedger :: FilePath = encodeString $ walletDir </> "updates/charles-schwab.ledger"
  liftIO $ T.appendFile csLedger ledger

pullFinpension :: (MonadIO m) => m ()
pullFinpension = do
  wallet <- getWallet
  fetchTranscodeAppend
    (runFindataFetcher FFSourceFinpension)
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
  mBankCsv <- runFindataFetcher FFSourceMBank
  mBankLedger <- findataTranscoder FindataTranscoderMBank (textToFindataTranscoderInput mBankCsv)
  mBankLedgerFile <- (</> "updates/mbank.ledger") <$> getWalletDir
  liftIO $ T.writeFile (filePathToString mBankLedgerFile) mBankLedger
 where
  filePathToString = T.unpack . fromEither . toText

  textToFindataTranscoderInput :: Text -> Turtle.Shell Turtle.Line
  textToFindataTranscoderInput = fmap posixLineToLine . textToShell

pullIB :: (MonadIO m) => m ()
pullIB = do
  ibCsv :: Text <- runFindataFetcher FFSourceIB
  ledger :: Text <-
    findataTranscoder FindataTranscoderIBActivity $
      posixLineToLine <$> textToShell ibCsv
  walletDir <- getWalletDir
  liftIO $ T.appendFile (encodeString $ walletDir </> "updates/ib.ledger") ledger

pullRevolut :: (MonadIO m) => m ()
pullRevolut = do
  walletDir <- getWalletDir
  let revolutDownloadDir = walletDir </> "updates/revolut"
  runFindataFetcher (FFSourceRevolut (FindataFetcherRevolutParameters (fromEither $ toText revolutDownloadDir)))
  cd revolutDownloadDir
  Turtle.reduce Foldl.mconcat $ do
    file <- ls $ Turtle.fromText "."
    csv :: Turtle.FilePath <- bool Turtle.empty (return file) (match (compile "*.csv") (Turtle.encodeString file))
    csvText :: Text <- liftIO $ Turtle.readTextFile csv
    ledger :: Text <- findataTranscoder FindataTranscoderRevolut (Turtle.select $ textToLines csvText)
    let csvAsText :: String = T.unpack . fromEither . toText $ csv
    liftIO $ T.writeFile (csvAsText <> ".ledger") ledger
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
            "cs-brokerage-account"
            "Pulls Charles Schwab brokerage account's transaction history from Internet into a Ledger file in the wallet directory."
            pullCsBrokerageAccount
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
