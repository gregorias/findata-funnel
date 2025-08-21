-- | A collection of commands to run an e2e update for a specific source.
module CLI (main) where

import Auto (pullAuto)
import Bcge (pullBcge)
import Control.Foldl qualified as Foldl
import Control.Funnel (fetchTranscodeAppend)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Coop qualified
import Data.Aeson (FromJSON (parseJSON), genericParseJSON)
import Data.Aeson qualified as Aeson
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
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
import GHC.Base (failIO)
import GHC.Generics (Generic)
import GPayslip (GPayslipConfig, pullGooglePayslips)
import Galaxus (pullGalaxusReceipts)
import GooglePlay (pullGooglePlayReceipts)
import Options.Applicative (
  CommandFields,
  Mod,
  Parser,
  command,
  helper,
  info,
  long,
  metavar,
  progDesc,
  str,
  subparser,
  value,
  (<**>),
 )
import Options.Applicative qualified as Optparse
import PdfToText (
  PdfToTextInputMode (PttInputModeStdIn),
  PdfToTextMode (Raw),
  PdfToTextOutputMode (PttOutputModeStdOut),
  pdftotext,
 )
import Relude (chainedTo)
import Splitwise (pullSplitwise)
import System.Directory.Extra (XdgDirectory (XdgConfig), getXdgDirectory)
import System.FilePath.Glob (compile, match)
import Turtle (cd, ls, select, (</>))
import Turtle qualified
import Turtle.Extra (posixLineToLine, textToPosixLines, textToShell)
import Turtle.Line (textToLines)
import Turtle.Prelude (rm)
import Wallet (appendTransactionToWallet, getWallet, getWalletDir)

newtype FindataFunnelConfig = FindataFunnelConfig
  { ffcGPayslip :: GPayslipConfig
  }
  deriving stock (Generic, Show)

instance FromJSON FindataFunnelConfig where
  parseJSON =
    genericParseJSON
      ( Aeson.defaultOptions
          { Aeson.fieldLabelModifier = drop 3
          }
      )

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

-- | All pull subcommands, e.g., pull-coop or pull-gpayslip.
--
-- The subcommands accept the funnel's config and extract their relevant
-- parameters.
individualPipesP :: Parser (FindataFunnelConfig -> IO ())
individualPipesP =
  subparser
    ( mconcat
        [ pullCommand
            "auto"
            ( "Pulls financial data from sources that don't require my involvement.\n"
                <> "Currently this entails: Coop, EasyRide, Galaxus, Patreon Uber Eats receipts, and Revolut statements sent to gMail.\n"
            )
            (ignoreConfig pullAuto)
        , pullCommand
            "bcge"
            "Pulls BCGE statement data from Internet and saves it in a Ledger file in the wallet directory."
            (ignoreConfig pullBcge)
        , pullCommand
            "bcge-cc"
            "Pulls BCGE credit card statement data from Internet and saves it in a Ledger file in the wallet directory."
            (ignoreConfig pullBcgeCc)
        , pullCommand
            "coop-supercard"
            "Pulls Coop Supercard receipts (you need to manually log in and download) and saves it in a Ledger file in the wallet directory."
            (ignoreConfig Coop.pullCoopSupercardReceipts)
        , command'
            "coop-parse-receipt-pdfs-to-wallet"
            "Parses Coop receipt PDFs to Ledger text and writes them to the wallet."
            (ignoreConfig Coop.parseReceiptPdfsToWallet)
        , pullCommand
            "degiro-account-statement"
            "Pulls Degiro account statement and appends it to the wallet."
            (ignoreConfig pullDegiroAccountStatement)
        , pullCommand
            "degiro-portfolio"
            "Pulls Degiro portfolio status data from Internet and appends it to the wallet."
            (ignoreConfig pullDegiroPortfolio)
        , pullCommand
            "finpension"
            "Pulls Finpension portfolio status data from Internet and appends it to the wallet."
            (ignoreConfig pullFinpension)
        , pullCommand
            "galaxus"
            "Pulls Galaxus email receipts and appends them to the wallet."
            (ignoreConfig pullGalaxusReceipts)
        , pullCommand
            "google-play"
            "Pulls Google Play email receipts and appends them to the wallet."
            (ignoreConfig pullGooglePlayReceipts)
        , pullCommand
            "gpayslip"
            "Pulls Google Payslips from Internet and appends them to the wallet."
            (pullGooglePayslips . ffcGPayslip)
        , pullCommand
            "mbank"
            "Pulls mBank's transaction history into a Ledger file in the wallet directory."
            (ignoreConfig pullMBank)
        , pullCommand
            "ib"
            "Pulls Interactive Brokers data from Internet into a Ledger file in the wallet directory."
            (ignoreConfig pullIB)
        , pullCommand
            "revolut"
            "[TODO] Pulls Revolut data from Internet into Ledger files in the wallet directory."
            (ignoreConfig pullRevolut)
        , pullCommand
            "splitwise"
            "Pulls Splitwise data from Internet and appends Splitwise status to the wallet."
            (ignoreConfig pullSplitwiseFull)
        ]
    )
 where
  ignoreConfig :: a -> (b -> a)
  ignoreConfig = const

  command' :: String -> String -> a -> Mod CommandFields a
  command' title description action =
    command
      title
      ( info
          (pure action <**> helper)
          (progDesc description)
      )

  pullCommand title = command' ("pull-" <> title)

-- The main command parser without --help and --version.
main :: Parser (IO ())
main = runApp <$> configP <*> individualPipesP
 where
  configPathArgP :: Parser (Maybe FilePath)
  configPathArgP =
    Optparse.option
      (return <$> str)
      ( long "config"
          <> metavar "PATH"
          <> value Nothing
      )

  getConfigDefault :: IO FilePath
  getConfigDefault = do
    xdgConfig <- getXdgDirectory XdgConfig ""
    return $ xdgConfig <> "/findata/funnel.json"

  applyConfigDefault :: Maybe FilePath -> IO FilePath
  applyConfigDefault = maybe getConfigDefault return

  configPathP :: Parser (IO FilePath)
  configPathP = applyConfigDefault <$> configPathArgP

  parseConfig :: FilePath -> IO FindataFunnelConfig
  parseConfig configPath = do
    configBs <- LBS.readFile configPath
    case Aeson.eitherDecode configBs of
      Left err -> failIO $ "Could not parse config file: " <> err
      Right config -> return config

  configP :: Parser (IO FindataFunnelConfig)
  -- TODO: Really useful to extend with =<<,chainedTo
  configP = chainedTo parseConfig <$> configPathP

  runApp :: IO FindataFunnelConfig -> (FindataFunnelConfig -> IO ()) -> IO ()
  runApp configIO app = do
    config <- configIO
    app config
