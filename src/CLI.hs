-- | A collection of commands to run an e2e update for a specific source.
module CLI (individualPipesP) where

import Auto (pullAuto)
import Bcge (pullBcge)
import qualified Control.Foldl as Foldl
import Control.Funnel (fetchTranscodeAppend)
import Control.Monad.Cont (MonadIO, liftIO)
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.Either.Extra (fromEither)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Degiro (pullDegiroPortfolio)
import FindataFetcher (
  FindataFetcherCsParameters (FindataFetcherCsParameters),
  FindataFetcherSource (FFSourceBcgeCc, FFSourceCs, FFSourceFinpensionPortfolioTotal, FFSourceIB),
  runFindataFetcher,
 )
import FindataTranscoder (
  FindataTranscoderSource (
    FindataTranscoderBcgeCc,
    FindataTranscoderCs,
    FindataTranscoderFinpensionPortfolioTotals,
    FindataTranscoderIBActivity
  ),
  findataTranscoder,
 )
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
import qualified Turtle
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

pullCS :: (MonadIO m) => m ()
pullCS = do
  walletDir <- getWalletDir
  let csDownloadDir = walletDir </> "updates/charles-schwab-transaction-history"
  runFindataFetcher (FFSourceCs (FindataFetcherCsParameters (fromEither $ toText csDownloadDir)))
  Turtle.reduce Foldl.mconcat $ do
    cd csDownloadDir
    file <- ls $ Turtle.fromText "."
    csv <- bool Turtle.empty (return file) (match (compile "*.csv") (Turtle.encodeString file))
    csvText :: Text <- liftIO $ Turtle.readTextFile csv
    ledger :: Text <- findataTranscoder FindataTranscoderCs (Turtle.select $ textToLines csvText)
    let csvAsText :: String = T.unpack . fromEither . toText $ csv
    liftIO $ T.writeFile (csvAsText <> ".ledger") ledger
    rm csv

pullFinpension :: (MonadIO m) => m ()
pullFinpension = do
  wallet <- getWallet
  fetchTranscodeAppend
    (runFindataFetcher FFSourceFinpensionPortfolioTotal)
    transcodeFinpension
    (appendToWallet wallet)
 where
  transcodeFinpension =
    findataTranscoder FindataTranscoderFinpensionPortfolioTotals
      . fmap posixLineToLine
      . textToShell
  appendToWallet wallet =
    appendTransactionToWallet wallet
      . Turtle.select
      . textToPosixLines

pullIB :: (MonadIO m) => m ()
pullIB = do
  ibCsv :: Text <- runFindataFetcher FFSourceIB
  ledger :: Text <-
    findataTranscoder FindataTranscoderIBActivity $
      posixLineToLine <$> textToShell ibCsv
  walletDir <- getWalletDir
  liftIO $ T.appendFile (encodeString $ walletDir </> "updates/ib.ledger") ledger

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
                <> "Currently this entails: Coop, EasyRide, Galaxus, Patreon, Uber Eats receipts, and Revolut statements sent to gMail.\n"
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
            "cs"
            "Pulls Charles Schwab's transaction history from Internet into a Ledger file in the wallet directory."
            pullCS
        , pullCommand
            "degiro-portfolio"
            "Pulls Degiro portfolio status data from Internet and appends it to the wallet."
            pullDegiroPortfolio
        , pullCommand
            "finpension"
            "Pulls Finpension portfolio status data from Internet and appends it to the wallet."
            pullFinpension
        , pullCommand
            "ib"
            "Pulls Interactive Brokers data from Internet into a Ledger file in the wallet directory."
            pullIB
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
