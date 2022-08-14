-- | A collection of commands to run an e2e update for a specific source.
module CLI (individualPipesP) where

import Auto (pullAuto)
import Bcge (pullBcge)
import Control.Funnel (fetchTranscodeAppend)
import Control.Monad.Cont (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Degiro (pullDegiroPortfolio)
import FindataFetcher (
  FindataFetcherSource (FFSourceBcgeCc, FFSourceFinpensionPortfolioTotal),
  runFindataFetcher,
 )
import FindataTranscoder (
  FindataTranscoderSource (FindataTranscoderBcgeCc, FindataTranscoderFinpensionPortfolioTotals),
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
import PdfToText (PdfToTextInputMode (PttInputModeStdIn), PdfToTextMode (Raw), PdfToTextOutputMode (PttOutputModeStdOut), pdftotext)
import Splitwise (pullSplitwise)
import Turtle (encodeString, select, (</>))
import Turtle.Extra (posixLineToLine, textToPosixLines, textToShell)
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
            ("Pulls financial data from sources that don't require my involvement.\n" <> "Currently this entails: Coop, EasyRide, Galaxus, Patreon, Uber Eats receipts, and Revolut statements sent to gMail.\n")
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
            "degiro-portfolio"
            "Pulls Degiro portfolio status data from Internet and appends it to the wallet."
            pullDegiroPortfolio
        , pullCommand
            "finpension"
            "Pulls Finpension portfolio status data from Internet and appends it to the wallet."
            pullFinpension
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
