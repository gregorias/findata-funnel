-- | A collection of commands to run an e2e update for a specific source.
module CLI (individualPipesP) where

import Bcge (pullBcge)
import Control.Funnel (fetchTranscodeAppend)
import Control.Monad.Cont (MonadIO)
import Degiro (pullDegiroPortfolio)
import FindataFetcher (
  FindataFetcherSource (FFSourceFinpensionPortfolioTotal),
  runFindataFetcher,
 )
import FindataTranscoder (
  FindataTranscoderSource (FindataTranscoderFinpensionPortfolioTotals),
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
import Splitwise (pullSplitwise)
import Turtle (select)
import Turtle.Extra (posixLineToLine, textToPosixLines, textToShell)
import Wallet (appendTransactionToWallet, getWallet)

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
            "bcge"
            "Pulls BCGE statement data from Internet and saves it in a Ledger file in the wallet directory."
            pullBcge
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
