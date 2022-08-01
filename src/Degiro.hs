module Degiro (pullDegiroPortfolio) where

import Control.Funnel (fetchTranscodeAppend)
import Control.Monad.Cont (MonadIO)
import FindataFetcher (FindataFetcherSource (FFSourceDegiroPortfolio), runFindataFetcher)
import FindataTranscoder (FindataTranscoderSource (FindataTranscoderDegiroPortfolio), findataTranscoder)
import qualified Turtle
import Turtle.Extra (
  posixLineToLine,
  textToPosixLines,
  textToShell,
 )
import Wallet (appendTransactionToWallet, getWallet)

pullDegiroPortfolio :: (MonadIO m) => m ()
pullDegiroPortfolio = do
  wallet <- getWallet
  fetchTranscodeAppend
    (runFindataFetcher FFSourceDegiroPortfolio)
    transcodeDegiroPortfolio
    (appendToWallet wallet)
 where
  transcodeDegiroPortfolio =
    findataTranscoder FindataTranscoderDegiroPortfolio
      . fmap posixLineToLine
      . textToShell
  appendToWallet wallet =
    appendTransactionToWallet wallet
      . Turtle.select
      . textToPosixLines
