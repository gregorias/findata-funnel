module Degiro (pullDegiroPortfolio) where

import Control.Funnel (fetchTranscodeAppend)
import Control.Monad.Cont (MonadIO)
import FindataFetcher qualified as FF
import FindataTranscoder (FindataTranscoderSource (FindataTranscoderDegiroPortfolio), findataTranscoder)
import Turtle qualified
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
    (FF.run FF.SourceDegiroPortfolio)
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
