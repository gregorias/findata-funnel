module Degiro (
  pullDegiroAccountStatement,
  pullDegiroPortfolio,
) where

import Control.Funnel (fetchTranscodeAppend)
import Control.Monad.Cont (MonadIO)
import Data.Text (Text)
import FindataFetcher qualified as FF
import FindataTranscoder (
  FindataTranscoderSource (
    FindataTranscoderDegiroAccount,
    FindataTranscoderDegiroPortfolio
  ),
  findataTranscoder,
 )
import Turtle qualified
import Turtle.Extra (
  posixLineToLine,
  textToPosixLines,
  textToShell,
 )
import Wallet (appendTransactionToWallet, getWallet)

appendToWallet :: (MonadIO m) => FilePath -> Text -> m ()
appendToWallet wallet =
  appendTransactionToWallet wallet
    . Turtle.select
    . textToPosixLines

pullDegiroAccountStatement :: (MonadIO m) => m ()
pullDegiroAccountStatement = do
  wallet <- getWallet
  fetchTranscodeAppend
    (FF.run FF.SourceDegiroAccountStatement)
    transcodeDegiroAccountStatement
    (appendToWallet wallet)
 where
  transcodeDegiroAccountStatement =
    findataTranscoder FindataTranscoderDegiroAccount
      . fmap posixLineToLine
      . textToShell

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
