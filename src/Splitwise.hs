-- | This module implements funnelling Splitwise data to a wallet.
module Splitwise (pullSplitwise) where

import Control.Funnel (fetchTranscodeAppend)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import FindataFetcher (FindataFetcherSource (FFSourceSplitwise), runFindataFetcher)
import FindataTranscoder (
  FindataTranscoderSource (FindataTranscoderSplitwise),
  findataTranscoder,
 )
import Turtle.Extra (
  posixLineToLine,
  textToShell,
 )

fetchSplitwise :: (MonadIO m) => m Text
fetchSplitwise = runFindataFetcher FFSourceSplitwise

transcodeSplitwise :: (MonadIO m) => Text -> m Text
transcodeSplitwise =
  findataTranscoder FindataTranscoderSplitwise
    . fmap posixLineToLine
    . textToShell

-- | Pulls Splitwise data into the Ledger
pullSplitwise ::
  (MonadIO m) =>
  -- | The action that appends to wallet.
  (Text -> m ()) ->
  m ()
pullSplitwise = fetchTranscodeAppend fetchSplitwise transcodeSplitwise
