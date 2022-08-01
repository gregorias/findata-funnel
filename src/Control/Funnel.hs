module Control.Funnel (fetchTranscodeAppend) where

import Data.Text (Text)

fetchTranscodeAppend ::
  (Monad m) =>
  -- | The action that fetches data.
  m Text ->
  -- | The action that transcodes data.
  (Text -> m Text) ->
  -- | The action that appends data.
  (Text -> m ()) ->
  m ()
fetchTranscodeAppend fetch transcode append = fetch >>= transcode >>= append
