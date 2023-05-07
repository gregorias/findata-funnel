-- Pulls Google Play receipts to the wallet.
module GooglePlay (pullGooglePlayReceipts) where

import Control.Monad.Cont (MonadIO)
import FindataFetcher qualified as FF
import FindataTranscoder (FindataTranscoderSource (..))
import Pipeline (parseTextStatements)
import Turtle (home, (</>))
import Turtle qualified

downloads :: (MonadIO io) => io Turtle.FilePath
downloads = do
  homeDir <- home
  return $ homeDir </> Turtle.fromText "Downloads"

-- | Pulls Google Play receipts to the wallet.
--
-- Throws an IO exception on failure.
pullGooglePlayReceipts :: (MonadIO io) => io ()
pullGooglePlayReceipts = do
  FF.runFindataFetcher FF.FFSourceGalaxus
  downloadsDir :: Turtle.FilePath <- downloads
  parseTextStatements downloadsDir "Your Google Play*mail" FindataTranscoderGooglePlay
