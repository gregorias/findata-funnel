-- Pulls Google Play receipts to the wallet.
module GooglePlay (pullGooglePlayReceipts) where

import Control.Monad.IO.Class (MonadIO)
import FindataFetcher qualified as FF
import FindataTranscoder (FindataTranscoderSource (..))
import Pipeline (parseTextStatements)
import Turtle (home, (</>))
import Turtle qualified

downloads :: (MonadIO io) => io Turtle.FilePath
downloads = do
  homeDir <- home
  return $ homeDir </> "Downloads"

-- | Pulls Google Play receipts to the wallet.
--
-- Throws an IO exception on failure.
pullGooglePlayReceipts :: (MonadIO io) => io ()
pullGooglePlayReceipts = do
  FF.run FF.SourceGalaxus
  downloadsDir :: Turtle.FilePath <- downloads
  parseTextStatements downloadsDir "Your Google Play*mail" FindataTranscoderGooglePlay
