-- Pulls Galaxus receipts to the wallet.
module Galaxus (pullGalaxusReceipts) where

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

-- | Pulls Galaxus receipts to the wallet.
--
-- Throws an IO exception on failure.
pullGalaxusReceipts :: (MonadIO io) => io ()
pullGalaxusReceipts = do
  FF.run FF.SourceGalaxus
  downloadsDir :: Turtle.FilePath <- downloads
  parseTextStatements downloadsDir "*.galaxus" FindataTranscoderGalaxus
