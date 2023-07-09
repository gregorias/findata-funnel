module Bcge (pullBcge) where

import Control.Funnel (fetchTranscodeAppend)
import Control.Monad.Cont (MonadIO (liftIO))
import qualified Data.Text.IO as T
import FindataFetcher (FindataFetcherSource (FFSourceBcge), runFindataFetcher)
import FindataTranscoder (FindataTranscoderSource (FindataTranscoderBcge), findataTranscoder)
import Turtle ((</>))
import Turtle.Extra (posixLineToLine, textToShell)
import Wallet (getWalletDir)

pullBcge :: (MonadIO m) => m ()
pullBcge = do
  walletDir <- getWalletDir
  let bcgeLedger :: FilePath = walletDir </> "updates/bcge.ledger"
  fetchTranscodeAppend
    (runFindataFetcher FFSourceBcge)
    transcodeBcge
    (liftIO . T.appendFile bcgeLedger)
 where
  transcodeBcge =
    findataTranscoder FindataTranscoderBcge
      . fmap posixLineToLine
      . textToShell
