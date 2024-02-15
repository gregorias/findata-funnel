module Bcge (pullBcge) where

import Control.Funnel (fetchTranscodeAppend)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text.IO qualified as T
import FindataFetcher qualified as FF
import FindataTranscoder (FindataTranscoderSource (FindataTranscoderBcge), findataTranscoder)
import Turtle ((</>))
import Turtle.Extra (posixLineToLine, textToShell)
import Wallet (getWalletDir)

pullBcge :: (MonadIO m) => m ()
pullBcge = do
  walletDir <- getWalletDir
  let bcgeLedger :: FilePath = walletDir </> "updates/bcge.ledger"
  fetchTranscodeAppend
    (FF.run FF.SourceBcge)
    transcodeBcge
    (liftIO . T.appendFile bcgeLedger)
 where
  transcodeBcge =
    findataTranscoder FindataTranscoderBcge
      . fmap posixLineToLine
      . textToShell
