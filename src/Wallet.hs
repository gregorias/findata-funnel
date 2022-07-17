-- | Utilities for interacting with the ledger file.
module Wallet (
  getWalletDir,
  getWallet,
  appendTransactionToWallet,
) where

import Control.Monad.IO.Class (MonadIO)
import Turtle (home, (<&>), (</>))
import qualified Turtle
import Turtle.Extra (emptyLine)
import qualified Turtle.Extra as Turtle

getWalletDir :: (MonadIO m) => m Turtle.FilePath
getWalletDir = do
  homeDir <- home
  return $ homeDir </> Turtle.fromText "Documents/Finance/Wallet"

getWallet :: (MonadIO m) => m Turtle.FilePath
getWallet = getWalletDir <&> (</> "wallet.txt")

-- | Appends the transaction to a wallet.
appendTransactionToWallet ::
  (MonadIO io) =>
  -- | Wallet
  Turtle.FilePath ->
  -- | Transaction
  Turtle.Shell Turtle.PosixLine ->
  io ()
appendTransactionToWallet wallet transaction = do
  Turtle.append wallet (Turtle.select emptyLine)
  Turtle.append wallet (Turtle.posixLineToLine <$> transaction)
