-- | Utilities for interacting with the ledger file.
module Wallet (
  getWalletDir,
  getWallet,
  appendToWallet,
) where

import Control.Monad.IO.Class (MonadIO)
import Turtle (home, (<&>), (</>))
import qualified Turtle
import Turtle.Extra (emptyLine)

getWalletDir :: (MonadIO m) => m Turtle.FilePath
getWalletDir = do
  homeDir <- home
  return $ homeDir </> Turtle.fromText "Documents/Finance/Wallet"

getWallet :: (MonadIO m) => m Turtle.FilePath
getWallet = getWalletDir <&> (</> "wallet.txt")

-- | Appends the transaction to the wallet.
appendToWallet ::
  (MonadIO io) =>
  -- | Transaction
  Turtle.Shell Turtle.Line ->
  io ()
appendToWallet transaction = do
  wallet <- getWallet
  Turtle.append wallet (Turtle.select emptyLine)
  Turtle.append wallet transaction
