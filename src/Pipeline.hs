module Pipeline (parseTextStatements) where

import Control.Foldl qualified as Foldl
import Control.Monad.IO.Class (MonadIO)
import Data.Bool (bool)
import Data.Text (Text)
import FindataTranscoder (FindataTranscoderSource, findataTranscoder)
import System.FilePath.Glob (compile, match)
import Turtle (ls, rm)
import Turtle qualified
import Turtle.Extra qualified as Turtle
import Wallet (appendTransactionToWallet, getWallet)

-- | Parses all text statements in a directory, appends them to a wallet, and deletes the source.
parseTextStatements ::
  (MonadIO io) =>
  -- | Source directory
  Turtle.FilePath ->
  -- | Statement filename glob pattern
  String ->
  FindataTranscoderSource ->
  io ()
parseTextStatements sourceDir stmtGlobPattern ftSource = Turtle.reduce Foldl.mconcat $ do
  file <- ls sourceDir
  stmt <- bool Turtle.empty (return file) (match (compile stmtGlobPattern) (Turtle.filename file))
  transaction :: Text <- findataTranscoder ftSource (Turtle.input stmt)
  wallet <- getWallet
  appendTransactionToWallet wallet (Turtle.select $ Turtle.textToPosixLines transaction)
  rm stmt
