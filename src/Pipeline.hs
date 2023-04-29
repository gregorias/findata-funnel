module Pipeline (parseTextStatements) where

import qualified Control.Foldl as Foldl
import Control.Monad.Cont (MonadIO)
import Data.Bool (bool)
import Data.Text (Text)
import FindataTranscoder (FindataTranscoderSource, findataTranscoder)
import System.FilePath.Glob (compile, match)
import Turtle (ls, rm)
import qualified Turtle
import qualified Turtle.Extra as Turtle
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
  stmt <- bool Turtle.empty (return file) (match (compile stmtGlobPattern) (Turtle.encodeString $ Turtle.filename file))
  transaction :: Text <- findataTranscoder ftSource (Turtle.input stmt)
  wallet <- getWallet
  appendTransactionToWallet wallet (Turtle.select $ Turtle.textToPosixLines transaction)
  rm stmt
