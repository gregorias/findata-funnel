-- | A collection of commands to run an e2e update for a specific source.
module CLI (individualPipesP) where

import Options.Applicative (
  Parser,
  command,
  helper,
  info,
  progDesc,
  subparser,
  (<**>),
 )
import Splitwise (pullSplitwise)
import Turtle (select)
import Turtle.Extra (textToPosixLines)
import Wallet (appendTransactionToWallet, getWallet)

pullSplitwiseFull :: IO ()
pullSplitwiseFull = do
  wallet <- getWallet
  pullSplitwise $ appendTransactionToWallet wallet . Turtle.select . textToPosixLines

individualPipesP :: Parser (IO ())
individualPipesP =
  subparser
    ( command
        "pull-splitwise"
        ( info
            (pure pullSplitwiseFull <**> helper)
            (progDesc "Pulls Splitwise data from the Internet and appends Splitwise status to the wallet.")
        )
    )
