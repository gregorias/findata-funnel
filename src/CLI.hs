-- | A collection of commands to run an e2e update for a specific source.
module CLI (individualPipesP) where

import Bcge (pullBcge)
import Degiro (pullDegiroPortfolio)
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
    ( mconcat
        [ command
            "pull-bcge"
            ( info
                (pure pullBcge <**> helper)
                (progDesc "Pulls BCGE statement data from Internet and saves it in a Ledger file in the wallet directory.")
            )
        , command
            "pull-degiro-portfolio"
            ( info
                (pure pullDegiroPortfolio <**> helper)
                (progDesc "Pulls Degiro portfolio status data from Internet and appends it to the wallet.")
            )
        , command
            "pull-splitwise"
            ( info
                (pure pullSplitwiseFull <**> helper)
                (progDesc "Pulls Splitwise data from Internet and appends Splitwise status to the wallet.")
            )
        ]
    )
