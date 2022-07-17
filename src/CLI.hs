-- | A collection of commands to run an e2e update for a specific source.
module CLI (main) where

import Control.Monad (join)
import Options.Applicative (ParserInfo, command, execParser, idm, info, subparser)
import Splitwise (pullSplitwise)
import Turtle (select, textToLines)
import Wallet (appendToWallet)

pullSplitwiseFull :: IO ()
pullSplitwiseFull = pullSplitwise (appendToWallet . Turtle.select . textToLines)

programP :: ParserInfo (IO ())
programP = info (subparser (command "pull-splitwise" (info (pure pullSplitwiseFull) idm))) idm

main :: IO ()
main = join $ execParser programP
