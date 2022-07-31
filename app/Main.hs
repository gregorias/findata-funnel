module Main (main) where

import qualified CLI
import Control.Applicative ((<|>))
import Control.Monad (join)
import qualified Lib
import Options.Applicative (execParser, fullDesc, header, info, progDesc)

main :: IO ()
main = join $ execParser mainP
 where
  mainP =
    info
      ( CLI.individualPipesP
          <|> pure Lib.main
      )
      ( fullDesc
          <> header "findata-funnel"
          <> progDesc "Orchestrates findata tools to automate moving financial data to the wallet."
      )
