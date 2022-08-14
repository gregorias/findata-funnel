module Main (main) where

import qualified CLI
import Control.Monad (join)
import Options.Applicative (
  execParser,
  fullDesc,
  header,
  helper,
  info,
  progDesc,
  (<**>),
 )

main :: IO ()
main = join $ execParser mainP
 where
  mainP =
    info
      (CLI.individualPipesP <**> helper)
      ( fullDesc
          <> header "findata-funnel"
          <> progDesc "Orchestrates findata tools to automate moving financial data to the wallet."
      )
