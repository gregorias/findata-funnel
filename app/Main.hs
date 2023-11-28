module Main (main) where

import CLI qualified
import Control.Monad (join)
import Data.Version (showVersion)
import Options.Applicative (
  execParser,
  fullDesc,
  header,
  helper,
  info,
  progDesc,
  simpleVersioner,
  (<**>),
 )
import Paths_findata_funnel (version)

main :: IO ()
main = join $ execParser mainP
 where
  mainP =
    info
      (CLI.individualPipesP <**> helper <**> simpleVersioner (showVersion version))
      ( fullDesc
          <> header "findata-funnel"
          <> progDesc "Orchestrates findata tools to automate moving financial data to the wallet."
      )
