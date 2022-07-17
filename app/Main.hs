module Main (main) where

import qualified CLI
import qualified Lib
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then Lib.main
    else CLI.main
