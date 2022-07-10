module Main (main) where

import Test.Hspec (hspec)
import qualified Test.Turtle.Extra
import Prelude

main :: IO ()
main = hspec $ do
  Test.Turtle.Extra.tests
