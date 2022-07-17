module Main (main) where

import Test.Hspec (hspec)
import qualified Test.Turtle.Extra
import qualified Test.Wallet
import Prelude

main :: IO ()
main = hspec $ do
  Test.Turtle.Extra.tests
  Test.Wallet.tests
