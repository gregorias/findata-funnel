module Test.Wallet (tests) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Managed (with)
import Data.Text (Text)
import qualified Data.Text.IO as T
import NeatInterpolation (trimming)
import qualified Test.Hspec as Hspec
import qualified Turtle
import Turtle.Extra (textToPosixLines)
import Wallet (appendTransactionToWallet)

tests :: Hspec.SpecWith ()
tests = do
  Hspec.describe "Test.Wallet" $ do
    Hspec.describe "appendTransactionToWallet" $ do
      Hspec.it "Appends a transaction prepending a newline." $ do
        with
          (Turtle.mktempfile "/tmp" "")
          ( \tmpTxt -> do
              let transaction =
                    [trimming|
                      2022-02-06 Foo
                        Foo 123
                        Bar -123|]
                      <> "\n"
              appendTransactionToWallet tmpTxt (Turtle.select $ textToPosixLines transaction)
              walletContent :: Text <- liftIO $ T.readFile tmpTxt
              walletContent `Hspec.shouldBe` ("\n" <> transaction)
          )
