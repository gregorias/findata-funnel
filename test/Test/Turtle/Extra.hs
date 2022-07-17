module Test.Turtle.Extra (tests) where

import qualified Control.Foldl as Foldl
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import NeatInterpolation (trimming)
import Test.Hspec (shouldBe)
import qualified Test.Hspec as Hspec
import qualified Turtle
import Turtle.Extra (
  PosixLine,
  posixLinesToText,
  textToShell,
 )

tests :: Hspec.SpecWith ()
tests = do
  Hspec.describe "Test.Turtle.Extra" $ do
    Hspec.describe "textToShell" $ do
      Hspec.it "wraps Text into a Shell" $ do
        let unwrap :: (MonadIO io) => Turtle.Shell PosixLine -> io Text
            unwrap = fmap posixLinesToText . Turtle.reduce Foldl.list
            helloWorld = "Hello,\nWorld!\n"
        let wrappedText = textToShell helloWorld
        output <- unwrap wrappedText
        output `shouldBe` helloWorld
