{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Integration tests
--
-- These are medium tests that run findata-funnel's functionality in a sandbox.
--
-- These tests depend on the filesystem as well as on other findata tools being
-- installed in the system.
module Main (main) where

import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Managed (Managed, with)
import Control.Monad.Managed.Safe (MonadManaged, using)
import Data.Either (isRight)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GPayslip (moveGPayslipToWallet)
import Test.Hspec (
  Spec,
  describe,
  hspec,
  it,
  shouldBe,
  shouldNotSatisfy,
  shouldSatisfy,
 )
import Turtle (FilePath)
import qualified Turtle
import Prelude

unsafeRunManaged :: Managed a -> IO a
unsafeRunManaged m = with m return

gpayslipTest :: Spec
gpayslipTest = it "Moves a payslip to a ledger" $ do
  maybeResults :: Either Text (Text, Bool) <- unsafeRunTest $ do
    gpayslipPdf :: Turtle.FilePath <- using createTmpPayslipPdf
    walletTxt :: Turtle.FilePath <- using createTmpWalletTxt
    pdfSize <- show <$> Turtle.du gpayslipPdf
    liftIO . putStrLn $ "File length: " <> pdfSize
    moveGPayslipToWallet walletTxt gpayslipPdf
    walletContent :: Text <- liftIO $ T.readFile walletTxt
    doesGPayslipExist <- Turtle.testfile gpayslipPdf
    return (walletContent, doesGPayslipExist)
  maybeResults `shouldSatisfy` isRight
  let Right (walletContent, doesGPayslipExist) = maybeResults
  doesGPayslipExist `shouldBe` False
  -- I'm only testing that some content has been appended.
  -- Testing that the content is good is in findata-transcoder's scope.
  walletContent `shouldNotSatisfy` T.null
 where
  createTmpPayslipPdf :: (MonadManaged m) => m Turtle.FilePath
  createTmpPayslipPdf = do
    tmpPdf <- Turtle.mktempfile "/tmp" ""
    Turtle.cp "integration-test/data/gpayslip.pdf" tmpPdf
    return tmpPdf
  createTmpWalletTxt :: (MonadManaged m) => m Turtle.FilePath
  createTmpWalletTxt = Turtle.mktempfile "/tmp" ""

  unsafeRunTest = unsafeRunManaged . runExceptT

gpayslipTests :: Spec
gpayslipTests = do
  describe "Google Payslip tests" $ do
    gpayslipTest

main :: IO ()
main = hspec $ do
  describe "Integration tests" $ do
    gpayslipTests
