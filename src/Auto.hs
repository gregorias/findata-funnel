{-# LANGUAGE PartialTypeSignatures #-}

-- | This module contains fully-automatic dataflows.
--
-- A fully-automatic dataflow is one that is triggerred by an external event
-- and can be completed without my involvement. Usually these are tasks that
-- don't require my input to solve a 2FA challenge.
module Auto (
  pullAuto,
) where

import Control.Concurrent.ParallelIO.Global (parallel)
import Control.Exception (IOException, catch)
import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import FindataFetcher qualified as FF
import FindataTranscoder (
  FindataTranscoderSource (..),
 )
import Galaxus (pullGalaxusReceipts)
import GooglePlay (pullGooglePlayReceipts)
import Pipeline (parseTextStatements)
import System.IO (stderr)
import Turtle (
  ExitCode (ExitFailure),
  exit,
  home,
  (</>),
 )
import Turtle qualified

downloads :: (MonadIO io) => io Turtle.FilePath
downloads = do
  homeDir <- home
  return $ homeDir </> "Downloads"

-- | Pulls EasyRide receipts to the wallet.
--
-- Throws an IO exception on failure.
pullEasyRideReceipts :: IO ()
pullEasyRideReceipts = do
  FF.run FF.SourceEasyRide

-- | Pulls Patreon receipts to the wallet.
--
-- Throws an IO exception on failure.
pullPatreonReceipts :: IO ()
pullPatreonReceipts = do
  FF.run FF.SourcePatreon
  downloadsDir :: Turtle.FilePath <- downloads
  parseTextStatements downloadsDir "patreon_*.txt" FindataTranscoderPatreon

-- | Pulls Uber Eats receipts to the wallet.
--
-- Throws an IO exception on failure.
pullUberEatsReceipts :: IO ()
pullUberEatsReceipts = do
  FF.run FF.SourceUberEats
  downloadsDir :: Turtle.FilePath <- downloads
  parseTextStatements downloadsDir "*.ubereats" FindataTranscoderUberEats

-- | Pulls data fully automatically.
pullAuto :: IO ()
pullAuto = do
  results :: [Either Text ()] <-
    parallel $
      uncurry handleException
        <$> [ ("EasyRide pull", pullEasyRideReceipts)
            , ("Galaxus pull", pullGalaxusReceipts)
            , ("Google Play pull", pullGooglePlayReceipts)
            , ("Patreon pull", pullPatreonReceipts)
            , ("Uber Eats pull", pullUberEatsReceipts)
            ]
  let errMsgs :: [Text] =
        foldr
          ( \result acc -> case result of
              Left errMsg -> errMsg : acc
              Right _ -> acc
          )
          []
          results
  unless
    (null errMsgs)
    ( do
        forM_ errMsgs $ \errMsg -> do
          T.hPutStr stderr errMsg
          T.hPutStrLn stderr ""
        exit (ExitFailure 1)
    )
 where
  handleException :: Text -> IO a -> IO (Either Text a)
  handleException name action =
    catch
      (Right <$> action)
      ( \(ioe :: IOException) -> do
          let errMsg = "Could not execute " <> name <> "\n" <> T.pack (show ioe)
          return (Left errMsg)
      )
