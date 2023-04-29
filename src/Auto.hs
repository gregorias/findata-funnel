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
import Control.Exception (IOException, catch, throwIO, try)
import qualified Control.Foldl as Foldl
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Bool (bool)
import Data.Either (isRight)
import Data.Either.Combinators (leftToMaybe)
import qualified FindataFetcher as FF
import FindataTranscoder (
  FindataTranscoderSource (..),
 )
import Galaxus (pullGalaxusReceipts)
import PdfToText (
  PdfToTextInputMode (PttInputModeFilePath),
  PdfToTextMode (..),
  PdfToTextOutputMode (PttOutputModeFilePath),
  pdftotext,
 )
import Pipeline (parseTextStatements)
import System.FilePath.Glob (compile, match)
import System.IO (hPutStr, hPutStrLn, stderr)
import Turtle (
  ExitCode (ExitFailure),
  cd,
  exit,
  home,
  ls,
  rm,
  (<.>),
  (</>),
 )
import qualified Turtle
import Wallet (getWalletDir)

downloads :: (MonadIO io) => io Turtle.FilePath
downloads = do
  homeDir <- home
  return $ homeDir </> Turtle.fromText "Downloads"

cdDownloads :: (MonadIO io) => io ()
cdDownloads = downloads >>= cd

printIOExceptionOnStderr :: String -> IO a -> IO a
printIOExceptionOnStderr name action =
  catch
    action
    ( \(ioe :: IOException) -> do
        hPutStrLn stderr $ "Could not execute " <> name
        hPutStr stderr (show ioe)
        throwIO ioe
    )

textifyPdf ::
  (MonadIO m) =>
  -- | The target path in the wallet dir.
  Turtle.FilePath ->
  -- | The path to the PDF.
  Turtle.FilePath ->
  m ()
textifyPdf subdir pdf = do
  walletDir <- getWalletDir
  let txtFile = walletDir </> subdir </> (pdf <.> "txt")
  pdftotext Raw (PttInputModeFilePath pdf) (PttOutputModeFilePath txtFile)

-- | Pulls coop receipts to the wallet.
--
-- Throws an IO exception on failure.
pullCoopReceipts :: IO ()
pullCoopReceipts = do
  -- The coop fetcher often fails, so let's not block textification if that happens.
  ioException :: (Maybe IOException) <-
    fmap leftToMaybe . try @IOException $ FF.runFindataFetcher FF.FFSourceCoopSupercard
  textifyCoopPdfReceipts
  maybe (return ()) throwIO ioException
 where
  textifyCoopPdfReceipts :: IO ()
  textifyCoopPdfReceipts = Turtle.reduce Foldl.mconcat $ do
    cdDownloads
    file <- ls $ Turtle.fromText "."
    pdf <- bool Turtle.empty (return file) (match (compile "Coop *.pdf") (Turtle.encodeString file))
    textifyPdf "updates/coop-receipts" pdf
    rm pdf

-- | Pulls EasyRide receipts to the wallet.
--
-- Throws an IO exception on failure.
pullEasyRideReceipts :: IO ()
pullEasyRideReceipts = do
  FF.runFindataFetcher FF.FFSourceEasyRide

-- | Pulls Patreon receipts to the wallet.
--
-- Throws an IO exception on failure.
pullPatreonReceipts :: IO ()
pullPatreonReceipts = do
  FF.runFindataFetcher FF.FFSourcePatreon
  downloadsDir :: Turtle.FilePath <- downloads
  parseTextStatements downloadsDir "patreon_*.txt" FindataTranscoderPatreon

-- | Pulls Revolut receipts to the wallet.
--
-- Throws an IO exception on failure.
pullRevolutReceipts :: IO ()
pullRevolutReceipts = do
  FF.runFindataFetcher FF.FFSourceRevolutMail
  downloadsDir :: Turtle.FilePath <- downloads
  parseTextStatements downloadsDir "revolut-account-statement*.csv" FindataTranscoderRevolut

-- | Pulls Uber Eats receipts to the wallet.
--
-- Throws an IO exception on failure.
pullUberEatsReceipts :: IO ()
pullUberEatsReceipts = do
  FF.runFindataFetcher FF.FFSourceUberEats
  downloadsDir :: Turtle.FilePath <- downloads
  parseTextStatements downloadsDir "*.ubereats" FindataTranscoderUberEats

-- | Pulls data fully automatically.
pullAuto :: IO ()
pullAuto = do
  pullSuccesses :: [Bool] <-
    parallel $
      isIOSuccessful . uncurry printIOExceptionOnStderr
        <$> [ ("Coop pull", pullCoopReceipts)
            , ("EasyRide pull", pullEasyRideReceipts)
            , ("Galaxus pull", pullGalaxusReceipts)
            , ("Patreon pull", pullPatreonReceipts)
            , ("Revolut pull", pullRevolutReceipts)
            , ("Uber Eats pull", pullUberEatsReceipts)
            ]
  when (False `elem` pullSuccesses) (exit (ExitFailure 1))
 where
  isIOSuccessful :: IO () -> IO Bool
  isIOSuccessful = fmap isRight . try @IOException
