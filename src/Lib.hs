module Lib (
  main,
) where

import Control.Exception (try)
import qualified Control.Foldl as Foldl
import Control.Monad.Except (MonadError (throwError))
import Data.Text.IO (hPutStr)
import qualified FindataFetcher as FF
import Relude
import System.FilePath.Glob (compile, match)
import Turtle (
  ExitCode (ExitFailure, ExitSuccess),
  Shell,
  cd,
  exit,
  home,
  inproc,
  ls,
  rm,
  sh,
  (<.>),
  (</>),
 )
import qualified Turtle

fromEither :: Either a a -> a
fromEither = either id id

fpToText :: Turtle.FilePath -> Text
fpToText = fromEither . Turtle.toText

reportErrors :: (MonadIO io) => Text -> ExceptT Text io () -> io ExitCode
reportErrors name action = do
  eitherErrorOrValue <- runExceptT action
  either
    (\e -> liftIO $ hPutStr stderr (name <> " has failed.\n" <> e) >> return (ExitFailure 1))
    (const $ return ExitSuccess)
    eitherErrorOrValue

-- | Runs pdftotext utility.
--
-- https://en.wikipedia.org/wiki/Pdftotext
pdftotext :: (MonadError e m, MonadIO m, e ~ Text) => Turtle.FilePath -> Turtle.FilePath -> m ()
pdftotext pdfFile txtFile = do
  eitherErrorOrUnit <-
    liftIO $
      try @SomeException . sh $
        void $ inproc "pdftotext" ["-raw", fpToText pdfFile, fpToText txtFile] mempty
  whenLeft () eitherErrorOrUnit (\e -> throwError $ "The pdftotext has failed: " <> show e)

cdDownloads :: (MonadIO io) => io ()
cdDownloads = do
  homeDir <- home
  cd $ homeDir </> Turtle.fromText "Downloads"

parseAndMoveCoopPdfReceipt :: (MonadError e m, MonadIO m, e ~ Text) => Turtle.FilePath -> m ()
parseAndMoveCoopPdfReceipt receiptPdf = do
  homeDir <- home
  let receiptTxt =
        homeDir
          </> Turtle.fromText "Documents/Finance/Wallet/updates/coop-receipts"
          </> (receiptPdf <.> "txt")
  pdftotext receiptPdf receiptTxt
  rm receiptPdf

parseAndMoveCoopPdfReceipts :: Shell ExitCode
parseAndMoveCoopPdfReceipts = do
  cdDownloads
  file <- ls $ Turtle.fromText "."
  bool
    (return ExitSuccess)
    (reportErrors ("Parsing " <> fpToText file) $ parseAndMoveCoopPdfReceipt file)
    (match (compile "Coop *.pdf") (Turtle.encodeString file))

main :: IO ()
main = do
  fetchingExitCodes :: [ExitCode] <-
    mapM
      (\(sourceName, ffSource) -> reportErrors ("Fetching " <> sourceName) $ FF.runFindataFetcher ffSource)
      [ ("Coop receipts", FF.FFSourceCoop)
      , ("EasyRide receipts", FF.FFSourceEasyRide)
      , ("Patreon receipts", FF.FFSourcePatreon)
      , ("Revolut statements", FF.FFSourceRevolutMail)
      ]
  anyParseAndMoveFailure <- Turtle.fold parseAndMoveCoopPdfReceipts (Foldl.any isExitFailure)
  when
    (any isExitFailure fetchingExitCodes || anyParseAndMoveFailure)
    (exit (ExitFailure 1))
 where
  isExitFailure ExitSuccess = False
  isExitFailure (ExitFailure _) = True
