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
import Turtle (ExitCode (ExitFailure, ExitSuccess), Line, Shell, cd, exit, home, inproc, ls, output, rm, sh, (<.>), (</>))
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

pdf2Txt :: (MonadError e m, MonadIO m, e ~ Text) => Turtle.FilePath -> Turtle.FilePath -> m ()
pdf2Txt pdfFile txtFile = do
  eitherErrorOrUnit <- liftIO $
    try @SomeException . sh $ do
      homeDir <- home
      let pdf2txt = fpToText $ homeDir </> Turtle.fromText ".local/bin/pdf2txt"
      let (txtContent :: Shell Line) = inproc pdf2txt [fpToText pdfFile] mempty
      output txtFile txtContent
  whenLeft () eitherErrorOrUnit (\e -> throwError $ "The pdf2txt has failed: " <> show e)

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
  pdf2Txt receiptPdf receiptTxt
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
      (\(sourceName, ffSource) -> reportErrors ("Fetching" <> sourceName) $ FF.runFindataFetcher ffSource)
      [ ("Coop receipts", FF.FFSourceCoop)
      , ("EasyRide receipts", FF.FFSourceEasyRide)
      ]
  anyParseAndMoveFailure <- Turtle.fold parseAndMoveCoopPdfReceipts (Foldl.any isExitFailure)
  when
    (any isExitFailure fetchingExitCodes || anyParseAndMoveFailure)
    (exit (ExitFailure 1))
 where
  isExitFailure ExitSuccess = False
  isExitFailure (ExitFailure _) = True
