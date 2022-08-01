-- | This module handles using findata-fetcher.
module FindataFetcher (
  FindataFetcherSource (..),
  runFindataFetcher,
) where

import Control.Exception.Extra (eitherToIO, failIO)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import qualified Data.Text as T
import Turtle (home, (</>))
import qualified Turtle

data FindataFetcherSource
  = FFSourceCoopSupercard
  | FFSourceDegiroPortfolio
  | FFSourceEasyRide
  | FFSourceGalaxus
  | FFSourcePatreon
  | FFSourceRevolutMail
  | FFSourceSplitwise
  | FFSourceUberEats

findataFetcherSourceToCommand :: FindataFetcherSource -> Text
findataFetcherSourceToCommand FFSourceCoopSupercard = "pull-coop-supercard"
findataFetcherSourceToCommand FFSourceDegiroPortfolio = "pull-degiro-portfolio"
findataFetcherSourceToCommand FFSourceEasyRide = "pull-easyride-receipts"
findataFetcherSourceToCommand FFSourceGalaxus = "pull-galaxus"
findataFetcherSourceToCommand FFSourcePatreon = "pull-patreon"
findataFetcherSourceToCommand FFSourceRevolutMail = "pull-revolut-mail"
findataFetcherSourceToCommand FFSourceSplitwise = "pull-splitwise"
findataFetcherSourceToCommand FFSourceUberEats = "pull-uber-eats"

-- | Runs findata-fetcher
runFindataFetcher ::
  (MonadIO m) =>
  FindataFetcherSource ->
  -- The stdout of findata-fetcher
  m Text
runFindataFetcher source = do
  homeDir <- home
  ffPath <-
    eitherToFailIO
      ("Could not convert findata-fetcher path to text.\n" <>)
      (Turtle.toText $ homeDir </> ".local/bin/findata-fetcher")
  configFilePath <-
    eitherToFailIO
      ("Could not convert findata-fetcher config path to text.\n" <>)
      (Turtle.toText $ homeDir </> "Code/findata/fetcher/config.json")
  (exitCode, stdout) <-
    Turtle.procStrict
      ffPath
      [ "--config_file=" <> configFilePath
      , findataFetcherSourceToCommand source
      ]
      mempty
  case exitCode of
    Turtle.ExitSuccess -> return stdout
    Turtle.ExitFailure _ -> failIO "findata-fetcher has failed.\n"
 where
  eitherToFailIO :: (MonadIO io) => (a -> Text) -> Either a b -> io b
  eitherToFailIO f = eitherToIO (userError . T.unpack . f)
