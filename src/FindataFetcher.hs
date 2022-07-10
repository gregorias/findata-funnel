-- | This module handles using findata-fetcher.
module FindataFetcher (
  FindataFetcherSource (..),
  runFindataFetcher,
) where

import Control.Monad.Except (MonadError (throwError), liftEither)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Turtle (
  home,
  (</>),
 )
import qualified Turtle

data FindataFetcherSource
  = FFSourceCoopSupercard
  | FFSourceEasyRide
  | FFSourceGalaxus
  | FFSourcePatreon
  | FFSourceRevolutMail
  | FFSourceSplitwise
  | FFSourceUberEats

findataFetcherSourceToCommand :: FindataFetcherSource -> Text
findataFetcherSourceToCommand FFSourceCoopSupercard = "pull-coop-supercard"
findataFetcherSourceToCommand FFSourceEasyRide = "pull-easyride-receipts"
findataFetcherSourceToCommand FFSourceGalaxus = "pull-galaxus"
findataFetcherSourceToCommand FFSourcePatreon = "pull-patreon"
findataFetcherSourceToCommand FFSourceRevolutMail = "pull-revolut-mail"
findataFetcherSourceToCommand FFSourceSplitwise = "pull-splitwise"
findataFetcherSourceToCommand FFSourceUberEats = "pull-uber-eats"

-- | Runs findata-fetcher
runFindataFetcher ::
  (MonadError e m, MonadIO m, e ~ Text) =>
  FindataFetcherSource ->
  m Text
runFindataFetcher source = do
  homeDir <- home
  ffPath <- liftEither <$> Turtle.toText $ homeDir </> ".local/bin/findata-fetcher"
  configFilePath <-
    liftEither
      <$> Turtle.toText
      $ homeDir </> "Code/findata/fetcher/config.json"
  (exitCode, stdout) <-
    Turtle.procStrict
      ffPath
      [ "--config_file=" <> configFilePath
      , findataFetcherSourceToCommand source
      ]
      mempty
  case exitCode of
    Turtle.ExitSuccess -> return stdout
    Turtle.ExitFailure _ -> throwError "The findata-fetcher tool has failed."
