-- | This module handles using findata-fetcher.
module FindataFetcher (
  FindataFetcherSource (..),
  runFindataFetcher,
) where

import Control.Exception (try)
import Control.Monad.Except (MonadError (throwError))
import Relude
import Turtle (ShellFailed (ShellFailed), home, pushd, sh, shells, (</>))
import qualified Turtle

data FindataFetcherSource
  = FFSourceCoop
  | FFSourceEasyRide
  | FFSourcePatreon
  | FFSourceRevolutMail

findataFetcherSourceToCommand :: FindataFetcherSource -> Text
findataFetcherSourceToCommand FFSourceCoop = "pull-coop-receipts"
findataFetcherSourceToCommand FFSourceEasyRide = "pull-easyride-receipts"
findataFetcherSourceToCommand FFSourcePatreon = "pull-patreon"
findataFetcherSourceToCommand FFSourceRevolutMail = "pull-revolut-mail"

-- | Runs findata-fetcher
runFindataFetcher :: (MonadError e m, MonadIO m, e ~ Text) => FindataFetcherSource -> m ()
runFindataFetcher source = do
  homeDir <- home
  eitherErrorOrUnit <- liftIO $
    try @ShellFailed . sh $ do
      pushd $ homeDir </> Turtle.fromText "Code/findata-fetcher"
      shells ("pipenv run python -m fetcher.tool " <> findataFetcherSourceToCommand source) mempty
  whenLeft
    ()
    eitherErrorOrUnit
    (\(ShellFailed _ _) -> throwError "The findata-fetcher tool has failed.")
