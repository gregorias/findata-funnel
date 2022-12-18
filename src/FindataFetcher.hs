-- | This module handles using findata-fetcher.
module FindataFetcher (
  FindataFetcherSource (..),
  FindataFetcherCsParameters (..),
  FindataFetcherRevolutParameters (..),
  runFindataFetcher,
) where

import Control.Exception.Extra (eitherToIO, failIO)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Turtle (home, (</>))
import qualified Turtle
import qualified Turtle.Bytes as TurtleB

data FindataFetcherSource parameters output where
  FFSourceBcge :: FindataFetcherSource () Text
  FFSourceBcgeCc :: FindataFetcherSource () ByteString
  FFSourceCoopSupercard :: FindataFetcherSource () ()
  FFSourceCs :: FindataFetcherCsParameters -> FindataFetcherSource FindataFetcherCsParameters ()
  FFSourceDegiroPortfolio :: FindataFetcherSource () Text
  FFSourceEasyRide :: FindataFetcherSource () ()
  FFSourceFinpensionPortfolioTotal :: FindataFetcherSource () Text
  FFSourceGalaxus :: FindataFetcherSource () ()
  FFSourceIB :: FindataFetcherSource () Text
  FFSourceMBank :: FindataFetcherSource () Text
  FFSourcePatreon :: FindataFetcherSource () ()
  FFSourceRevolut :: FindataFetcherRevolutParameters -> FindataFetcherSource FindataFetcherRevolutParameters ()
  FFSourceRevolutMail :: FindataFetcherSource () ()
  FFSourceSplitwise :: FindataFetcherSource () Text
  FFSourceUberEats :: FindataFetcherSource () ()

newtype FindataFetcherCsParameters = FindataFetcherCsParameters
  { findataFetcherCsParametersDownloadDirectory :: Text
  }

newtype FindataFetcherRevolutParameters = FindataFetcherRevolutParameters
  { findataFetcherRevolutParametersDownloadDirectory :: Text
  }

findataFetcherSourceToCommand :: FindataFetcherSource a b -> Text
findataFetcherSourceToCommand FFSourceBcge = "pull-bcge"
findataFetcherSourceToCommand FFSourceBcgeCc = "pull-bcgecc"
findataFetcherSourceToCommand FFSourceCoopSupercard = "pull-coop-supercard"
findataFetcherSourceToCommand (FFSourceCs _) = "pull-cs-account-history"
findataFetcherSourceToCommand FFSourceDegiroPortfolio = "pull-degiro-portfolio"
findataFetcherSourceToCommand FFSourceEasyRide = "pull-easyride-receipts"
findataFetcherSourceToCommand FFSourceFinpensionPortfolioTotal = "pull-finpension-portfolio-total"
findataFetcherSourceToCommand FFSourceGalaxus = "pull-galaxus"
findataFetcherSourceToCommand FFSourceIB = "pull-ib"
findataFetcherSourceToCommand FFSourceMBank = "pull-mbank"
findataFetcherSourceToCommand FFSourcePatreon = "pull-patreon"
findataFetcherSourceToCommand (FFSourceRevolut _) = "pull-revolut"
findataFetcherSourceToCommand FFSourceRevolutMail = "pull-revolut-mail"
findataFetcherSourceToCommand FFSourceSplitwise = "pull-splitwise"
findataFetcherSourceToCommand FFSourceUberEats = "pull-uber-eats"

sourceToParameters :: FindataFetcherSource a b -> [Text]
sourceToParameters (FFSourceCs (FindataFetcherCsParameters{findataFetcherCsParametersDownloadDirectory = downloadDirectory})) = ["--download-directory=" <> downloadDirectory]
sourceToParameters (FFSourceRevolut (FindataFetcherRevolutParameters{findataFetcherRevolutParametersDownloadDirectory = downloadDirectory})) = ["--download-directory=" <> downloadDirectory]
sourceToParameters _ = []

convertTextToOutputType :: FindataFetcherSource a output -> ByteString -> output
convertTextToOutputType FFSourceBcge = decodeUtf8
convertTextToOutputType FFSourceBcgeCc = id
convertTextToOutputType FFSourceCoopSupercard = const ()
convertTextToOutputType (FFSourceCs _) = const ()
convertTextToOutputType FFSourceDegiroPortfolio = decodeUtf8
convertTextToOutputType FFSourceEasyRide = const ()
convertTextToOutputType FFSourceFinpensionPortfolioTotal = decodeUtf8
convertTextToOutputType FFSourceGalaxus = const ()
convertTextToOutputType FFSourceIB = decodeUtf8
convertTextToOutputType FFSourceMBank = decodeUtf8
convertTextToOutputType FFSourcePatreon = const ()
convertTextToOutputType (FFSourceRevolut _) = const ()
convertTextToOutputType FFSourceRevolutMail = const ()
convertTextToOutputType FFSourceSplitwise = decodeUtf8
convertTextToOutputType FFSourceUberEats = const ()

-- | Runs findata-fetcher
--
-- Throws IOException on error.
runFindataFetcher ::
  (MonadIO m) =>
  FindataFetcherSource parameters output ->
  -- The stdout of findata-fetcher
  m output
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
    TurtleB.procStrict
      ffPath
      ( [ "--config_file=" <> configFilePath
        , findataFetcherSourceToCommand source
        ]
          <> sourceToParameters source
      )
      mempty
  case exitCode of
    Turtle.ExitSuccess -> return $ convertTextToOutputType source stdout
    Turtle.ExitFailure _ -> failIO "findata-fetcher has failed.\n"
 where
  eitherToFailIO :: (MonadIO io) => (a -> Text) -> Either a b -> io b
  eitherToFailIO f = eitherToIO (userError . T.unpack . f)
