-- | This module handles using findata-fetcher.
module FindataFetcher (
  FindataFetcherSource (..),
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

data FindataFetcherSource outputType where
  FFSourceBcge :: FindataFetcherSource Text
  FFSourceBcgeCc :: FindataFetcherSource ByteString
  FFSourceCoopSupercard :: FindataFetcherSource Text
  FFSourceDegiroPortfolio :: FindataFetcherSource Text
  FFSourceEasyRide :: FindataFetcherSource Text
  FFSourceFinpensionPortfolioTotal :: FindataFetcherSource Text
  FFSourceGalaxus :: FindataFetcherSource Text
  FFSourcePatreon :: FindataFetcherSource Text
  FFSourceRevolutMail :: FindataFetcherSource Text
  FFSourceSplitwise :: FindataFetcherSource Text
  FFSourceUberEats :: FindataFetcherSource Text

findataFetcherSourceToCommand :: FindataFetcherSource a -> Text
findataFetcherSourceToCommand FFSourceBcge = "pull-bcge"
findataFetcherSourceToCommand FFSourceBcgeCc = "pull-bcgecc"
findataFetcherSourceToCommand FFSourceCoopSupercard = "pull-coop-supercard"
findataFetcherSourceToCommand FFSourceDegiroPortfolio = "pull-degiro-portfolio"
findataFetcherSourceToCommand FFSourceEasyRide = "pull-easyride-receipts"
findataFetcherSourceToCommand FFSourceFinpensionPortfolioTotal = "pull-finpension-portfolio-total"
findataFetcherSourceToCommand FFSourceGalaxus = "pull-galaxus"
findataFetcherSourceToCommand FFSourcePatreon = "pull-patreon"
findataFetcherSourceToCommand FFSourceRevolutMail = "pull-revolut-mail"
findataFetcherSourceToCommand FFSourceSplitwise = "pull-splitwise"
findataFetcherSourceToCommand FFSourceUberEats = "pull-uber-eats"

convertTextToOutputType :: FindataFetcherSource outputType -> ByteString -> outputType
convertTextToOutputType FFSourceBcge = decodeUtf8
convertTextToOutputType FFSourceBcgeCc = id
convertTextToOutputType FFSourceCoopSupercard = decodeUtf8
convertTextToOutputType FFSourceDegiroPortfolio = decodeUtf8
convertTextToOutputType FFSourceEasyRide = decodeUtf8
convertTextToOutputType FFSourceFinpensionPortfolioTotal = decodeUtf8
convertTextToOutputType FFSourceGalaxus = decodeUtf8
convertTextToOutputType FFSourcePatreon = decodeUtf8
convertTextToOutputType FFSourceRevolutMail = decodeUtf8
convertTextToOutputType FFSourceSplitwise = decodeUtf8
convertTextToOutputType FFSourceUberEats = decodeUtf8

-- | Runs findata-fetcher
runFindataFetcher ::
  (MonadIO m) =>
  FindataFetcherSource outputType ->
  -- The stdout of findata-fetcher
  m outputType
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
      [ "--config_file=" <> configFilePath
      , findataFetcherSourceToCommand source
      ]
      mempty
  case exitCode of
    Turtle.ExitSuccess -> return $ convertTextToOutputType source stdout
    Turtle.ExitFailure _ -> failIO "findata-fetcher has failed.\n"
 where
  eitherToFailIO :: (MonadIO io) => (a -> Text) -> Either a b -> io b
  eitherToFailIO f = eitherToIO (userError . T.unpack . f)
