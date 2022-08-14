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
  FFSourceCoopSupercard :: FindataFetcherSource ()
  FFSourceDegiroPortfolio :: FindataFetcherSource Text
  FFSourceEasyRide :: FindataFetcherSource ()
  FFSourceFinpensionPortfolioTotal :: FindataFetcherSource Text
  FFSourceGalaxus :: FindataFetcherSource ()
  FFSourcePatreon :: FindataFetcherSource ()
  FFSourceRevolutMail :: FindataFetcherSource ()
  FFSourceSplitwise :: FindataFetcherSource Text
  FFSourceUberEats :: FindataFetcherSource ()

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
convertTextToOutputType FFSourceCoopSupercard = const ()
convertTextToOutputType FFSourceDegiroPortfolio = decodeUtf8
convertTextToOutputType FFSourceEasyRide = const ()
convertTextToOutputType FFSourceFinpensionPortfolioTotal = decodeUtf8
convertTextToOutputType FFSourceGalaxus = const ()
convertTextToOutputType FFSourcePatreon = const ()
convertTextToOutputType FFSourceRevolutMail = const ()
convertTextToOutputType FFSourceSplitwise = decodeUtf8
convertTextToOutputType FFSourceUberEats = const ()

-- | Runs findata-fetcher
--
-- Throws IOException on error.
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
