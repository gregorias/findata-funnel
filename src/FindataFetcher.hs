-- | This module handles using findata-fetcher.
module FindataFetcher (
  FindataFetcherSource (..),
  FindataFetcherRevolutParameters (..),
  runFindataFetcher,
) where

import Control.Exception.Extra (failIO)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Turtle (home, (</>))
import Turtle qualified
import Turtle.Bytes qualified as TurtleB

data FindataFetcherSource parameters output where
  FFSourceBcge :: FindataFetcherSource () Text
  FFSourceBcgeCc :: FindataFetcherSource () ByteString
  FFSourceCoopSupercard :: FindataFetcherSource () ()
  FFSourceCs :: FindataFetcherSource () ByteString
  FFSourceDegiroPortfolio :: FindataFetcherSource () Text
  FFSourceEasyRide :: FindataFetcherSource () ()
  FFSourceFinpension :: FindataFetcherSource () Text
  FFSourceGalaxus :: FindataFetcherSource () ()
  FFSourceGooglePlay :: FindataFetcherSource () ()
  FFSourceIB :: FindataFetcherSource () Text
  FFSourceMBank :: FindataFetcherSource () Text
  FFSourcePatreon :: FindataFetcherSource () ()
  FFSourceRevolut :: FindataFetcherRevolutParameters -> FindataFetcherSource FindataFetcherRevolutParameters ()
  FFSourceRevolutMail :: FindataFetcherSource () ()
  FFSourceSplitwise :: FindataFetcherSource () Text
  FFSourceUberEats :: FindataFetcherSource () ()

newtype FindataFetcherRevolutParameters = FindataFetcherRevolutParameters
  { findataFetcherRevolutParametersDownloadDirectory :: Text
  }

findataFetcherSourceToCommand :: FindataFetcherSource a b -> Text
findataFetcherSourceToCommand FFSourceBcge = "pull-bcge"
findataFetcherSourceToCommand FFSourceBcgeCc = "pull-bcgecc"
findataFetcherSourceToCommand FFSourceCoopSupercard = "pull-coop-supercard"
findataFetcherSourceToCommand FFSourceCs = "pull-cs-account-history"
findataFetcherSourceToCommand FFSourceDegiroPortfolio = "pull-degiro-portfolio"
findataFetcherSourceToCommand FFSourceEasyRide = "pull-easyride-receipts"
findataFetcherSourceToCommand FFSourceFinpension = "pull-finpension"
findataFetcherSourceToCommand FFSourceGalaxus = "pull-galaxus"
findataFetcherSourceToCommand FFSourceGooglePlay = "pull-google-play-mail"
findataFetcherSourceToCommand FFSourceIB = "ib-pull"
findataFetcherSourceToCommand FFSourceMBank = "pull-mbank"
findataFetcherSourceToCommand FFSourcePatreon = "pull-patreon"
findataFetcherSourceToCommand (FFSourceRevolut _) = "pull-revolut"
findataFetcherSourceToCommand FFSourceRevolutMail = "pull-revolut-mail"
findataFetcherSourceToCommand FFSourceSplitwise = "pull-splitwise"
findataFetcherSourceToCommand FFSourceUberEats = "pull-uber-eats"

sourceToParameters :: FindataFetcherSource a b -> [Text]
sourceToParameters (FFSourceRevolut (FindataFetcherRevolutParameters{findataFetcherRevolutParametersDownloadDirectory = downloadDirectory})) = ["--download-directory=" <> downloadDirectory]
sourceToParameters _ = []

convertTextToOutputType :: FindataFetcherSource a output -> ByteString -> output
convertTextToOutputType FFSourceBcge = decodeUtf8
convertTextToOutputType FFSourceBcgeCc = id
convertTextToOutputType FFSourceCoopSupercard = const ()
convertTextToOutputType FFSourceCs = id
convertTextToOutputType FFSourceDegiroPortfolio = decodeUtf8
convertTextToOutputType FFSourceEasyRide = const ()
convertTextToOutputType FFSourceFinpension = decodeUtf8
convertTextToOutputType FFSourceGalaxus = const ()
convertTextToOutputType FFSourceGooglePlay = const ()
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
  let ffPath = T.pack (homeDir </> ".local/bin/findata-fetcher")
  (exitCode, stdout) <-
    TurtleB.procStrict
      ffPath
      ( [ findataFetcherSourceToCommand source
        ]
          <> sourceToParameters source
      )
      mempty
  case exitCode of
    Turtle.ExitSuccess -> return $ convertTextToOutputType source stdout
    -- Don't add any "." or newline. The error message is wrapped in parentheses.
    Turtle.ExitFailure _ -> failIO "findata-fetcher has failed"
