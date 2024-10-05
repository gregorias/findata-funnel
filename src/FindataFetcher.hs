-- | This module handles using findata-fetcher.
module FindataFetcher (
  run,
  Source (..),
  CoopSupercardParameters (..),
  CoopSupercardHeadlessMode (..),
  CoopSupercardVerboseMode (..),
  RevolutParameters (..),
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

data Source parameters output where
  SourceBcge :: Source () Text
  SourceBcgeCc :: Source () ByteString
  SourceCoopSupercard :: Source () ()
  SourceDegiroAccountStatement :: Source () Text
  SourceDegiroPortfolio :: Source () Text
  SourceEasyRide :: Source () ()
  SourceFinpension :: Source () Text
  SourceGalaxus :: Source () ()
  SourceGooglePlay :: Source () ()
  SourceIbActivity :: Source () Text
  SourceMBank :: Source () Text
  SourcePatreon :: Source () ()
  SourceRevolut :: RevolutParameters -> Source RevolutParameters ()
  SourceSplitwise :: Source () Text
  SourceUberEats :: Source () ()

data CoopSupercardHeadlessMode = CoopSupercardHeadless | CoopSupercardNoHeadless

data CoopSupercardVerboseMode = CoopSupercardVerbose | CoopSupercardQuiet

data CoopSupercardParameters = CoopSupercardParameters
  { coopSupercardParametersHeadlessMode :: !CoopSupercardHeadlessMode
  , coopSupercardParametersVerboseMode :: !CoopSupercardVerboseMode
  }

newtype RevolutParameters = RevolutParameters
  { revolutParametersDownloadDirectory :: Text
  }

sourceToCommand :: Source a b -> Text
sourceToCommand SourceBcge = "pull-bcge"
sourceToCommand SourceBcgeCc = "pull-bcgecc"
sourceToCommand SourceCoopSupercard = "coop-supercard-pull"
sourceToCommand SourceDegiroAccountStatement = "degiro-account-pull"
sourceToCommand SourceDegiroPortfolio = "degiro-portfolio-pull"
sourceToCommand SourceEasyRide = "pull-easyride-receipts"
sourceToCommand SourceFinpension = "pull-finpension"
sourceToCommand SourceGalaxus = "pull-galaxus"
sourceToCommand SourceGooglePlay = "pull-google-play-mail"
sourceToCommand SourceIbActivity = "ib-activity-pull"
sourceToCommand SourceMBank = "pull-mbank"
sourceToCommand SourcePatreon = "pull-patreon"
sourceToCommand (SourceRevolut _) = "revolut-pull"
sourceToCommand SourceSplitwise = "pull-splitwise"
sourceToCommand SourceUberEats = "pull-uber-eats"

sourceToParameters :: Source a b -> [Text]
sourceToParameters (SourceRevolut (RevolutParameters{revolutParametersDownloadDirectory = downloadDirectory})) = ["--download-directory=" <> downloadDirectory]
sourceToParameters _ = []

convertTextToOutputType :: Source a output -> ByteString -> output
convertTextToOutputType SourceBcge = decodeUtf8
convertTextToOutputType SourceBcgeCc = id
convertTextToOutputType SourceCoopSupercard = const ()
convertTextToOutputType SourceDegiroAccountStatement = decodeUtf8
convertTextToOutputType SourceDegiroPortfolio = decodeUtf8
convertTextToOutputType SourceEasyRide = const ()
convertTextToOutputType SourceFinpension = decodeUtf8
convertTextToOutputType SourceGalaxus = const ()
convertTextToOutputType SourceGooglePlay = const ()
convertTextToOutputType SourceIbActivity = decodeUtf8
convertTextToOutputType SourceMBank = decodeUtf8
convertTextToOutputType SourcePatreon = const ()
convertTextToOutputType (SourceRevolut _) = const ()
convertTextToOutputType SourceSplitwise = decodeUtf8
convertTextToOutputType SourceUberEats = const ()

-- | Runs findata-fetcher
--
-- Throws IOException on error.
run ::
  (MonadIO m) =>
  Source parameters output ->
  -- The stdout of findata-fetcher
  m output
run source = do
  homeDir <- home
  let ffPath = T.pack (homeDir </> ".local/bin/findata-fetcher")
  (exitCode, stdout) <-
    TurtleB.procStrict
      ffPath
      ( [ sourceToCommand source
        ]
          <> sourceToParameters source
      )
      mempty
  case exitCode of
    Turtle.ExitSuccess -> return $ convertTextToOutputType source stdout
    -- Don't add any "." or newline. The error message is wrapped in parentheses.
    Turtle.ExitFailure _ -> failIO "findata-fetcher has failed"
