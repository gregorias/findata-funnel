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
  SourceCoopSupercard :: CoopSupercardParameters -> Source CoopSupercardParameters ()
  SourceCs :: Source () ByteString
  SourceDegiroAccountStatement :: Source () Text
  SourceDegiroPortfolio :: Source () Text
  SourceEasyRide :: Source () ()
  SourceFinpension :: Source () Text
  SourceGalaxus :: Source () ()
  SourceGooglePlay :: Source () ()
  SourceIB :: Source () Text
  SourceMBank :: Source () Text
  SourcePatreon :: Source () ()
  SourceRevolut :: RevolutParameters -> Source RevolutParameters ()
  SourceRevolutMail :: Source () ()
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
sourceToCommand (SourceCoopSupercard _) = "coop-supercard-pull"
sourceToCommand SourceCs = "pull-cs-account-history"
sourceToCommand SourceDegiroAccountStatement = "pull-degiro-account"
sourceToCommand SourceDegiroPortfolio = "pull-degiro-portfolio"
sourceToCommand SourceEasyRide = "pull-easyride-receipts"
sourceToCommand SourceFinpension = "pull-finpension"
sourceToCommand SourceGalaxus = "pull-galaxus"
sourceToCommand SourceGooglePlay = "pull-google-play-mail"
sourceToCommand SourceIB = "ib-pull"
sourceToCommand SourceMBank = "pull-mbank"
sourceToCommand SourcePatreon = "pull-patreon"
sourceToCommand (SourceRevolut _) = "pull-revolut"
sourceToCommand SourceRevolutMail = "pull-revolut-mail"
sourceToCommand SourceSplitwise = "pull-splitwise"
sourceToCommand SourceUberEats = "pull-uber-eats"

sourceToParameters :: Source a b -> [Text]
sourceToParameters
  ( SourceCoopSupercard
      ( CoopSupercardParameters
          { coopSupercardParametersVerboseMode = verboseMode
          , coopSupercardParametersHeadlessMode = headlessMode
          }
        )
    ) =
    [ case verboseMode of
        CoopSupercardVerbose -> "--verbose"
        CoopSupercardQuiet -> "--quiet"
    , case headlessMode of
        CoopSupercardHeadless -> "--headless"
        CoopSupercardNoHeadless -> "--no-headless"
    ]
sourceToParameters (SourceRevolut (RevolutParameters{revolutParametersDownloadDirectory = downloadDirectory})) = ["--download-directory=" <> downloadDirectory]
sourceToParameters _ = []

convertTextToOutputType :: Source a output -> ByteString -> output
convertTextToOutputType SourceBcge = decodeUtf8
convertTextToOutputType SourceBcgeCc = id
convertTextToOutputType (SourceCoopSupercard _) = const ()
convertTextToOutputType SourceCs = id
convertTextToOutputType SourceDegiroAccountStatement = decodeUtf8
convertTextToOutputType SourceDegiroPortfolio = decodeUtf8
convertTextToOutputType SourceEasyRide = const ()
convertTextToOutputType SourceFinpension = decodeUtf8
convertTextToOutputType SourceGalaxus = const ()
convertTextToOutputType SourceGooglePlay = const ()
convertTextToOutputType SourceIB = decodeUtf8
convertTextToOutputType SourceMBank = decodeUtf8
convertTextToOutputType SourcePatreon = const ()
convertTextToOutputType (SourceRevolut _) = const ()
convertTextToOutputType SourceRevolutMail = const ()
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
