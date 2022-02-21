-- | This module handles using findata-transcoder.
module FindataTranscoder (
  findataTranscoder,
  FindataTranscoderSource (..),
) where

import Control.Exception (try)
import Control.Monad.Except (MonadError (throwError))
import Relude
import Turtle (input)
import qualified Turtle

data FindataTranscoderSource
  = FindataTranscoderDegiroAccount
  | FindataTranscoderDegiroPortfolio
  | FindataTranscoderPatreon
  | FindataTranscoderRevolut
  | FindataTranscoderSplitwise

findataTranscoderSourceToCommand :: FindataTranscoderSource -> Text
findataTranscoderSourceToCommand FindataTranscoderDegiroAccount = "parse-degiro-account"
findataTranscoderSourceToCommand FindataTranscoderDegiroPortfolio = "parse-degiro-portfolio"
findataTranscoderSourceToCommand FindataTranscoderPatreon = "parse-patreon"
findataTranscoderSourceToCommand FindataTranscoderRevolut = "parse-revolut"
findataTranscoderSourceToCommand FindataTranscoderSplitwise = "parse-splitwise"

-- | Runs findata-transcoder
findataTranscoder ::
  (MonadError e m, MonadIO m, e ~ Text) =>
  FindataTranscoderSource ->
  Turtle.FilePath ->
  Turtle.FilePath ->
  m ()
findataTranscoder source inputFile outputFile = do
  eitherErrorOrUnit <- liftIO $
    try @Turtle.ExitCode . Turtle.sh $ do
      let findataTranscoderOutput =
            Turtle.inshell
              ( "/home/grzesiek/.local/bin/findata-transcoder "
                  <> findataTranscoderSourceToCommand source
              )
              (input inputFile)
      Turtle.output outputFile findataTranscoderOutput
  whenLeft
    ()
    eitherErrorOrUnit
    (const $ throwError "The findata-transcoder tool has failed.")
