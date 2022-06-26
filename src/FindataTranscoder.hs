-- | This module handles using findata-transcoder.
module FindataTranscoder (
  findataTranscoder,
  FindataTranscoderSource (..),
) where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import qualified Turtle

data FindataTranscoderSource
  = FindataTranscoderBcgeCc
  | FindataTranscoderDegiroAccount
  | FindataTranscoderDegiroPortfolio
  | FindataTranscoderGalaxus
  | FindataTranscoderGPayslip
  | FindataTranscoderPatreon
  | FindataTranscoderRevolut
  | FindataTranscoderSplitwise
  | FindataTranscoderUberEats

findataTranscoderSourceToCommand :: FindataTranscoderSource -> Text
findataTranscoderSourceToCommand FindataTranscoderBcgeCc = "parse-bcge-cc"
findataTranscoderSourceToCommand FindataTranscoderDegiroAccount = "parse-degiro-account"
findataTranscoderSourceToCommand FindataTranscoderDegiroPortfolio = "parse-degiro-portfolio"
findataTranscoderSourceToCommand FindataTranscoderGalaxus = "parse-galaxus"
findataTranscoderSourceToCommand FindataTranscoderGPayslip = "parse-gpayslip"
findataTranscoderSourceToCommand FindataTranscoderPatreon = "parse-patreon"
findataTranscoderSourceToCommand FindataTranscoderRevolut = "parse-revolut"
findataTranscoderSourceToCommand FindataTranscoderSplitwise = "parse-splitwise"
findataTranscoderSourceToCommand FindataTranscoderUberEats = "parse-uber-eats"

-- | Runs findata-transcoder.
findataTranscoder ::
  (MonadError e m, MonadIO m, e ~ Text) =>
  FindataTranscoderSource ->
  -- | Text input.
  Turtle.Shell Turtle.Line ->
  m Text
findataTranscoder source inputContent = do
  let ftPath = "/Users/grzesiek/.local/bin/findata-transcoder"
  (exitCode, txt) <- Turtle.procStrict ftPath [findataTranscoderSourceToCommand source] inputContent
  case exitCode of
    Turtle.ExitSuccess -> return txt
    Turtle.ExitFailure _ -> throwError "findata-transcoder has failed.\n"
