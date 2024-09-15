-- | This module handles using findata-transcoder.
module FindataTranscoder (
  findataTranscoder,
  FindataTranscoderSource (..),
) where

import Control.Exception.Extra (failIO)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Turtle qualified

data FindataTranscoderSource
  = FindataTranscoderBcge
  | FindataTranscoderBcgeCc
  | FindataTranscoderCoop
  | FindataTranscoderCsBrokerageAccount
  | FindataTranscoderCsEacAccount
  | FindataTranscoderDegiroAccount
  | FindataTranscoderDegiroPortfolio
  | FindataTranscoderFinpension
  | FindataTranscoderGalaxus
  | FindataTranscoderGooglePlay
  | FindataTranscoderGPayslip
  | FindataTranscoderIBActivity
  | FindataTranscoderMBank
  | FindataTranscoderPatreon
  | FindataTranscoderRevolut
  | FindataTranscoderSplitwise
  | FindataTranscoderUberEats

findataTranscoderSourceToCommand :: FindataTranscoderSource -> Text
findataTranscoderSourceToCommand FindataTranscoderBcge = "parse-bcge"
findataTranscoderSourceToCommand FindataTranscoderBcgeCc = "parse-bcge-cc"
findataTranscoderSourceToCommand FindataTranscoderCoop = "parse-coop"
findataTranscoderSourceToCommand FindataTranscoderCsBrokerageAccount = "parse-cs-brokerage-account-history"
findataTranscoderSourceToCommand FindataTranscoderCsEacAccount = "parse-cs-eac-history"
findataTranscoderSourceToCommand FindataTranscoderDegiroAccount = "parse-degiro-account"
findataTranscoderSourceToCommand FindataTranscoderDegiroPortfolio = "parse-degiro-portfolio"
findataTranscoderSourceToCommand FindataTranscoderFinpension = "parse-finpension"
findataTranscoderSourceToCommand FindataTranscoderGalaxus = "parse-galaxus"
findataTranscoderSourceToCommand FindataTranscoderGooglePlay = "parse-google-play"
findataTranscoderSourceToCommand FindataTranscoderGPayslip = "parse-gpayslip"
findataTranscoderSourceToCommand FindataTranscoderIBActivity = "parse-ib-activity"
findataTranscoderSourceToCommand FindataTranscoderMBank = "parse-mbank"
findataTranscoderSourceToCommand FindataTranscoderPatreon = "parse-patreon"
findataTranscoderSourceToCommand FindataTranscoderRevolut = "parse-revolut"
findataTranscoderSourceToCommand FindataTranscoderSplitwise = "parse-splitwise"
findataTranscoderSourceToCommand FindataTranscoderUberEats = "parse-uber-eats"

-- | Runs findata-transcoder.
--
-- Throws 'userError' on failure.
findataTranscoder ::
  (MonadIO m) =>
  FindataTranscoderSource ->
  -- | Text input.
  Turtle.Shell Turtle.Line ->
  m Text
findataTranscoder source inputContent = do
  let ftPath = "/Users/grzesiek/.local/bin/findata-transcoder"
  (exitCode, txt) <- Turtle.procStrict ftPath [findataTranscoderSourceToCommand source] inputContent
  case exitCode of
    Turtle.ExitSuccess -> return txt
    -- Do not add a newline to the error message, because IOException's Show instance wraps the
    -- message in parantheses, e.g., "user error (findata-transcoder has failed)".
    Turtle.ExitFailure _ -> failIO "findata-transcoder has failed."
