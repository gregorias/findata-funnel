-- | This module handles using findata-transcoder.
module FindataTranscoder (
  findataTranscoder,
  FindataTranscoderSource (..),
) where

import Control.Exception.Extra (failIO)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import qualified Turtle

data FindataTranscoderSource
  = FindataTranscoderBcge
  | FindataTranscoderBcgeCc
  | FindataTranscoderCs
  | FindataTranscoderDegiroAccount
  | FindataTranscoderDegiroPortfolio
  | FindataTranscoderFinpensionPortfolioTotals
  | FindataTranscoderGalaxus
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
findataTranscoderSourceToCommand FindataTranscoderCs = "parse-cs"
findataTranscoderSourceToCommand FindataTranscoderDegiroAccount = "parse-degiro-account"
findataTranscoderSourceToCommand FindataTranscoderDegiroPortfolio = "parse-degiro-portfolio"
findataTranscoderSourceToCommand FindataTranscoderFinpensionPortfolioTotals = "parse-finpension-portfolio-total"
findataTranscoderSourceToCommand FindataTranscoderGalaxus = "parse-galaxus"
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
    Turtle.ExitFailure _ -> failIO "findata-transcoder has failed.\n"
