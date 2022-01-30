-- | This module handles using hledupt.
module Hledupt (
  hledupt,
  HleduptSource (..),
) where

import Control.Exception (try)
import Control.Monad.Except (MonadError (throwError))
import Relude
import Turtle (input)
import qualified Turtle

data HleduptSource
  = HleduptPatreon
  | HleduptRevolut

hleduptSourceToCommand :: HleduptSource -> Text
hleduptSourceToCommand HleduptPatreon = "parse-patreon"
hleduptSourceToCommand HleduptRevolut = "parse-revolut"

-- | Runs hledupt
hledupt :: (MonadError e m, MonadIO m, e ~ Text) => HleduptSource -> Turtle.FilePath -> Turtle.FilePath -> m ()
hledupt source inputFile outputFile = do
  eitherErrorOrUnit <- liftIO $
    try @Turtle.ExitCode . Turtle.sh $ do
      let hleduptOutput = Turtle.inshell ("hledupt " <> hleduptSourceToCommand source) (input inputFile)
      Turtle.output outputFile hleduptOutput
  whenLeft
    ()
    eitherErrorOrUnit
    (const $ throwError "The hledupt tool has failed.")
