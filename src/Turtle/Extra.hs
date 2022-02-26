module Turtle.Extra (
  decodePathM,
  emptyLine,
) where

import Control.Monad.Except (MonadError (throwError))
import Relude
import qualified Turtle

emptyLine :: NonEmpty Turtle.Line
emptyLine = Turtle.textToLines ""

decodePathM :: (MonadError e m, e ~ Text) => Turtle.FilePath -> m Text
decodePathM path =
  either
    (\e -> throwError $ "Could not decode the path: " <> e <> ".\n")
    return
    (Turtle.toText path)
