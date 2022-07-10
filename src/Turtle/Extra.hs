module Turtle.Extra (
  PosixLine,
  posixLineToText,
  posixLineToLine,
  lineToPosixLine,
  posixLinesToText,
  decodePathM,
  emptyLine,
  textToShell,
) where

import Control.Monad.Except (MonadError (throwError))
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Turtle

-- | A line of text (does not contain newlines).
--
-- It's like 'Turtle.Line' except it adheres better to POSIX line principles.
newtype PosixLine = PosixLine Text
  deriving newtype (Eq, Ord, Show)

posixLineToText :: PosixLine -> Text
posixLineToText (PosixLine l) = l <> "\n"

posixLineToLine :: PosixLine -> Turtle.Line
posixLineToLine (PosixLine l) = Turtle.unsafeTextToLine l

lineToPosixLine :: Turtle.Line -> PosixLine
lineToPosixLine = PosixLine . Turtle.lineToText

textToPosixLines :: Text -> [PosixLine]
textToPosixLines = fmap (lineToPosixLine . Turtle.unsafeTextToLine) . T.lines

posixLinesToText :: (Foldable f) => f PosixLine -> Text
posixLinesToText = foldMap posixLineToText

emptyLine :: NonEmpty Turtle.Line
emptyLine = Turtle.textToLines ""

decodePathM :: (MonadError e m, e ~ Text) => Turtle.FilePath -> m Text
decodePathM path =
  either
    (\e -> throwError $ "Could not decode the path: " <> e <> ".\n")
    return
    (Turtle.toText path)

textToShell :: Text -> Turtle.Shell PosixLine
textToShell = Turtle.select . textToPosixLines
