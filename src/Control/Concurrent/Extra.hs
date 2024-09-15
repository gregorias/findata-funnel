module Control.Concurrent.Extra (
  threadDelay,
) where

import Control.Concurrent qualified
import Data.Time.Clock (DiffTime, diffTimeToPicoseconds)
import Relude ((&))

threadDelay :: DiffTime -> IO ()
threadDelay delay = fromInteger (diffTimeToPicoseconds delay `div` 1_000_000) & Control.Concurrent.threadDelay
