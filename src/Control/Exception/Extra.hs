module Control.Exception.Extra (
  failIO,
  eitherToIO,
) where

import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text)
import qualified Data.Text as T

-- | Throws a 'UserError' using the provided message.
--
-- It is the same function as 'Turtle.die' but I think it has a more apt name.
-- It is like 'fail' but it doesn't require 'MonadFail' but 'MonadIO'
failIO :: (MonadIO io) => Text -> io a
failIO = liftIO . throwIO . userError . T.unpack

-- | Throws an exception on error.
--
-- 'ExceptT IO' is an anti-pattern but so can be 'IO (Either l r)'. This
-- function translates eithers to exceptions.
eitherToIO :: (MonadIO io, Exception e) => (l -> e) -> Either l r -> io r
eitherToIO f = either (liftIO . throwIO . f) return
