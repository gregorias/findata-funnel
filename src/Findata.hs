-- | Utilities related to Findata tools.
module Findata (ledgersort) where

import GHC.Base (failIO)
import Relude (MonadIO (liftIO), fromString)
import Turtle qualified
import Wallet (getWallet)

-- | Runs ledgersort on the wallet.
ledgersort :: (MonadIO m) => m ()
ledgersort = do
  wallet <- getWallet
  (exitCode, _) <- Turtle.shellStrict ("ledgersort < " <> fromString wallet <> " | sponge " <> fromString wallet) mempty
  if exitCode == Turtle.ExitSuccess
    then pure ()
    else liftIO $ failIO "Ledgersort failed"
