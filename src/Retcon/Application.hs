module Retcon.Application where

import Control.Monad.Reader

import Retcon.DataSource
import Retcon.Error
import Retcon.Monad
import Retcon.Options
import Retcon.Store

-- | Top-level "handlers" operate with no "local" state and an arbitrary store.
type RetconHandler s a = RetconMonad s () a

-- | Run an action in the 'RetconHandler' monad (aka the 'RetconMonad' with
-- '()' local state).
runRetconHandler :: RetconOptions
                 -> [InitialisedEntity]
                 -> s
                 -> RetconHandler s a
                 -> IO (Either RetconError a)
runRetconHandler opt state store = runRetconMonad opt state store ()

-- | Bottom-level "actions" operate with a "local" state and a read-only store.
type RetconAction l a = RetconMonad ROToken l a

-- | Run an action in the 'RetconAction' monad (aka the 'RetconMonad' with
-- read-only storage).
runRetconAction :: StoreToken s
                => l
                -> RetconAction l a
                -> RetconHandler s a
runRetconAction l = RetconMonad . withReaderT (localise). unRetconMonad
  where
    localise (st, _) =
        let st' = st { retconStore = restrictToken $ retconStore st }
        in (st', l)

