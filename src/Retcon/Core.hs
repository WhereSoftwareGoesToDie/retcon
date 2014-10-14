--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

module Retcon.Core
(
    runRetconAction
) where

import Control.Applicative
import Control.Exception.Enclosed
import Control.Lens.Operators
import Control.Lens(view)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Bifunctor

import Retcon.Error
import Retcon.Monad
import Retcon.Store

-- | State held in the reader part of the 'RetconHandler' monad.
data RetconState s = RetconState
    { _retconOptions :: RetconOptions
    , _retconState   :: [InitialisedEntity]
    , _retconStore   :: s
    }
makeLenses ''RetconState

-- * Action monad

type RetconAction l a = RetconMonad ROToken l a

    st = RetconState opt state store

-- | Run an action in the 'RetconAction' monad (aka the 'RetconMonad' with
-- read-only storage).
--
-- Errors which occur in the action are handled and will not propagate up into
-- the parent handler.
runRetconAction :: StoreToken s
                => l
                -> RetconAction l a
                -> RetconHandler s (Either RetconError a)
runRetconAction l =
    -- Restrict store and add the local state.
    RetconMonad . withReaderT localise . unRetconMonad .
    -- Do exception and error handling.
    handle . handleAny (throwError . RetconError)
  where
    localise = bimap (retconStore %~ restrictToken) (const l)

    -- | Handle any errors in an action, pulling them into the monad.
    handle :: (Functor m, MonadError e m) => m v -> m (Either e v)
    handle a = (Right <$> a) `catchError` (return . Left)

-- | Execute an action in the 'RetconMonad' monad with configuration
-- initialized and finalized.
runRetconMonadOnce
    :: RetconOptions
    -> RetconConfig
    -> s
    -> l
    -> RetconMonad s l a
    -> IO (Either RetconError a)
runRetconMonadOnce opt (RetconConfig entities) store l action =
    let params = pickParams opt
    in bracket (initialiseEntities params entities)
               (void . finaliseEntities params)
               (\state -> runRetconMonad opt state store l action)

-- | Get the retcon component of the environment.
getRetconState :: RetconMonad s l (RetconState s)
getRetconState = asks _globalState

-- | Get the action-specific component of the environment.
getActionState :: RetconMonad s l l
getActionState = asks _localState

-- | Do something when the verbose option is set
whenVerbose :: (MonadReader (RetconMonadState (RetconState s) x) m) => m () -> m ()
whenVerbose f = do
    verbose <- view (globalState . retconOptions . optVerbose)
    when verbose f
