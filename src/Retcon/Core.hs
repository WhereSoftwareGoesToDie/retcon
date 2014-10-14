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
    runRetconAction,
    runRetconMonadOnce,
) where

import Control.Applicative
import Control.Exception.Enclosed
import Control.Exception
import Control.Lens.Operators
import Control.Lens(view)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Bifunctor

import Retcon.Error
import Retcon.Monad
import Retcon.DataSource
import Retcon.Options

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
    RetconMonad . withReaderT (localise l) . unRetconMonad .
    -- Do exception and error handling.
    handle . handleAny (throwError . RetconError)
  where
    -- | Handle any errors in an action, pulling them into the monad.
    handle :: (Functor m, MonadError e m) => m v -> m (Either e v)
    handle a = (Right <$> a) `catchError` (return . Left)

-- | "Localise" the state as appropriate to run a 'RetconAction' in a 'RetconHandler' context.
localise
    :: StoreToken s
    => l -- ^ Local state value
    -> RetconMonadState e s () -- ^ Handler state
    -> RetconMonadState e ROToken l
localise l (RetconMonadState opt state store local) = RetconMonadState opt state (restrictToken store) l

-- | Execute an action in the 'RetconMonad' monad with configuration
-- initialized and finalized.
runRetconMonadOnce
    :: RetconOptions
    -> RetconConfig
    -> s
    -> l
    -> RetconMonad InitialisedEntity s l a
    -> IO (Either RetconError a)
runRetconMonadOnce opt (RetconConfig entities) store l action =
    let params = pickParams opt
        stater = \inited -> RetconMonadState opt inited store l
    in bracket (initialiseEntities params entities)
               (void . finaliseEntities params)
               (\state -> runRetconMonad opt (stater state) action)
