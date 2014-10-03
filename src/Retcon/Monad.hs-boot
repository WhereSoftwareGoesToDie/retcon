--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

-- | Description: Monad for retcon handlers.
--
-- This module implements the monad in which the main "handler" actions
-- execute. This monad provides the following effects:
--
-- - Error handling
-- - Logging
-- - Accessing configuration
-- - I/O

{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Retcon.Monad (
    RetconMonad,
    RetconState,
    retconStore,
    carefully,
    getRetconState,
    getActionState,
    ) where

import Control.Applicative
import Control.Exception.Enclosed
import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control
import qualified Control.Monad.Trans.Reader as ReaderT (ask, local, reader)

import Retcon.Error
import Retcon.Options

data RetconState s

retconStore :: RetconState s -> s

type RetconHandlerStack s l = ReaderT (RetconState s, l) (ExceptT RetconError (LoggingT IO))

-- | Monad for the retcon system.
--
-- This monad provides access to an environment containing the "global" state
-- of the retcon system and the "local" state of the current action; error
-- handling through exceptions; logging; and I/O.
newtype RetconMonad s l a =
    RetconMonad {
        unRetconMonad :: (RetconHandlerStack s l) a
    }

instance Functor (RetconMonad s l)

instance Applicative (RetconMonad s l)

instance Monad (RetconMonad s l)

instance MonadIO (RetconMonad s l)

instance MonadLogger (RetconMonad s l)

instance MonadReader (RetconState s, l) (RetconMonad s l)

instance MonadError RetconError (RetconMonad s l)

instance MonadBase IO (RetconMonad s l)

carefully :: RetconMonad s l a -> RetconMonad s l a

getRetconState :: RetconMonad s l (RetconState s)

getActionState :: RetconMonad s l l

