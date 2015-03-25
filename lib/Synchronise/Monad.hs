--
-- Copyright © 2014-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Synchronise.Monad where

import Control.Applicative
import Control.Exception
import Control.Lens
import Control.Monad.Error.Class
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Data.Typeable

import Synchronise.Configuration
import Synchronise.DataSource

data SynchroniseError
    = SynchroniseFailed
    | SynchroniseDBError String      -- ^ Describes an error with the synchronise database.
    | SynchroniseSourceError DataSourceError -- ^ An error occured with a data source.
    | SynchroniseNotSupported String -- ^ An unsupported configuration.
    | SynchroniseUnknown String      -- ^ Unknown entity, source, or document.
  deriving (Show, Typeable)

instance Exception SynchroniseError

-- | Monad transformer stack used in the 'SynchroniseHandler' monad.
type SynchroniseHandlerStack =
    ReaderT SynchroniseMonadState
            (ExceptT SynchroniseError IO)

-- | Product type wrapper for state components.
data SynchroniseMonadState = SynchroniseMonadState
    { _synchroniseConfig :: Configuration
    }
makeLenses ''SynchroniseMonadState

-- | Monad for the Synchronise system.
--
-- This monad provides access to an environment containing the state
-- of the synchronise system and the state of the current action; error
-- handling through exceptions; and I/O.
newtype SynchroniseMonad a =
    SynchroniseMonad {
        unSynchroniseMonad :: SynchroniseHandlerStack a
    }
  deriving (Functor, Applicative, Monad, MonadIO,
            MonadReader (SynchroniseMonadState),
            MonadError SynchroniseError)

-- | Execute an action in the 'SynchroniseMonad' monad.
runSynchroniseMonad :: SynchroniseMonadState
               -> SynchroniseMonad a
               -> IO (Either SynchroniseError a)
runSynchroniseMonad state =
    runExceptT .
    flip runReaderT state . unSynchroniseMonad
