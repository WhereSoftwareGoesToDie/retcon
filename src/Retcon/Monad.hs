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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Retcon.Monad where

import Control.Applicative
import Control.Exception
import Control.Exception.Enclosed
import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Database.PostgreSQL.Simple

import Retcon.Config

-- * Errors

-- | Exceptions which can be raised by actions in the RetconHandler monad.
data HandlerError =
      HandlerFailed --
    | HandlerError SomeException --
  deriving (Show)

-- | Run an action in 'RetconHandler', catching any exceptions and propagating them
-- as 'HandlerError's.
carefully :: RetconHandler a -> RetconHandler a
carefully = handleAny (\e -> throwError $ HandlerError e)

-- * Monad

-- $ Retcon handlers (i.e. actions which process and response to an event) are
-- actions in the 'RetconHandler' monad.

-- | The retcon handler monad has a bunch of effects. Here they are!
type RetconHandlerStack = ExceptT HandlerError (LoggingT (ReaderT (RetconConfig, Connection) IO))

-- | Monad for the retcon system.
--
-- This monad provides error handling (by throwing 'HandlerLog' exceptions),
-- logging (write 'HandlerLog'), access to configuration and PostgreSQL
-- connections (reader), and IO facilities.
newtype RetconHandler a =
    RetconHandler {
        unRetconHandler :: RetconHandlerStack a
    }
  deriving (Functor, Applicative, Monad, MonadIO, MonadLogger,
  MonadReader (RetconConfig,Connection), MonadError HandlerError, MonadBase IO)

-- | MonadBaseControl used to catch IO exceptions
instance MonadBaseControl IO RetconHandler where
    newtype StM RetconHandler a = StHandler {
        unStHandler :: StM RetconHandlerStack a
    }

    -- Unwrap the bits in our monad to get to the base.
    liftBaseWith f = RetconHandler . liftBaseWith $ \r ->
        f $ liftM StHandler . r . unRetconHandler

    -- Wrap things back up into our monad.
    restoreM       = RetconHandler . restoreM . unStHandler

-- | Run a 'RetconHandler' action with the given configuration.
runRetconHandler :: RetconConfig
                 -> Connection
                 -> RetconHandler a
                 -> IO (Either HandlerError a)
runRetconHandler cfg conn (RetconHandler a) =
    flip runReaderT (cfg,conn) $ runStderrLoggingT $ runExceptT a


