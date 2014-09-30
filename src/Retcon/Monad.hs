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
import Control.Exception.Enclosed
import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control
import qualified Control.Monad.Trans.Reader as ReaderT (ask, local, reader)

import Retcon.Config
import Retcon.DataSource
import Retcon.Error
import Retcon.Options

-- | Run an action in 'RetconHandler', catching any exceptions and propagating
-- them as 'RetconError's.
carefully :: RetconMonad s l a -> RetconMonad s l a
carefully = handleAny (throwError . RetconError)

-- * Handler monad

-- $ Retcon handlers (i.e. actions which process and response to an event) are
-- actions in the 'RetconHandler' monad.

-- | State held in the reader part of the 'RetconHandler' monad.
data RetconState s = RetconState
    { retconOptions :: RetconOptions
    , retconState   :: [InitialisedEntity]
    , retconStore   :: s
    }

-- | Monad transformer stack used in the 'RetconHandler' monad.
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
  deriving (Functor, Applicative, Monad, MonadIO, MonadLogger,
  MonadReader (RetconState s, l), MonadError RetconError, MonadBase IO)

-- | Get the retcon component of the environment.
getRetconState :: RetconMonad s l (RetconState s)
getRetconState = asks fst

-- | Get the action-specific component of the environment.
getActionState :: RetconMonad s l l
getActionState = asks snd

-- | Execute an action in the 'RetconMonad' monad.
runRetconMonad :: RetconOptions
               -> [InitialisedEntity]
               -> s
               -> l
               -> RetconMonad s l a
               -> IO (Either RetconError a)
runRetconMonad opt state store l =
    runLogging . runExceptT . flip runReaderT (st, l) . unRetconMonad
  where
    st = RetconState opt state store
    runLogging = case optLogging opt of
      LogStderr -> runStderrLoggingT
      LogStdout -> runStdoutLoggingT
      LogNone   -> (`runLoggingT` \_ _ _ _ -> return ())

runRetconMonad' opt conf store l = do
    let state = []
    runRetconMonad opt state store l

-- | MonadBaseControl used to catch IO exceptions
instance MonadBaseControl IO (RetconMonad s l) where
    newtype StM (RetconMonad s l) a = StHandler {
        unStHandler :: StM (RetconHandlerStack s l) a
    }

    -- Unwrap the bits in our monad to get to the base.
    liftBaseWith f = RetconMonad . liftBaseWith $ \r ->
        f $ liftM StHandler . r . unRetconMonad

    -- Wrap things back up into our monad.
    restoreM       = RetconMonad . restoreM . unStHandler

