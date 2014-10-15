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
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Retcon.Monad (
    -- * Retcon monad

    -- $ Retcon handlers (i.e. actions which process and response to an event)
    -- are actions in the 'RetconHandler' monad.
    RetconMonad(..),

    RetconMonadState(..),
    retconOptions,
    retconState,
    retconStore,
    localState,

    getRetconState,
    getActionState,
    whenVerbose,

    -- ** Evaluators
    runRetconMonad,
) where

import Control.Applicative
import Control.Lens
import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control

import Retcon.Error
import Retcon.Options

-- | Monad transformer stack used in the 'RetconHandler' monad.
type RetconHandlerStack entity store local_state =
    ReaderT (RetconMonadState entity store local_state)
            (ExceptT RetconError (LoggingT IO))

-- | Product type wrapper for global and local state components. This is
-- instantiated in Core.hs
data RetconMonadState entity store local = RetconMonadState
    { _retconOptions :: RetconOptions
    , _retconState   :: [entity]
    , _retconStore   :: store
    , _localState    :: local
    }
makeLenses ''RetconMonadState

-- | Monad for the retcon system.
--
-- This monad provides access to an environment containing the "global" state
-- of the retcon system and the "local" state of the current action; error
-- handling through exceptions; logging; and I/O.
newtype RetconMonad entity store local_state a =
    RetconMonad {
        unRetconMonad :: (RetconHandlerStack entity store local_state) a
    }
  deriving (Functor, Applicative, Monad, MonadIO, MonadLogger,
            MonadReader (RetconMonadState entity store local_state),
            MonadError RetconError, MonadBase IO)

-- | MonadBaseControl used to catch IO exceptions
instance MonadBaseControl IO (RetconMonad e s l) where
    newtype StM (RetconMonad e s l) a = StHandler {
        unStHandler :: StM (RetconHandlerStack e s l) a
    }

    -- Unwrap the bits in our monad to get to the base.
    liftBaseWith f = RetconMonad . liftBaseWith $ \r ->
        f $ liftM StHandler . r . unRetconMonad

    -- Wrap things back up into our monad.
    restoreM       = RetconMonad . restoreM . unStHandler

-- | Execute an action in the 'RetconMonad' monad.
runRetconMonad :: RetconOptions
               -> RetconMonadState entity store local_state
               -> RetconMonad entity store local_state a
               -> IO (Either RetconError a)
runRetconMonad opt state =
    runLogging .
    runExceptT .
    flip runReaderT state . unRetconMonad
  where
    runLogging = case opt ^. optLogging of
      LogStderr -> runStderrLoggingT
      LogStdout -> runStdoutLoggingT
      LogNone   -> (`runLoggingT` \_ _ _ _ -> return ())

-- | Get the retcon component of the environment.
getRetconState :: RetconMonad e s l [e]
getRetconState = view retconState

-- | Get the action-specific component of the environment.
getActionState :: RetconMonad e s l l
getActionState = view localState

-- | Do something when the verbose option is set
whenVerbose :: (MonadReader (RetconMonadState e s x) m) => m () -> m ()
whenVerbose f = do
    verbose <- view (retconOptions . optVerbose)
    when verbose f
