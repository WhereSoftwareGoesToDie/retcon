--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

-- | Description: Dispatch events with a retcon configuration.

{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Retcon.Handler where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class ()
import Control.Monad.Reader
import Data.Maybe
import Data.Proxy
import GHC.TypeLits

import Retcon.DataSource
import Retcon.Document

-- | Configuration for the retcon system.
data RetconConfig =
    RetconConfig { retconEntities :: [SomeEntity] }

-- | Monad for the retcon system.
newtype RetconHandler a =
    RetconHandler { unRetconHandler :: ReaderT RetconConfig IO a }
  deriving (Functor, Applicative, Monad, MonadReader RetconConfig, MonadIO)

-- | Run a 'RetconHandler' action with the given configuration.
runRetconHandler :: RetconConfig -> RetconHandler a -> IO a
runRetconHandler cfg (RetconHandler a) = runReaderT a cfg

-- | Check that two symbols are the same.
same :: (KnownSymbol a, KnownSymbol b) => Proxy a -> Proxy b -> Bool
same a b = isJust (sameSymbol a b)

-- | Encode a 'ForeignKey' as a 'String'.
encodeForeignKey :: forall entity source. (KnownSymbol entity, KnownSymbol source)
                 => ForeignKey entity source
                 -> String
encodeForeignKey (ForeignKey key) =
    show ( symbolVal (Proxy :: Proxy entity)
         , symbolVal (Proxy :: Proxy source)
         , key)

-- | Parse a request string and handle an event.
dispatch :: String -> RetconHandler ()
dispatch work = do
    let (entity_str, source_str, key) = (read work :: (String, String, String))
    entities <- asks retconEntities
    case someSymbolVal entity_str of
        SomeSymbol (entity :: Proxy entity_ty) ->
            forM_ entities $ \(SomeEntity e) ->
                if same e entity
                then forM_ (entitySources e) $ \(SomeDataSource (sp :: Proxy st) :: SomeDataSource et) -> do
                    case someSymbolVal source_str of
                        SomeSymbol (source :: Proxy source_ty) -> do
                          let fk = (ForeignKey key :: ForeignKey et st)
                          when (same source sp) (process fk)

                else return ()

-- | Process an event on a specified 'ForeignKey'.
--
-- This function is responsible for determining the type of event which has
-- occured and invoking the correct 'RetconDataSource' actions and retcon
-- algorithms to handle it.
process :: (RetconDataSource entity source)
        => ForeignKey entity source
        -> RetconHandler ()
process fk = do
    doc <- liftIO $ getDocument fk
    liftIO $ putStr "Now handling event on: " >> print fk >> print doc
    return ()

