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
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Retcon.Handler where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class ()
import Control.Monad.Reader
import Data.Maybe
import Data.Proxy
import Database.PostgreSQL.Simple
import GHC.TypeLits

import Retcon.DataSource
import Retcon.Document

-- | Configuration for the retcon system.
data RetconConfig =
    RetconConfig { retconEntities :: [SomeEntity] }

-- | Monad for the retcon system.
newtype RetconHandler a =
    RetconHandler { unRetconHandler :: ReaderT (RetconConfig,Connection) IO a }
  deriving (Functor, Applicative, Monad, MonadReader (RetconConfig,Connection), MonadIO)

-- | Run a 'RetconHandler' action with the given configuration.
runRetconHandler :: RetconConfig -> Connection -> RetconHandler a -> IO a
runRetconHandler cfg conn (RetconHandler a) = runReaderT a (cfg,conn)

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

-- | The unique identifier used to identify a unique 'entity' document within
-- retcon.
newtype RetconEntity entity => InternalKey entity =
    InternalKey { unInternalKey :: String }
  deriving (Eq, Ord, Show)

-- | Translate a 'ForeignKey' to an 'InternalKey'
--
-- This involves looking for the specific @entity@, @source@, and 'ForeignKey'
-- in a translation table in the database.
lookupInternalKey :: (RetconDataSource entity source)
                  => ForeignKey entity source
              -> RetconHandler (Maybe (InternalKey entity))
lookupInternalKey _fk = do
    conn <- asks snd
    (res :: [Only String]) <- liftIO $ query conn "SELECT CONCAT(?,?)" ("lol"::String, "wut"::String)
    -- Look for the internal key in the database.
    -- If it exists, return it.
    -- Otherwise:
    --     Allocate a new internal key.
    --     Record it in the database.
    --     Return it.
    return Nothing

-- | Parse a request string and handle an event.
dispatch :: String -> RetconHandler ()
dispatch work = do
    let (entity_str, source_str, key) = (read work :: (String, String, String))
    entities <- asks (retconEntities . fst)
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
    liftIO $ putStr "EVENT\t" >> print fk

    -- If we can't find an InternalKey: it's a CREATE.
    key <- lookupInternalKey fk
    case key of
        Nothing -> create fk
        Just _  -> return ()

    -- If we can't find a Document: it's a DELETE.
    doc <- liftIO $ catch (getDocument fk >>= return . Just)
                          (\(_ :: IOException) -> return Nothing)
    case doc of
        Nothing -> delete fk
        Just _  -> return ()

    -- Otherwise: it's an UPDATE.
    when (isJust key && isJust doc) $ update fk

-- | Process a creation event.
create :: (RetconDataSource entity source)
       => ForeignKey entity source
       -> RetconHandler ()
create fk = do
    liftIO $ putStr "\tCREATE\t"
    liftIO $ print fk

-- | Process a deletion event.
delete :: (RetconDataSource entity source)
       => ForeignKey entity source
       -> RetconHandler ()
delete fk = do
    liftIO $ putStr "\tDELETE\t"
    liftIO $ print fk

-- | Process an update event.
update :: (RetconDataSource entity source)
       => ForeignKey entity source
       -> RetconHandler ()
update fk = do
    key <- lookupInternalKey fk
    liftIO $ putStr "\tUPDATE\t"
    liftIO $ print key

