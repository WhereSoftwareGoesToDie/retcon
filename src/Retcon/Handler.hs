--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

-- | Description: Dispatch events with a retcon configuration.

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Retcon.Handler where

import Control.Applicative
import Control.Exception
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Maybe
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import GHC.TypeLits

import Retcon.Config
import Retcon.DataSource
import Retcon.Document
import Retcon.Error
import Retcon.Monad

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
    -- Look for the internal key in the database.
    -- If it exists, return it.
    -- Otherwise:
    --     Allocate a new internal key.
    --     Record it in the database.
    --     Return it.
    return Nothing

-- | Resolve the 'ForeignKey' associated with an 'InternalKey' for a given data
-- source.
lookupForeignKey :: forall entity source. (RetconDataSource entity source)
                 => InternalKey entity
                 -> RetconHandler (ForeignKey entity source)
lookupForeignKey key = do
    let fk = "123"
    return (ForeignKey fk :: ForeignKey entity source)

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

-- | Run the retcon process on an event.
retcon :: RetconConfig
       -> Connection
       -> String -- ^ Key to use.
       -> IO (Either RetconError ())
retcon config conn key = do
    runRetconHandler config conn $ dispatch $ key

-- | Process an event on a specified 'ForeignKey'.
--
-- This function is responsible for determining the type of event which has
-- occured and invoking the correct 'RetconDataSource' actions and retcon
-- algorithms to handle it.
process :: forall entity source. (RetconDataSource entity source)
        => ForeignKey entity source
        -> RetconHandler ()
process fk = do
    $logDebug $ T.concat ["EVENT against ", (T.pack $ show $ length sources), " sources"]

    -- If we can't find an InternalKey: it's a CREATE.
    key <- lookupInternalKey fk

    (lookupInternalKey fk >> (carefully $ create fk))

    $logDebug "PROCESS 1"

    -- If we can't get the upstream Document: it's a DELETE.

    $logDebug "PROCESS 2"

    -- Otherwise: it's an UPDATE.
    update fk

    $logDebug "PROCESS 3"
  where
    sources = entitySources (Proxy :: Proxy entity)


-- | Process a creation event.
create :: (RetconDataSource entity source)
       => ForeignKey entity source
       -> RetconHandler ()
create fk = do
    $logDebug "CREATE"

    liftIO $ throwIO $ userError "I am walrus"

-- | Process a deletion event.
delete :: (RetconDataSource entity source)
       => ForeignKey entity source
       -> RetconHandler ()
delete fk = do
    $logDebug "DELETE"

-- | Process an update event.
update :: (RetconDataSource entity source)
       => ForeignKey entity source
       -> RetconHandler ()
update fk = do
    $logDebug "UPDATE"
    key <- lookupInternalKey fk
    return ()

getDocuments :: forall entity. (RetconEntity entity)
             => InternalKey entity
             -> [SomeDataSource entity]
             -> RetconHandler [Either SomeException Document]
getDocuments key = undefined -- sequence . map (tryAny . (lookupForeignKey key >>= flip getDocument))

