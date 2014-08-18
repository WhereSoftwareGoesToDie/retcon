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
import Control.Exception.Enclosed (tryAny)
import Control.Monad.Error.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson
import Data.Bifunctor
import Data.Either
import Data.Maybe
import Data.Monoid
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
import Retcon.Options

-- | Check that two symbols are the same.
same :: (KnownSymbol a, KnownSymbol b) => Proxy a -> Proxy b -> Bool
same a b = isJust (sameSymbol a b)

-- | Extract the type-level information from a 'ForeignKey'.
--
-- The triple contains the entity, data source, and key in that order.
foreignKeyValue :: forall entity source. (RetconDataSource entity source)
                => ForeignKey entity source
                -> (String, String, String)
foreignKeyValue (ForeignKey key) =
    let entity = symbolVal (Proxy :: Proxy entity)
        source = symbolVal (Proxy :: Proxy source)
    in (entity, source, key)

-- | Encode a 'ForeignKey' as a 'String'.
encodeForeignKey :: forall entity source. (RetconDataSource entity source)
                 => ForeignKey entity source
                 -> String
encodeForeignKey = show . foreignKeyValue

-- | The unique identifier used to identify a unique 'entity' document within
-- retcon.
newtype RetconEntity entity => InternalKey entity =
    InternalKey { unInternalKey :: Int }
  deriving (Eq, Ord, Show)

-- | Extract the type-level information from an 'InternalKey'.
--
-- The pair contains the entity, and the key in that order.
internalKeyValue :: forall entity. RetconEntity entity
                 => InternalKey entity
                 -> (String, Int)
internalKeyValue (InternalKey key) =
    let entity = symbolVal (Proxy :: Proxy entity)
    in (entity, key)

-- | Translate a 'ForeignKey' to an 'InternalKey'
--
-- This involves looking for the specific @entity@, @source@, and 'ForeignKey'
-- in a translation table in the database.
lookupInternalKey :: (RetconDataSource entity source)
                  => ForeignKey entity source
                  -> RetconHandler (Maybe (InternalKey entity))
lookupInternalKey fk = do
    conn <- asks snd
    -- Look for the internal key in the database.

    (results :: [Only Int]) <- liftIO $ query conn "SELECT id FROM retcon_fk WHERE entity = ? AND source = ? AND fk = ? LIMIT 1" $ foreignKeyValue fk
    case results of
      Only key:_ -> return $ Just (InternalKey key)
      []         -> return Nothing
    -- If it exists, return it
    -- Otherwise:
    --     Allocate a new internal key.
    --     Record it in the database.
    --     Return it.

-- | Resolve the 'ForeignKey' associated with an 'InternalKey' for a given data
-- source.
lookupForeignKey :: forall entity source. (RetconDataSource entity source)
                 => InternalKey entity
                 -> RetconHandler (Maybe (ForeignKey entity source))
lookupForeignKey (InternalKey key) = do
    conn <- asks snd

    let entity = symbolVal (Proxy :: Proxy entity)
    let source = symbolVal (Proxy :: Proxy source)

    (results::[Only String]) <- liftIO $ query conn "SELECT fk FROM retcon_fk WHERE entity = ? AND source = ? AND id = ?" (entity, source, key)
    liftIO $ putStrLn (unlines . map show $ results)

    return $ case results of
        Only key:_ -> Just (ForeignKey key :: ForeignKey entity source)
        []         -> Nothing

-- | Parse a request string and handle an event.
dispatch :: String -> RetconHandler ()
dispatch work = do
    let (entity_str, source_str, key) = read work :: (String, String, String)
    entities <- asks (retconEntities . fst)
    case someSymbolVal entity_str of
        SomeSymbol (entity :: Proxy entity_ty) ->
            forM_ entities $ \(SomeEntity e) ->
                when (same e entity) $ forM_ (entitySources e) $ \(SomeDataSource (sp :: Proxy st) :: SomeDataSource et) -> do
                    case someSymbolVal source_str of
                        SomeSymbol (source :: Proxy source_ty) -> do
                          let fk = ForeignKey key :: ForeignKey et st
                          when (same source sp) (process fk)

-- | Run the retcon process on an event.
retcon :: RetconOptions
       -> RetconConfig
       -> Connection
       -> String -- ^ Key to use.
       -> IO (Either RetconError ())
retcon opts config conn key = do
    runRetconHandler opts config conn . dispatch $ key

-- | Process an event on a specified 'ForeignKey'.
--
-- This function is responsible for determining the type of event which has
-- occured and invoking the correct 'RetconDataSource' actions and retcon
-- algorithms to handle it.
process :: forall entity source. (RetconDataSource entity source)
        => ForeignKey entity source
        -> RetconHandler ()
process fk = do
    $logDebug $ T.concat ["EVENT against ", T.pack $ show $ length sources, " sources"]

    -- If we can't find an InternalKey: it's a CREATE.
    -- If we can't get the upstream Document: it's a DELETE.
    -- Otherwise: it's an UPDATE.

    update fk
  where
    sources = entitySources (Proxy :: Proxy entity)


-- | Process a creation event.
create :: (RetconDataSource entity source)
       => ForeignKey entity source
       -> RetconHandler ()
create fk = do
    $logDebug "CREATE"

    -- liftIO $ throwIO $ userError "I am walrus"

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
    ik <- lookupInternalKey fk
    docs <- case ik of
        Nothing  -> return []
        Just ik' -> do
            carefully $ getDocuments ik'
    let valid = rights docs
    liftIO $ print valid

    return ()

-- | Get 'Document's corresponding to an 'InternalKey' for all sources for an
-- entity.
getDocuments :: forall entity. (RetconEntity entity)
             => InternalKey entity
             -> RetconHandler [Either RetconError Document]
getDocuments ik =
    forM (entitySources (Proxy :: Proxy entity)) $
        \(SomeDataSource (Proxy :: Proxy source)) ->
            --
            join . first RetconError <$> tryAny (do
                -- Lookup the foreign key for this data source.
                mkey <- lookupForeignKey ik
                -- If there was a
                case mkey of
                    Just (fk :: ForeignKey entity source) ->
                        liftIO . runDataSourceAction $ getDocument fk
                    Nothing ->
                        return . Left $ RetconFailed)

