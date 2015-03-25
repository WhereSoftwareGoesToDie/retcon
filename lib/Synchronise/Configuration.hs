--
-- Copyright © 2014-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- | Description: Configuration for the system.
module Synchronise.Configuration where

import Control.Applicative
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Configurator as C
import Data.Configurator.Types
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import System.Log.Logger

import Synchronise.Identifier

-- | Command template.
newtype Command = Command { unCommand :: Text }
  deriving (Eq, Show, Ord)

instance IsString Command where
    fromString = Command . T.pack

-- | Record describing an external data source and how we interact with it.
data DataSource = DataSource
    { sourceEntity      :: EntityName -- ^ Unique name for entity.
    , sourceName        :: SourceName -- ^ Unique name for this data source.
    , sourceDescription :: Maybe Text -- ^ Description for this data source.
    , commandCreate     :: Command    -- ^ Command template: create object.
    , commandRead       :: Command    -- ^ Command template: read object.
    , commandUpdate     :: Command    -- ^ Command template: update object.
    , commandDelete     :: Command    -- ^ Command template: delete object.
    }
  deriving (Eq, Ord, Show)

instance Synchronisable DataSource where
    getEntityName = sourceEntity
    getSourceName = sourceName

data Entity = Entity
    { entityName        :: EntityName
    , entityDescription :: Maybe Text
    , entitySchema      :: Maybe FilePath
    , entityPolicy      :: Maybe FilePath
    , entitySources     :: Map SourceName DataSource
    }
  deriving (Eq, Ord, Show)

-- | Construct an 'Entity' with only a name.
emptyEntity
    :: Text
    -> Entity
emptyEntity name = Entity (EntityName name) mempty mempty mempty mempty

-- | Configuration of entities and data sources.
data Configuration = Configuration
    { configEntities :: Map EntityName Entity
    , configServer   :: (String, Priority)
    }
  deriving (Eq, Show)

type Parser a = Config -> ExceptT Text IO a

-- | Parse a configurator 'Config' value into a 'Configuration'.
parseConfiguration
    :: Parser Configuration
parseConfiguration cfg = Configuration
    <$> entities cfg
    <*> server cfg
  where
    entities :: Parser (Map EntityName Entity)
    entities _ = do
        enabled <- liftIO $ C.lookup cfg "entities.enabled"
        ents <- case enabled of
            Nothing -> throwError "No entities enabled in configuration."
            Just [] -> throwError "No entities enabled in configuration."
            Just es -> mapM (`parseEntity` cfg) es
        return . M.fromList . fmap (\e -> (entityName e, e)) $ ents
    server :: Parser (String, Priority)
    server _ =
        (,) <$> liftIO (C.require cfg "server.listen")
            <*> liftIO (read <$> C.require cfg "server.log-level")

-- | Parse an
parseEntity
    :: Text
    -> Parser Entity
parseEntity name cfg' = do
    let cfg = C.subconfig ("entities." <> name) cfg'
    Entity
        <$> parseName cfg
        <*> parseDescription cfg
        <*> parsePath "schema" cfg
        <*> parsePath "merge-policy" cfg
        <*> parseSources cfg
  where
    parseName :: Parser EntityName
    parseName _ = EntityName <$> pure name
    parseDescription :: Parser (Maybe Text)
    parseDescription cfg = liftIO $ C.lookup cfg (name <> ".description")
    parsePath n cfg = liftIO $ C.lookup cfg (name <> "." <> n)
    parseSources :: Parser (Map SourceName DataSource)
    parseSources cfg = do
        enabled <- liftIO $ C.lookup cfg "enabled"
        sources <- case enabled of
            Nothing -> throwError $ "No sources enabled in " <> name
            Just [] -> throwError $ "No sources enabled in " <> name
            Just ss -> mapM (`parseDataSource` cfg) . fmap (name,) $ ss
        return . M.fromList $ sources

-- | Parse a data source from a configuration.
parseDataSource
    :: (Text, Text)
    -> Parser (SourceName, DataSource)
parseDataSource (entity_name, source_name) cfg' = do
    let name = source_name
    let cfg = C.subconfig name cfg'

    let en = EntityName entity_name
    let sn = SourceName source_name
    desc <- liftIO $ C.lookup cfg "description"

    c_cmd <- get_cmd cfg "create"
    r_cmd <- get_cmd cfg "read"
    u_cmd <- get_cmd cfg "update"
    d_cmd <- get_cmd cfg "delete"

    return (sn, DataSource en sn desc c_cmd r_cmd u_cmd d_cmd)
  where
    get_cmd cfg name = do
        cmd <- liftIO $ C.lookup cfg name
        case cmd of
            Nothing -> throwError $ "No " <> name <> " command for " <>
                entity_name <> "." <> source_name
            Just c -> return $ Command c

-- | Get a 'DataSource' from a 'Configuration'.
getDataSource
    :: Configuration
    -> EntityName
    -> SourceName
    -> Either String DataSource
getDataSource Configuration{configEntities = es} en sn =
    case M.lookup en es of
        Nothing -> Left $ "No configuration for entity: " <> show en
        Just ss -> case M.lookup sn (entitySources ss) of
            Nothing -> Left $ "No configuration for entity and data source: "
                <> n
            Just ds -> Right ds
  where
    n = T.unpack $ ename en <> "/" <> sname sn
