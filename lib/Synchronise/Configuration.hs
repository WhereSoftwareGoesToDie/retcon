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
    }
  deriving (Eq, Show)

type Parser a = Config -> ExceptT Text IO a

-- | Parse a configurator 'Config' value into a 'Configuration'.
parseConfiguration
    :: Parser Configuration
parseConfiguration cfg = Configuration <$> entities cfg
  where
    entities :: Parser (Map EntityName Entity)
    entities _ = do
        enabled <- liftIO $ C.lookup cfg "entities.enabled"
        ents <- case enabled of
            Nothing -> throwError "No entities enabled in configuration."
            Just [] -> throwError "No entities enabled in configuration."
            Just es -> mapM (`parseEntity` cfg) . fmap ("entities." <>) $ es
        return . M.fromList . fmap (\e -> (entityName e, e)) $ ents

-- | Parse an
parseEntity
    :: Text
    -> Parser Entity
parseEntity name cfg = Entity
    <$> parseName cfg
    <*> parseDescription cfg
    <*> parsePath "schema" cfg
    <*> parsePath "merge-policy" cfg
    <*> parseSources cfg
  where
    parseName :: Parser EntityName
    parseName _ = return "LOL"
    parseDescription :: Parser (Maybe Text)
    parseDescription _ = liftIO $ C.lookup cfg (name <> ".description")
    parsePath n _ = liftIO $ C.lookup cfg (name <> "." <> n)
    parseSources :: Parser (Map SourceName DataSource)
    parseSources _ = do
        enabled <- liftIO $ C.lookup cfg (name <> ".enabled")
        sources <- case enabled of
            Nothing -> throwError $ "No sources enabled in " <> name
            Just [] -> throwError $ "No sources enabled in " <> name
            Just ss -> mapM (`parseDataSource` cfg) . fmap (name,) $ ss
        return . M.fromList $ sources

-- | Parse a data source from a configuration.
parseDataSource
    :: (Text, Text)
    -> Parser (SourceName, DataSource)
parseDataSource (_entity_name, _source_name) _cfg =
    throwError "Data source parsing not implemented"
