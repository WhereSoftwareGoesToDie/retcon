--
-- Copyright © 2014-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}

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
  deriving (Eq, Show)

instance Synchronisable DataSource where
    getEntityName = sourceEntity
    getSourceName = sourceName

data Entity = Entity
    { entityName        :: EntityName
    , entityDescription :: Maybe Text
    , entitySchmea      :: Maybe FilePath
    , entitySources     :: [SourceName]
    }
  deriving (Eq, Ord, Show)

-- | Configuration
data Configuration = Configuration
    { configEntities :: Map EntityName Entity
    , configSources  :: Map (EntityName, SourceName) DataSource
    }
  deriving (Eq, Show)

type Parser a = Config -> ExceptT Text IO a

-- | Parse a configurator 'Config' value into a 'Configuration'.
parseConfiguration
    :: Parser Configuration
parseConfiguration cfg = Configuration
    <$> entities cfg
    <*> sources cfg
  where
    entities :: Parser (Map EntityName Entity)
    entities _ = do
        enabled <- liftIO $ C.lookup cfg "entities.enabled"
        liftIO . print $ (enabled :: Maybe [Text])
        lol <- case enabled of
            Nothing -> throwError "No entities enabled in configuration."
            Just [] -> throwError "No entities enabled in configuration."
            Just es -> mapM (`parseEntity` cfg) . fmap ("entities." <>) $ es
        liftIO . print $ lol
        return M.empty
    sources :: Parser (Map (EntityName, SourceName) DataSource)
    sources _ = return M.empty

parseEntity
    :: Text -> Parser Entity
parseEntity _ _ = throwError "No"

parseDataSource
    :: Text -> Parser DataSource
parseDataSource _ _ = throwError "No"
