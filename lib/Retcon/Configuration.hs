{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}

-- | Description: Configuration for the system.
module Retcon.Configuration where

import           Control.Applicative
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.ByteString
import qualified Data.ByteString.Char8      as BS
import           Data.Configurator          as C
import           Data.Configurator.Types
import qualified Data.List                  as L
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Monoid
import           Data.String
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           System.Log.Logger

import           Retcon.Diff
import           Retcon.Identifier


-- | Command template.
newtype Command = Command { unCommand :: Text }
  deriving (Eq, Show, Ord)

instance IsString Command where
    fromString = Command . T.pack

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

data Entity = Entity
    { entityName        :: EntityName
    , entityDescription :: Maybe Text
    , entitySchema      :: Maybe FilePath
    , entityPolicy      :: MergePolicy
    , entitySources     :: Map SourceName DataSource
    }

instance Eq Entity where
  (==) (Entity n1 d1 s1 _ c1) (Entity n2 d2 s2 _ c2)
    = n1 == n2 && d1 == d2 && s1 == s2 && c1 == c2

instance Show Entity where
  show (Entity n d s _ c)
    = L.intercalate "," [show n, show d, show s, show c]

-- | Construct an 'Entity' with only a name.
emptyEntity
    :: Text
    -> Entity
emptyEntity name = Entity (EntityName name) mempty mempty doNothing mempty

--------------------------------------------------------------------------------

-- | Configuration of entities and data sources.
data Configuration = Configuration
    { configEntities :: Map EntityName Entity
    , configServer   :: (String, Priority, ByteString)
    }
  deriving (Eq, Show)

-- | An \"empty\" configuration.
emptyConfiguration :: Configuration
emptyConfiguration = Configuration
    mempty
    ("tcp://127.0.0.1:9999", EMERGENCY, "")

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
    server :: Parser (String, Priority, ByteString)
    server _ =
        (,,) <$> liftIO (C.require cfg "server.listen")
             <*> liftIO (read <$> C.require cfg "server.log-level")
             <*> liftIO (BS.pack <$> C.require cfg "server.database")

-- | Parse an entity from a configuration.
parseEntity
    :: Text -- ^ Entity name.
    -> Parser Entity
parseEntity name cfg' = do
    let cfg = C.subconfig ("entities." <> name) cfg'
    Entity
        <$> parseName cfg
        <*> parseDescription cfg
        <*> parseSchema cfg
        <*> (parsePolicy cfg >>= mkPolicy)
        <*> parseSources cfg
  where
    parseName :: Parser EntityName
    parseName _ = EntityName <$> pure name

    parseDescription cfg = liftIO $ C.lookup cfg "description"
    parseSchema      cfg = liftIO $ C.lookup cfg "schema"
    parsePolicy      cfg = liftIO $ C.lookup cfg "merge-policy"

    mkPolicy :: MonadError Text m => Maybe String -> m MergePolicy
    mkPolicy n
      | n == Just "accept-all"       = return acceptAll
      | n == Just "reject-all"       = return rejectAll
      | n == Just "ignore-conflicts" = return ignoreConflicts
      | Just thing   <- n
      , Just trusted <- L.stripPrefix "trust-only:" thing
      = return (trustOnlySource $ SourceName $ T.pack trusted)
      | otherwise = throwError $ "Unrecognised merge policy " <> T.pack (show n)

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
