--
-- Copyright © 2014-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}

-- | Description: Unique identifiers for internal an external use.
module Synchronise.Identifier (
    -- * Configuration names
    EntityName,
    unwrapEntityName,
    SourceName,
    unwrapSourceName,
    -- * Unique identifiers
    ForeignKey(..),
    InternalKey(..),
    -- * Checking compatibility
    Synchronisable(..),
    compatibleEntity,
    compatibleSource,
) where

import Data.String
import Data.Text (Text)
import qualified Data.Text as T

-- | Unique name for an entity.
newtype EntityName = EntityName { unwrapEntityName :: Text }
  deriving (Eq, Ord, Show)

instance IsString EntityName where
    fromString = EntityName . T.pack

-- | Unique name for a data source.
newtype SourceName = SourceName { unwrapSourceName :: Text }
  deriving (Eq, Ord, Show)

instance IsString SourceName where
    fromString = SourceName . T.pack

-- | Types which participate, in one way or another, in the synchronisation
-- process.
class Synchronisable a where
    -- | Get the 'EntityName' for which the value is valid.
    getEntityName :: a -> EntityName

    -- | Get the 'SourceName' for which the value is valid.
    getSourceName :: a -> SourceName

-- | Uniquely identify a 'Document' shared across one or more 'DataSource's.
--
-- Each 'InternalKey' value can be mapped to the 'ForeignKey's for the
-- 'DataSource's which store copies of the associated 'Document'.
data InternalKey = InternalKey
    { ikEntity :: EntityName
    , ikKey    :: Int
    }
  deriving (Eq)

instance Synchronisable InternalKey where
    getEntityName = ikEntity
    getSourceName _ = SourceName ""

-- | Uniquely identify a 'Document' stored in 'DataSource'.
data ForeignKey = ForeignKey
    { fkEntity :: EntityName
    , fkSource :: SourceName
    , fkKey    :: Text
    }
  deriving (Eq)

instance Synchronisable ForeignKey where
    getEntityName = fkEntity
    getSourceName = fkSource

-- | Check that two synchronisable values have the same entity.
compatibleEntity
    :: (Synchronisable a, Synchronisable b)
    => a
    -> b
    -> Bool
compatibleEntity a b = getEntityName a == getEntityName b

-- | Check that two synchronisable values have the same data source.
compatibleSource
    :: (Synchronisable a, Synchronisable b)
    => a
    -> b
    -> Bool
compatibleSource a b = compatibleEntity a b && getSourceName a == getSourceName b
