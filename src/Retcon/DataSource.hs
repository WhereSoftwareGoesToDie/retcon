--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

-- | Description: API to implement entities and data sources.

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}

module Retcon.DataSource where

import Data.Proxy
import GHC.TypeLits

import Retcon.Document

type Retcon a = IO a

-- * Entities

-- | The 'RetconEntity' type class associates a 'Symbol' identifying a
-- particular entity (i.e. a type of data) with a list of 'RetconDataSource's
-- which deal in that entity.
--
-- An implementation should look something like this:
--
-- > instance RetconEntity "account" where
-- >     entitySource _ = [SomeDataSource (Proxy :: "customer-api")]
--
class (KnownSymbol entity) => RetconEntity entity where
    -- | Get a list of data sources associated with the entity.
    entitySources :: Proxy entity -> [SomeDataSource entity]

-- * Data sources

-- | The 'RetconDataSource' type class associates two 'Symbol' types: the first
-- identifies an entity (i.e. a type of data) and the second identifies a
-- system which handles data of that type.
--
-- Each instances provides operations allowing retcon to get, set, delete
-- 'Document' values of the appropriate sort from the external system.
class (KnownSymbol source, RetconEntity entity) => RetconDataSource entity source where

    -- | Put a document into a data source.
    --
    -- If the 'ForeignKey' is not known, it will be omitted and the data source
    -- should treat the 'Document' as being newly created. In either case, the
    -- correct 'ForeignKey' for the 'Document' is returned.
    --
    -- If the document cannot be saved an error is returned in the 'Retcon'
    -- monad.
    setDocument :: Document
                -> Maybe (ForeignKey entity source)
                -> Retcon (ForeignKey entity source)

    -- | Retrieve a document from a data source.
    --
    -- If the document cannot be retrieved an error is returned in the 'Retcon'
    -- monad.
    getDocument :: ForeignKey entity source
                -> Retcon Document

    -- | Delete a document from a data source.
    --
    -- If the document cannot be deleted an error is returned in the 'Retcon'
    -- monad.
    deleteDocument :: ForeignKey entity source
                   -> Retcon ()

-- | The unique identifier used by the 'source' data source to refer to an
-- 'entity' it stores.
newtype RetconDataSource entity source => ForeignKey entity source =
    ForeignKey { unForeignKey :: String }
  deriving (Eq, Ord, Show)

-- * Wrapper types
--
-- $ 'Proxy' values for instances of our 'RetconEntity' and 'RetconDataSource'
-- type classes can be wrapped with existential types, allowing us to put them
-- into data structures like lists easily.

-- | Wrap an arbitrary 'RetconEntity'.
data SomeEntity = forall e. (KnownSymbol e, RetconEntity e) => SomeEntity (Proxy e)

-- | Wrap an arbitrary 'RetconDataSource' for some entity 'e'.
data SomeDataSource e = forall s. RetconDataSource e s => SomeDataSource (Proxy s)

