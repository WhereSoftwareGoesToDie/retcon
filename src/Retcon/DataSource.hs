--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

-- | Description: API to implement entities and data sources.

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Retcon.DataSource where

import Data.Proxy
import GHC.TypeLits 

import Retcon.Document
import Retcon.Monad

class (KnownSymbol entity) => RetconEntity entity where
    -- | Get a list of data sources.
    entitySource :: Proxy entity -> [SomeDataSource entity]

-- | A foreign key for a specific entity and data source.
newtype ForeignKey entity source = ForeignKey { unForeignKey :: String }
  deriving (Eq, Ord, Show)

-- | Wrap an arbitrary data source for an entity.
data SomeDataSource e = forall s. RetconDataSource e s => SomeDataSource (Proxy s) 

class (KnownSymbol entity, KnownSymbol source) => RetconDataSource entity source where

    -- | Put a document into a data source.
    setDocument :: Document -- ^ Document to store.
                -> Maybe (ForeignKey entity source) -- ^ Foreign key (if known)
		-> Retcon (ForeignKey entity source)

    -- | Retrieve a document from a data source.
    getDocument :: ForeignKey entity source
                -> Retcon Document

    -- | Delete a document from a data source.
    deleteDocument :: ForeignKey entity source
                   -> Retcon ()

