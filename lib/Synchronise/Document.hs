--
-- Copyright © 2014-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

-- | Description: Represent and operate on synchronised documents.
--
-- A 'Document' is, essentially, a JSON 'Value' together with some metadata
-- describing its type, system of origin, etc.
module Synchronise.Document (
    Document(..),
) where

import Data.Aeson

import Synchronise.Identifier

-- | A JSON 'Value' from a particular 'Entity'.
data Document = Document
    { documentEntity  :: EntityName -- ^ Type of data.
    , documentSource  :: SourceName -- ^ System of origin.
    , documentContent :: Value      -- ^ Document content.
    }
  deriving (Show)

instance ToJSON Document where
    toJSON = documentContent

instance Synchronisable Document where
    getEntityName = documentEntity
    getSourceName = documentSource
