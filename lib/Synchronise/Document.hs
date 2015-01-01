--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

-- | Description: Represent and operate on documents.
module Synchronise.Document (
    Document,
) where

import Data.Aeson

import Synchronise.Identifier

-- | A JSON 'Value' from a particular 'Entity'.
data Document = Document
    { documentEntity  :: EntityName
    , documentSource  :: SourceName
    , documentContent :: Value
    }

instance ToJSON Document where
    toJSON = documentContent

instance Synchronisable Document where
    getEntityName = documentEntity
    getSourceName = documentSource
