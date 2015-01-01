--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

-- | Re-export the public API to /Synchronise/.
module Synchronise (
    -- * Identifiers
    module Synchronise.Identifier,
    -- * Documents
    module Synchronise.Document,
    -- * Data sources
    module Synchronise.DataSource,
) where

import Synchronise.DataSource
import Synchronise.Document
import Synchronise.Identifier

{-# ANN module "HLint: ignore Use import/export shortcut" #-}
