-- | Re-export the public API to /Retcon/.
module Retcon (
    R.version,

    -- * Configuration
    module Retcon.Configuration,
    -- * Identifiers
    module Retcon.Identifier,
    -- * Diffs
    module Retcon.Diff,
    -- * Documents
    module Retcon.Document,
    -- * Data sources
    module Retcon.DataSource,
) where

import           Retcon.Configuration
import           Retcon.DataSource
import           Retcon.Diff
import           Retcon.Document
import           Retcon.Identifier

import           Paths_retcon         as R
