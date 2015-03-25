--
-- Copyright © 2014-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Description: Diff and patch /Synchronise/ documents.
module Synchronise.Diff
     ( MergePolicy(..)
     , diff
     , mergePatches

       -- * Re-exports
     , Operation(..)
     , Patch(..)

     ) where

import Control.Lens hiding ((.=))
import Data.Aeson.Diff (Operation, Patch)
import qualified Data.Aeson.Diff as D
import Data.Monoid

import Synchronise.Document

data MergePolicy l = MergePolicy
    { extractLabel :: Document -> l
    , mergePatchs  :: Patch -> Patch -> Patch }

diff
    :: MergePolicy l
    -> Document
    -> Document
    -> Patch
diff MergePolicy{..} d1 d2 =
    let j1 = d1 ^. documentContent
        j2 = d2 ^. documentContent
    in  D.diff j1 j2

-- | Combine two 'Patch'es according to the rules of a 'MergePolicy'.
--
-- This allows case-specific criteria to be used in resolving ambiguities which
-- might arise when resolving conflicts between patches.
mergePatches
    :: Monoid l
    => MergePolicy l
    -> Patch
    -> Patch
    -> (Patch, (Patch, Patch))
mergePatches MergePolicy{..} p1 p2 = (mempty, (p1, p2))
