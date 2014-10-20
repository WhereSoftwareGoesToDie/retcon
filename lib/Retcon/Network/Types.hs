--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE TemplateHaskell       #-}

-- | Types and operations shared between the client and server components of retcon.
module Retcon.Network.Types (
    -- * Errors
    RetconClientError(..),

    -- * Client types
    DiffID(..),
    ChangeNotification(..),
    notificationEntity,
    notificationSource,
    notificationForeignID,
    ConflictedDiffOpID(..),

) where

import Control.Exception
import Control.Lens.TH

import Retcon.Core

data RetconClientError
    = UnknownError SomeException
    | TimeoutError

-- | An opaque reference to a Diff, used to uniquely reference the conflicted
-- diff for resolveDiff.
newtype DiffID = DiffID
    { unDiffID :: Int }

-- | A notification for Retcon that the document with 'ForeignID' which is an
-- 'EntityName' at the data source 'SourceName' has changed in some way.
data ChangeNotification = ChangeNotification
    { _notificationEntity    :: EntityName
    , _notificationSource    :: SourceName
    , _notificationForeignID :: ForeignID
    }
makeLenses ''ChangeNotification

-- | An opaque reference to a DiffOp, used when sending the list of selected
-- DiffOps to resolveDiff
newtype ConflictedDiffOpID = ConflictedDiffOpID
    { unConflictedDiffOpID :: Int }
