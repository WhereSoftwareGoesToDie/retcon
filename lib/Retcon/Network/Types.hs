--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

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

    -- * Protocol
    RequestA (..),
    RequestB (..),
    ResponseA (..),
    ResponseB (..),
    InvalidRequest (..),
    InvalidResponse (..),

    Header (..),
    SomeHeader (..),
    Handler (..),
) where

import Control.Exception hiding (Handler)
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

data RequestA = RequestA deriving Read
data ResponseA = ResponseA deriving Show
data RequestB = RequestB deriving Read
data ResponseB = ResponseB deriving Show
data InvalidRequest = InvalidRequest deriving Read
data InvalidResponse = InvalidResponse deriving Show

data Header request response where
    HeaderA :: Header RequestA ResponseA
    HeaderB :: Header RequestB ResponseB
    InvalidHeader :: Header InvalidRequest InvalidResponse

data SomeHeader where
    SomeHeader
        :: Handler request response
        => Header request response
        -> SomeHeader

instance Enum SomeHeader where
    fromEnum (SomeHeader HeaderA) = 0
    fromEnum (SomeHeader HeaderB) = 1
    fromEnum (SomeHeader InvalidHeader) = maxBound

    toEnum 0 = SomeHeader HeaderA
    toEnum 1 = SomeHeader HeaderB
    toEnum _ = SomeHeader InvalidHeader

class (Read request, Show response) => Handler request response where
    handle :: request -> IO response

instance Handler RequestA ResponseA where
    handle _ = return ResponseA

instance Handler RequestB ResponseB where
    handle _ = return ResponseB

instance Handler InvalidRequest InvalidResponse where
    handle _ = return InvalidResponse
