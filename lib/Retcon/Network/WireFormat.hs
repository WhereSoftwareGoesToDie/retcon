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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Types and operations shared between the client and server components of retcon.
module Retcon.Network.WireFormat (
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
    RequestChange (..),
    RequestConflicted (..),
    RequestResolve (..),
    ResponseChange (..),
    ResponseConflicted (..),
    ResponseResolve (..),
    InvalidRequest (..),
    InvalidResponse (..),

    Header (..),
    SomeHeader (..),
    Handler (..)
) where

import qualified Data.Aeson as Aeson
import Control.Applicative
import Control.Exception hiding (Handler, handle)
import Control.Lens.TH
import Control.Monad
import Data.Binary

import Retcon.Core
import Retcon.Diff
import Retcon.Document

data RetconClientError
    = UnknownError SomeException
    | TimeoutError

-- | An opaque reference to a Diff, used to uniquely reference the conflicted
-- diff for resolveDiff.
newtype DiffID = DiffID
    { unDiffID :: Int }
    deriving (Binary)

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
    deriving (Binary)

instance Binary (Diff ()) where
    put = put . Aeson.encode
    get = decode <$> get

instance Binary (DiffOp ()) where
    put = put . Aeson.encode
    get = decode <$> get

instance Binary Document where
    put = put . Aeson.encode
    get = decode <$> get

data RequestConflicted = RequestConflicted
data ResponseConflicted = ResponseConflicted
    [ ( Document
      , Diff ()
      , DiffID
      , [(ConflictedDiffOpID, DiffOp ())]
      )]

instance Binary RequestConflicted where
    put _ = return ()
    get = return RequestConflicted
instance Binary ResponseConflicted where
    put (ResponseConflicted ds) = put ds
    get = ResponseConflicted <$> get

data RequestChange = RequestChange ChangeNotification
data ResponseChange = ResponseChange

instance Binary RequestChange where
    put (RequestChange (ChangeNotification entity source fk)) =
        put (entity, source, fk)
    get = do
        (entity, source, fk) <- get
        return . RequestChange $ ChangeNotification entity source fk
instance Binary ResponseChange where
    put _ = return ()
    get = return ResponseChange

data RequestResolve = RequestResolve DiffID [ConflictedDiffOpID]
data ResponseResolve = ResponseResolve

instance Binary RequestResolve where
    put (RequestResolve did conflicts) = put (did, conflicts)
    get = do
        (did, conflicts) <- get
        return $ RequestResolve did conflicts
instance Binary ResponseResolve where
    put _ = return ()
    get = return ResponseResolve

data InvalidRequest = InvalidRequest
data InvalidResponse = InvalidResponse

instance Binary InvalidRequest where
    put _ = return ()
    get = return InvalidRequest
instance Binary InvalidResponse where
    put _ = return ()
    get = return InvalidResponse

data Header request response where
    HeaderConflicted :: Header RequestConflicted ResponseConflicted
    HeaderChange :: Header RequestChange ResponseChange
    HeaderResolve :: Header RequestResolve ResponseResolve
    InvalidHeader :: Header InvalidRequest InvalidResponse

data SomeHeader where
    SomeHeader
        :: Handler request response
        => Header request response
        -> SomeHeader

instance Enum SomeHeader where
    fromEnum (SomeHeader HeaderConflicted) = 0
    fromEnum (SomeHeader HeaderChange) = 1
    fromEnum (SomeHeader HeaderResolve) = 2
    fromEnum (SomeHeader InvalidHeader) = maxBound

    toEnum 0 = SomeHeader HeaderConflicted
    toEnum 1 = SomeHeader HeaderChange
    toEnum 2 = SomeHeader HeaderResolve
    toEnum _ = SomeHeader InvalidHeader

class (Binary request, Binary response) => Handler request response where
    handle :: request -> IO response


instance Handler RequestConflicted ResponseConflicted where
    handle _ = return undefined --ResponseConflicted

instance Handler RequestChange ResponseChange where
    handle _ = return ResponseChange

instance Handler RequestResolve ResponseResolve where
    handle _ = return ResponseResolve

instance Handler InvalidRequest InvalidResponse where
    handle _ = return InvalidResponse

-- | Send a request and receive the response over the channel.
makeRequest
    :: (Handler request response)
    => Header request response
    -> request
    -> IO response
makeRequest op request = do
    let n = fromEnum (SomeHeader op)
    -- print n
    return undefined

handleHeader
    :: forall request response.
        (Read request, Show response, Handler request response)
    => Header request response
    -> IO ()
handleHeader _ = do
    let req = read "RequestA"
    x <- handle (req :: request)
    print (x :: response)
