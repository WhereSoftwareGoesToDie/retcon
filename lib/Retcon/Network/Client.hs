--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | A typeclass and IO implementation of a client API for Retcon
--
-- With this API you will be able to:
-- * List conflicted diffs
-- * Force a conflicted diff to resolve with any operations you chose
--   pushed upstream.
-- * Notfiy retcon of datasources that need to be updated
--
-- Example usage:
--
-- @
--         TODO: Put example usage here
-- @
module Retcon.Network.Client
(
    -- * Operations
    getConflicted,
    enqueueResolveDiff,
    enqueueChangeNotification,

    runRetconZMQ,
) where

import Control.Monad.Except
import Control.Monad.Identity
import Data.ByteString
import Data.List.NonEmpty

import Retcon.Diff
import Retcon.Document
import Retcon.Network.WireFormat

-- | Retrieve all documents that are currently marked as being conflicted
getConflicted
    :: (RetconClientConnection m, MonadError RetconClientError m)
    =>  m [( Document
           , Diff a
           , DiffID
           , [(ConflictedDiffOpID, DiffOp a)]
          )]
getConflicted = error "ZMQ getConflicted unimplemented"

-- | Tell Retcon to apply the given operations upstream at some point
enqueueResolveDiff
    :: (RetconClientConnection m, MonadError RetconClientError m)
    => DiffID
    -> [ConflictedDiffOpID]
    -> m ()
enqueueResolveDiff _ _ = error "ZMQ resolveDiff unimplemented"

-- | Notify Retcon of an external change
enqueueChangeNotification
    :: (RetconClientConnection m, MonadError RetconClientError m)
    => ChangeNotification
    -> m ()
enqueueChangeNotification _ = error "ZMQ enqueueChangeNotification unimplemented"


-- | This typeclass provides an abstraction for sending messages to and
-- recieving messages from a Retcon server.
class MonadError RetconClientError m => RetconClientConnection m where
    send :: NonEmpty ByteString -> m ()
    recv :: m (NonEmpty ByteString)

type ZMQ z = Identity

-- | Concrete implementation of RetconClientConnection for an established ZMQ
-- monad connection.
instance RetconClientConnection (ExceptT RetconClientError (ZMQ z)) where
    send = error "ZMQ send unimplemented"
    recv = error "ZMQ send unimplemented"

-- | Set up a connection to the target and then run some ZMQ action
runRetconZMQ
    :: String -- ^ ZMQ connection target, e.g. \"tcp://127.0.0.1:1234\"
    -> ZMQ z a
    -> IO a
runRetconZMQ = error "runRetconZMQ unimplemented"

test :: IO (Either RetconClientError [(Document, Diff a, DiffID, [(ConflictedDiffOpID, DiffOp a)])])
test =
    runRetconZMQ "tcp://host:1234" . runExceptT $ getConflicted
