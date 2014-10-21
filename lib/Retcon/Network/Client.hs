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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Data.Binary
import Data.ByteString.Lazy (toStrict, fromStrict)
import Data.List.NonEmpty


import Retcon.Diff
import Retcon.Document
import Retcon.Network.Server

import System.ZMQ4.Monadic

-- | Retrieve all documents that are currently marked as being conflicted
getConflicted
    :: (RetconClientConnection m, MonadError RetconClientError m)
    =>  m [( Document
           , Diff ()
           , DiffID
           , [(ConflictedDiffOpID, DiffOp ())]
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

newtype RetconClientZMQ z a =
    RetconClientZMQ {
        unRetconClientZMQ :: ExceptT RetconClientError (ReaderT (Socket z Req) (ZMQ z)) a
      } deriving ( Functor, Applicative, Monad, MonadError RetconClientError
                 , MonadReader (Socket z Req), MonadIO)


-- | This typeclass provides an abstraction for sending messages to and
-- recieving messages from a Retcon server.
class MonadError RetconClientError m => RetconClientConnection m where
    performRequest :: (Handler request response, Binary request, Binary response)
                   => Header request response -> request -> m response


liftZMQ :: ZMQ z a -> RetconClientZMQ z a
liftZMQ = RetconClientZMQ . lift . lift

-- | Concrete implementation of RetconClientConnection for an established ZMQ
-- monad connection.
instance RetconClientConnection (RetconClientZMQ z) where
    performRequest header request = do
        let n = toStrict . encode $ fromEnum (SomeHeader header)
            req = toStrict . encode $ request

        soc <- ask
        liftZMQ . sendMulti soc . fromList $ [n, req]
        response <- liftZMQ . receiveMulti $ soc
        case response of
            [isErr,body]
              | decode . fromStrict $ isErr -> return . decode . fromStrict $ body
              | otherwise -> throwError $ undefined
            _ -> throwError $ undefined

-- | Set up a connection to the target and then run some ZMQ action
runRetconZMQ
    :: forall a.
       String -- ^ ZMQ connection target, e.g. \"tcp://127.0.0.1:1234\"
    -> (forall z. RetconClientZMQ z a)
    -> IO (Either RetconClientError a)
runRetconZMQ target action = runZMQ $ do
        soc <- socket Req
        connect soc target
        let action' = runExceptT $ unRetconClientZMQ action
        x <- runReaderT action' soc
        disconnect soc target
        close soc
        return x

test :: IO (Either RetconClientError [(Document, Diff (), DiffID, [(ConflictedDiffOpID, DiffOp ())])])
test =
    runRetconZMQ "tcp://host:1234" getConflicted
