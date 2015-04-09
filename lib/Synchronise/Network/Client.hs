--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Synchronise.Network.Client
  ( -- * Interface
    getConflicted
  , enqueueResolvePatch
  , enqueueChangeNotification

    -- * Running
  , runSynchroniseZMQ

    -- * Utils
  , mkChangeNotification
  ) where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Data.Binary
import Data.ByteString.Lazy (fromStrict)
import Data.List.NonEmpty
import System.ZMQ4.Monadic

import Synchronise.Identifier
import Synchronise.Network.Protocol
import Synchronise.Network.Server
import Synchronise.Store hiding (ops)


-- | Retrieve all documents that are currently marked as being conflicted
getConflicted
    :: (SynchroniseClientConnection m, MonadError APIError m)
    =>  m [ResponseConflictedItem]
getConflicted = do
    ResponseConflicted response <- performRequest HeaderConflicted RequestConflicted
    return response

-- | Tell Synchronise to apply the given operations upstream at some point
enqueueResolvePatch
    :: (SynchroniseClientConnection m, MonadError APIError m)
    => DiffID
    -> [OpID]
    -> m ()
enqueueResolvePatch did ops =
    void $ performRequest HeaderResolve (RequestResolve did ops)

-- | Notify Synchronise of an external change
enqueueChangeNotification
    :: (SynchroniseClientConnection m, MonadError APIError m)
    => ChangeNotification
    -> m ()
enqueueChangeNotification notification =
    void $ performRequest HeaderChange (RequestChange notification)

--------------------------------------------------------------------------------

mkChangeNotification :: ForeignKey -> ChangeNotification
mkChangeNotification k = ChangeNotification (fkEntity k) (fkSource k) (fkID k)

--------------------------------------------------------------------------------

newtype SynchroniseClientZMQ z a =
    SynchroniseClientZMQ {
        unSynchroniseClientZMQ :: ExceptT APIError (ReaderT (Socket z Req) (ZMQ z)) a
      } deriving ( Functor, Applicative, Monad, MonadError APIError
                 , MonadReader (Socket z Req), MonadIO)


-- | This typeclass provides an abstraction for sending messages to and
-- recieving messages from a Synchronise server.
class (MonadError APIError m, Functor m)
        => SynchroniseClientConnection m where
    performRequest :: (Binary request, Binary response)
                   => Header request response -> request -> m response


liftZMQ :: ZMQ z a -> SynchroniseClientZMQ z a
liftZMQ = SynchroniseClientZMQ . lift . lift

-- | Concrete implementation of SynchroniseClientConnection for an established ZMQ
-- monad connection.
instance SynchroniseClientConnection (SynchroniseClientZMQ z) where
    performRequest header request = do
        let n = encodeStrict $ fromEnum (SomeHeader header)
            req = encodeStrict request

        soc <- ask
        liftZMQ . sendMulti soc . fromList $ [n, req]
        response <- liftZMQ . receiveMulti $ soc
        case response of
            [success,body]
                | decode . fromStrict $ success ->
                    decodeStrict body
                | otherwise -> throwError =<< (toEnum <$> decodeStrict body)
            _ -> throwError InvalidNumberOfMessageParts

-- | Set up a connection to the target and then run some ZMQ action
runSynchroniseZMQ
    :: forall a.
       String -- ^ ZMQ connection target, e.g. \"tcp://127.0.0.1:1234\"
    -> (forall z. SynchroniseClientZMQ z a)
    -> IO (Either APIError a)
runSynchroniseZMQ target action = runZMQ $ do
        soc <- socket Req
        connect soc target
        x <- flip runReaderT soc
           $ runExceptT
           $ unSynchroniseClientZMQ action
        disconnect soc target
        close soc
        return x
