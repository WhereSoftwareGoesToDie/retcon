{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

module Synchronise.Network.Server where

import Control.Applicative
import Control.Concurrent.Async
import Control.Lens hiding (Context)
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Data.Binary
import qualified Data.ByteString as BS hiding (unpack)
import qualified Data.ByteString.Char8 as BS (unpack)
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.ByteString.Lazy as LBS
import Data.List.NonEmpty hiding (filter, length, map)
import Data.Monoid
import Data.String
import System.Log.Logger
import System.ZMQ4

import Synchronise.Configuration
import Synchronise.Network.Protocol

--------------------------------------------------------------------------------
-- * Server

-- | Name of server component for logging.
logName :: String
logName = "Synchronise.Server"

-- | Spawn a new thread running the synchronised API in a new thread.
spawnApiServer
    :: Configuration
    -> IO (Async ())
spawnApiServer cfg = async $ apiServer cfg

-- | Run the synchronised API in the current thread.
--
-- This does not return.
apiServer
    :: Configuration
    -> IO ()
apiServer cfg = do
    noticeM logName "Starting server"
    bracket start stop (flip runProtocol protocol)
    noticeM logName "Finished server"
    return ()
  where
    start :: IO ServerState
    start = do
        ctx <- context
        sock <- socket ctx Rep
        bind sock (fst . configServer $ cfg)
        return (ctx, sock, cfg)
    stop :: ServerState -> IO ()
    stop (ctx, sock, _) = do
        close sock
        term ctx
        return ()

--------------------------------------------------------------------------------
-- * Protocol implementation

type ServerState = (Context, Socket Rep, Configuration)

newtype Protocol a = Proto
    { unProtocol :: ExceptT APIError (ReaderT ServerState IO) a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader ServerState, MonadError APIError, MonadThrow, MonadCatch)

-- | Execute a 'Protocol' action.
runProtocol :: ServerState -> Protocol a -> IO a
runProtocol s act = do
    res <- flip runReaderT s . runExceptT . unProtocol $ act
    case res of
        Left e -> throwM e
        Right a -> return a

-- | Server protocol handler.
protocol :: Protocol ()
protocol = loop
  where
    loop = do
        (_, sock, _cfg) <- ask
        cmd <- liftIO . receiveMulti $ sock
        (status, resp) <- case cmd of
            [hdr, req] -> catchAndInject . join $
                    dispatch <$> (toEnum <$> decodeStrict hdr)
                             <*> pure (fromStrict req)
            _ -> throwError InvalidNumberOfMessageParts
        liftIO . sendMulti sock . fromList $ [encodeStrict status, resp]
        loop
    dispatch
        :: SomeHeader
        -> LBS.ByteString
        -> Protocol (Bool, BS.ByteString)
    dispatch (SomeHeader hdr) body =
        (True,) <$> case hdr of
            HeaderConflicted -> encodeStrict <$> listConflicts (decode body)
            HeaderResolve -> encodeStrict <$> resolveConflict (decode body)
            HeaderChange -> encodeStrict <$> notify (decode body)
            InvalidHeader -> return . encodeStrict $ InvalidResponse

    -- Catch exceptions and inject them into the monad as errors.
    --
    -- TODO: Chain together the catching and handling of difference Exception
    -- types and return more specific errors, if available.
    catchAndInject
        :: Protocol (Bool, BS.ByteString)
        -> Protocol (Bool, BS.ByteString)
    catchAndInject act = catchError (catch act injectSomeException) reportAPIError
      where
        injectSomeException
            :: (MonadIO m, MonadError APIError m)
            => SomeException
            -> m a
        injectSomeException e = do
            liftIO . errorM logName . fromString $
                "Intercepted error to forward to client: " <> show e
            throwError UnknownServerError

    -- Handle an error in executing operations by sending it back to the client.
    reportAPIError
        :: APIError
        -> Protocol (Bool, BS.ByteString)
    reportAPIError e = do
        liftIO . errorM logName . fromString $
            "Could not process message: " <> show e
        return (False, toStrict . encode . fromEnum $ e)

-- | Process a request for unresolved conflicts.
listConflicts
    :: RequestConflicted
    -> Protocol ResponseConflicted
listConflicts RequestConflicted = do
    liftIO $ infoM logName "Listing conflicts"
    liftIO . emergencyM logName $ "Unimplemented: listConflicts"
    return $ ResponseConflicted []

-- | Process and resolve a conflict.
resolveConflict
    :: RequestResolve
    -> Protocol ResponseResolve
resolveConflict (RequestResolve conflict_id _ops) = do
    liftIO . infoM logName $ "Resolving conflict: " <> show conflict_id
    -- TODO(thsutton) Implement
    liftIO . emergencyM logName $ "Unimplemented: resolveConflict"
    return ResponseResolve

-- | Notification of a change to be queued for processing.
notify
    :: RequestChange
    -> Protocol ResponseChange
notify (RequestChange note) = do
    liftIO . infoM logName $
        "Received change notification for: " <> show (note ^. notificationEntity)
    -- TODO(thsutton) Implement
    liftIO . emergencyM logName $ "Unimplemented: notify"
    return ResponseChange

--------------------------------------------------------------------------------
-- * Utility functions

-- | Decode a serializable value from a strict 'ByteString'.
--
-- If the bytestring cannot be decoded, a 'DecodeError' is thrown.
decodeStrict
    :: (MonadIO m, MonadError APIError m, Binary a)
    => BS.ByteString
    -> m a
decodeStrict bs =
    case decodeOrFail . fromStrict $ bs of
        Left _ -> do
            liftIO . warningM logName . BS.unpack $
                "Decode failure for input: " <> bs
            throwError DecodeError
        Right (_, _, x) -> return x

-- | Encode a serialisable value into a strict 'ByteString'.
encodeStrict
    :: (Binary a)
    => a
    -> BS.ByteString
encodeStrict = toStrict . encode
