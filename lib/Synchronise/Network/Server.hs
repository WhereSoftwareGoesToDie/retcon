{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

module Synchronise.Network.Server where


import qualified Data.List as L
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import qualified Control.Exception as E
import Control.Lens hiding (Context, coerce)
import Control.Monad.Catch
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Data.Binary
import qualified Data.ByteString as BS hiding (unpack)
import qualified Data.ByteString.Char8 as BS (unpack)
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce
import Data.List.NonEmpty hiding (filter, length, map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.String
import qualified Data.Text as T
import System.Log.Logger
import System.ZMQ4

import Synchronise.Configuration
import Synchronise.DataSource (runDSMonad, readDocument)
import Synchronise.Document
import Synchronise.Identifier
import Synchronise.Network.Protocol
import Synchronise.Monad
import Synchronise.Store
import Synchronise.Store.PostgreSQL

--------------------------------------------------------------------------------

-- * Server

data ServerState = ServerState
    { _serverContext :: Context
    , _serverSocket  :: Socket Rep
    , _serverConfig  :: Configuration
    , _serverStore   :: PGStore
    }
makeLenses ''ServerState

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
    bracket start stop (`runProtocol` protocol)
    noticeM logName "Finished server"
    return ()
  where
    start :: IO ServerState
    start = do
        let (zmq_conn, _, pg_conn) = configServer cfg
        ctx <- context
        sock <- socket ctx Rep
        bind sock zmq_conn
        db <- initBackend (PGOpts pg_conn)
        return $  ServerState ctx sock cfg db
    stop :: ServerState -> IO ()
    stop state = do
        close $ state ^. serverSocket
        term $ state ^. serverContext
        return ()

--------------------------------------------------------------------------------

-- * Protocol implementation

-- | A monad which wraps up some state, some error handling, and some IO to
-- implement the server side of synchronised.
newtype Protocol a = Proto
    { unProtocol :: ExceptT APIError (ReaderT ServerState IO) a }
  deriving (Applicative, Functor, Monad, MonadError APIError,
  MonadIO, MonadReader ServerState)

instance MonadThrow Protocol where
    throwM = liftIO . E.throwIO

instance MonadCatch Protocol where
    (Proto (ExceptT m)) `catch` f = Proto . ExceptT $ m `catch` (runExceptT . unProtocol . f)

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
        sock <- _serverSocket <$> ask
        -- Read a response from the client.
        cmd <- liftIO $ receiveMulti sock
        -- Decode and process it.
        (status, resp) <- case cmd of
            [hdr, req] -> catchAndInject . join $
                    dispatch <$> (toEnum <$> decodeStrict hdr)
                             <*> pure (fromStrict req)
            _ -> throwError InvalidNumberOfMessageParts
        -- Send the response to the client.
        liftIO . sendMulti sock . fromList $ [encodeStrict status, resp]
        -- Play it again, Sam.
        loop
    dispatch
        :: SomeHeader
        -> LBS.ByteString
        -> Protocol (Bool, BS.ByteString)
    dispatch (SomeHeader hdr) body =
        (True,) <$> case hdr of
            HeaderConflicted -> encodeStrict <$> listConflicts (decode body)
            HeaderResolve    -> encodeStrict <$> resolveConflict (decode body)
            HeaderChange     -> encodeStrict <$> notify (decode body)
            InvalidHeader    -> return . encodeStrict $ InvalidResponse

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
    conflicts <- liftIO . lookupConflicts =<< view serverStore
    return $ ResponseConflictedSerialised $ fmap mkRespItem conflicts
    where mkRespItem ConflictResp{..}
            = ResponseConflictedSerialisedItem
              _conflictRawDoc
              _conflictRawDiff
              (coerce _conflictDiffID)
              (coerce _conflictRawOps)

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
    let ent_name = note ^. notificationEntity
        src_name = note ^. notificationSource
        fid      = note ^. notificationForeignID
    cfg   <- _serverConfig <$> ask
    store <- _serverStore  <$> ask

    liftIO . infoM logName . T.unpack $
        "Received change notification for: " <> ename ent_name <> "." <>
        sname src_name <> "/" <> fid

    let m_ds = do
            Entity{..} <- M.lookup ent_name (configEntities cfg)
            M.lookup src_name entitySources

    case m_ds of
        Nothing -> do
            liftIO . errorM logName . T.unpack $ "Unknown entity or source: "
                <> ename ent_name <> "." <> sname src_name
            throwError UnknownKeyError
        Just _ ->
            liftIO . addWork store . WorkNotify $ ForeignKey ent_name src_name fid

    return ResponseChange


--------------------------------------------------------------------------------

-- * Asynchronous Server Workers

-- | Operations that the server can perform upon receiving a notification.
--
data NotifyOp
    = NotifyCreate  ForeignKey Document         -- ^ Create a new document.
    | NotifyDelete  InternalKey                 -- ^ Delete an existing document.
    | NotifyUpdate  InternalKey                 -- ^ Update an existing document.
    | NotifyProblem ForeignKey SynchroniseError -- ^ Record an error.
  deriving (Show)

-- | A worker for the synchronised server.
--   These workers cannot die, they simply log any errors and keep going.
--
worker :: Protocol ()
worker = go
  where go = do
          store          <- _serverStore <$> ask
          (workid, item) <- liftIO $ getWorkBackoff store
          case item of
            WorkNotify fk -> do
              liftIO . debugM logName $ "Processing a notifcation: " <> show fk
              return ()
              --dispatch fk
          go

process :: ForeignKey -> Protocol ()
process fk@(ForeignKey{..}) = do
  cfg   <- _serverConfig <$> ask

  let ds = do es <- M.lookup fkEntity (configEntities cfg)
              _  <- M.lookup fkSource (entitySources es)
              ds <- M.lookup fkSource (entitySources es)
              return ds
  case ds of
    Nothing         -> liftIO . errorM logName $ "Unknown key in workqueue: " <> show fk
    Just datasource -> mkOp datasource fk >>= runOp

mkOp :: DataSource -> ForeignKey -> Protocol NotifyOp
mkOp datasource fk@(ForeignKey{..}) = do
  store <- _serverStore  <$> ask
  ik    <- liftIO     $ lookupInternalKey store fk
  doc   <- runDSMonad $ readDocument datasource fk
  let op = case (ik, doc) of
        (Nothing, Left  _) -> NotifyProblem fk (SynchroniseUnknown "Unknown key. No Document")
        (Nothing, Right d) -> NotifyCreate  fk d
        (Just i,  Left  _) -> NotifyDelete  i
        (Just i,  Right d) -> NotifyUpdate  i
  return op 

runOp :: NotifyOp -> Protocol ()
runOp = undefined 

-- | Get a work item from the work queue, apply a constant backoff if there is
--   nothing in the queue.
--
--   This function must not
getWorkBackoff :: Store store => store -> IO (WorkItemID, WorkItem)
getWorkBackoff store = do
  work <- getWork store
  case work of
    Nothing -> threadDelay 50000 >> getWorkBackoff store
    Just x  -> return x

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
