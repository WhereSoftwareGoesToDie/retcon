{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

module Synchronise.Network.Server where


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
import qualified Data.List as L
import Data.List.NonEmpty hiding (filter, length, map)
import qualified Data.Map as M
import Data.Monoid
import Data.String
import qualified Data.Text as T
import System.Log.Logger
import System.ZMQ4

import Synchronise.Configuration
import Synchronise.DataSource (runDSMonad)
import qualified Synchronise.DataSource as DS
import Synchronise.Document
import Synchronise.Identifier
import Synchronise.Monad
import Synchronise.Network.Protocol
import Synchronise.Store
import Synchronise.Store.PostgreSQL


--------------------------------------------------------------------------------

-- * Server

data ServerState = ServerState
    { _serverContext     :: Context       -- ^ ZMQ context
    , _serverSocket      :: Socket Rep    -- ^ ZMQ socket
    , _serverConfig      :: Configuration -- ^ synchronised config
    , _serverStore       :: PGStore       -- ^ Internal data store, shared between all server threads
    , _serverDataSources :: [DataSource]  -- ^ "Memoised" eunmeration of data sources from conf
    }
makeLenses ''ServerState

-- | Name of server component for logging.
logName :: String
logName = "Synchronise.Server"

-- | Spawn a thread serving the synchronise API and a number of worker threads
--   to process requests accepted by that server.
--
spawnServer :: Configuration -> Int -> IO ()
spawnServer cfg n = do
    noticeM logName "Starting server"
    _ <- bracket start stop $ \state -> do

      -- Spawn a server implementing the protocol and some workers
      api      <- spawnServerAPI state
      peasants <- spawnServerWorkers state

      -- Wait for any of the API server or worker threads to finish.
      mapM_ (link2 api) peasants
      waitAny (api:peasants)
    noticeM logName "Finished server"
  where
    spawnServerAPI :: ServerState -> IO (Async ())
    spawnServerAPI           = async . flip runProtocol protocol

    spawnServerWorkers :: ServerState -> IO [Async ()]
    spawnServerWorkers state
      = replicateM n (async $ worker (_serverStore state) (_serverDataSources state) cfg)

    start :: IO ServerState
    start = do
        let (zmq_conn, _, pg_conn) = configServer cfg
        ctx    <- context
        sock   <- socket ctx Rep
        bind sock zmq_conn
        db     <- initBackend (PGOpts pg_conn)
        return $  ServerState ctx sock cfg db (allDataSources cfg)

    stop :: ServerState -> IO ()
    stop state = do
        close $ state ^. serverSocket
        term  $ state ^. serverContext

    allDataSources :: Configuration -> [DataSource]
    allDataSources Configuration{..}
      = let entities =      M.elems   configEntities
        in  concat   $ fmap M.elems $ map entitySources entities


--------------------------------------------------------------------------------

-- * Protocol Implementation

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

-- | A worker for the synchronised server.
--   These workers cannot die, they simply log any errors and keep going.
--
worker :: Store store => store -> [DataSource] -> Configuration -> IO ()
worker store datasources cfg = go
  where -- Get a work item from the queue, mark it as busy and try to complete it.
        -- If all goes well, mark the work as finished when done, otherwise signal
        -- it as free.
        --
        go      = bracketOnError getIt ungetIt completeIt

        getIt   = liftIO $ getWorkBackoff store
        ungetIt = ungetWork store . fst

        completeIt (work_id, item) = do
          case item of
            WorkNotify fk -> do
              liftIO . debugM logName $ "Processing a notifcation: " <> show fk
              processNotification store datasources cfg fk
            WorkApplyPatch did diff -> do
              liftIO . debugM logName $ "Processing a diff: " <> show did
              processDiff did diff
          completeWork store work_id
          go

-- notifications

processNotification :: Store store => store -> [DataSource] -> Configuration -> ForeignKey -> IO ()
processNotification store datasources cfg fk@(ForeignKey{..}) = do
  let ds = do es <- M.lookup fkEntity (configEntities cfg)
              M.lookup fkSource (entitySources es)
  case ds of
    Nothing         -> liftIO . criticalM logName $ "Unknown key in workqueue: " <> show fk
    Just datasource -> do
      ik    <- liftIO     $ lookupInternalKey store fk
      doc   <- runDSMonad $ DS.readDocument datasource fk

      -- Update data sources other than the one from which the notification originated.
      let dss = L.delete datasource datasources

      liftIO $ case (ik, doc) of
        (Nothing, Left  e) -> notifyProblem (SynchroniseUnknown $ show e)
        (Nothing, Right d) -> notifyCreate  store dss fk d
        (Just i,  Left  _) -> notifyDelete  store dss i
        (Just i,  Right d) -> notifyUpdate  store dss i

-- | Creates a new internal document to reflect a new foreign change. Update
--   all given data sources of the change.
--
--   Caller is responsible for: ensuring the datasources exclude the one from
--   which the event originates.
--
notifyCreate
  :: Store store => store
  -> [DataSource] -- ^ Create in each of these data sources.
  -> ForeignKey   -- ^ Using this foreign key.
  -> Document     -- ^ And this document.
  -> IO ()
notifyCreate store datasources fk@(ForeignKey{..}) doc = do
  infoM logName $ "CREATE: " <> show fk

  -- Create an internal key associated with the new document
  ik  <- createInternalKey store fkEntity
  recordForeignKey store ik fk

  -- Create an initial document
  recordInitialDocument store ik doc

  -- Update the document for all known entities
  forM_ datasources updateDoc

  where updateDoc ds = do
          x  <- runDSMonad $ DS.updateDocument ds fk doc
          case x of Left  e -> errorM logName (show e)
                    Right _ -> return ()

-- | Deletes internal document to reflect the foreign change. Update
--   all given data sources of the change.
--
--   Caller is responsible for: ensuring the datasources exclude the one from
--   which the event originates.
--
notifyDelete
  :: Store store => store
  -> [DataSource]
  -> InternalKey
  -> IO ()
notifyDelete store datasources ik = do
  infoM logName $ "DELETE: " <> show ik
  forM_ datasources deleteDoc

  where deleteDoc ds = do
          f <- lookupForeignKey store (sourceName ds) ik
          case f of
            Nothing -> return ()
            Just fk -> do
              -- Delete the document
              hushBoth $ runDSMonad $ DS.deleteDocument ds fk
              -- Delete known foreign key
              void $ deleteForeignKey store fk
          -- Delete internal bookkeeping
          void $ deleteInitialDocument store ik
          deleteInternalKey store ik

-- | Updates internal document to reflect the foreign change. Update
--   all given data sources of the change.
--
--   Caller is responsible for: ensuring the datasources exclude the one from
--   which the event originates.
--
notifyUpdate
  :: Store store => store
  -> [DataSource]
  -> InternalKey
  -> IO ()
notifyUpdate store datasources ik =
  infoM logName $ "UPDATE: " <> show ik

-- | Logs a problem with the notification.
--
notifyProblem :: SynchroniseError -> IO ()
notifyProblem = infoM logName . show

-- diffs

processDiff _ _ = do
  infoM logName "DIFF"

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

-- | Silences both errors (via logging) and results.
--
hushBoth :: Show a => IO (Either a b) -> IO ()
hushBoth act = act >>= \x -> case x of
  Left e  -> errorM logName (show e)
  Right _ -> return ()
