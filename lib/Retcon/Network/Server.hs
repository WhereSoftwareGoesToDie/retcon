--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections #-}

-- | Server component for the retcon network API.
module Retcon.Network.Server where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception hiding (Handler, handle)
import Control.Lens.Operators
import Control.Lens.TH
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Aeson as Aeson
import Data.Binary
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.List.NonEmpty
import Data.Monoid
import Data.String
import Options.Applicative
import System.ZMQ4.Monadic

import Retcon.Core
import Retcon.Diff
import Retcon.Document
import Retcon.Options

-- | Values describing error states of the retcon API.
data RetconAPIError
    = UnknownServerError
    | TimeoutError
    | InvalidNumberOfMessageParts
  deriving (Show, Eq)

instance Enum RetconAPIError where
    fromEnum TimeoutError = 0
    fromEnum InvalidNumberOfMessageParts = 1
    fromEnum UnknownServerError = maxBound

    toEnum 0 = TimeoutError
    toEnum 1 = InvalidNumberOfMessageParts
    toEnum _ = UnknownServerError

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

-- * Server configuration

-- | Configuration for the server.
data ServerConfig = ServerConfig
    { _cfgConnectionString :: String
    }
  deriving (Show, Eq)
makeLenses ''ServerConfig

-- | Parser for server options.
serverParser :: Parser ServerConfig
serverParser = ServerConfig <$> connString
  where
    connString = option str (
           long "address"
        <> short 'A'
        <> metavar "SOCKET"
        <> help "Server socket. e.g. tcp://0.0.0.0:60179")

-- * Server monad

-- | Monad for the API server actions to run in.
newtype RetconServer z a = RetconServer
    { unRetconServer :: ExceptT RetconAPIError (ReaderT (Socket z Rep) (ZMQ z)) a
    }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader (Socket z Rep),
  MonadError RetconAPIError)

-- | Run a handler in the 'RetconServer' monad using the ZMQ connection details.
runRetconServer
    :: forall a. ServerConfig
    -> (forall z. RetconServer z a)
    -> IO ()
runRetconServer cfg act = runZMQ $ do
    sock <- socket Rep
    bind sock $ cfg ^. cfgConnectionString
    void $ flip runReaderT sock . runExceptT . unRetconServer $ act

-- * Monads with ZMQ

liftZMQ :: ZMQ z a -> RetconServer z a
liftZMQ = RetconServer . lift . lift

-- * Server actions

-- | Implement the API protocol.
protocol
    :: Chan QueuedWork
    -> RetconServer z ()
protocol queue = loop
  where
    loop = do
        sock <- ask
        cmd <- liftZMQ . receiveMulti $ sock
        -- Decode and process the message.
        (status, resp) <- case cmd of
            [hdr, req] -> dispatch (toEnum . decode . fromStrict $ hdr) (fromStrict req)
            _        -> throwError InvalidNumberOfMessageParts
        -- Encode and send the response.
        liftZMQ . sendMulti sock . fromList $ [toStrict . encode $ status, resp]
        loop

    -- Decode a request and call the appropriate handler.
    dispatch (SomeHeader hdr) body = do
        flip catchError
            (\e -> return (False, toStrict . encode . fromEnum $ e))
            ((True,) <$> case hdr of
                HeaderConflicted -> (toStrict . encode) <$> listConflicts (decode body)
                HeaderResolve -> (toStrict . encode) <$> resolveConflict (decode body)
                HeaderChange -> (toStrict . encode) <$> notify queue (decode body)
                InvalidHeader -> return . toStrict . encode $ InvalidResponse)

-- | Process a _notify_ message from the client, checking the
notify
    :: Chan QueuedWork
    -> RequestChange
    -> RetconServer z ResponseChange
notify queue (RequestChange nid) = do
    liftIO . print $ "Notified"
    liftIO . writeChan queue $ Process nid
    throwError InvalidNumberOfMessageParts
    return ResponseChange

-- | Process a _resolve conflict_ message from the client.
--
-- The selected diff is marked as resolved; and a new diff is composed from the
-- selected operations and added to the work queue.
resolveConflict
    :: RequestResolve
    -> RetconServer z ResponseResolve
resolveConflict (RequestResolve diff_id op_ids) = do
    liftIO . print $ "Resolving diff " <> show (unDiffID diff_id)
    return ResponseResolve

-- | Fetch the details of outstanding conflicts and return them to the client.
listConflicts
    :: RequestConflicted
    -> RetconServer z ResponseConflicted
listConflicts _ = do
    liftIO . print $ "Listing conflicts!"
    return $ ResponseConflicted []

-- * API server

-- | Start a server running the retcon API over a ZMQ socket.
--
-- Open a ZMQ_REP socket and receive requests from it; unhandled errors are caught
-- and fed back through the socket to the client.
apiServer
    :: WritableToken store
    => RetconConfig InitialisedEntity store
    -> ServerConfig
    -> IO ()
apiServer retconCfg serverCfg = do
    -- TODO: We should probably do logging here just as in retcon proper.
    putStrLn . fromString $
        "Running server on " <> serverCfg ^. cfgConnectionString

    -- Setup the work queue.
    queue <- newChan
    qid <- forkIO $ queueWorker queue

    runRetconServer serverCfg (protocol queue)

data QueuedWork
    = Process ChangeNotification
    | Apply DiffID (Diff ())

instance Show QueuedWork where
    show (Process _) = "Process"
    show (Apply _ _) = "Apply"

queueWorker
    :: Chan QueuedWork
    -> IO ()
queueWorker chan = loop
  where
    loop = do
        cmd <- readChan chan
        print cmd
        loop
