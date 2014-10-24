--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Server component for the retcon network API.
module Retcon.Network.Server where

import Control.Applicative
import Control.Concurrent.Async
import Control.Lens.Operators
import Control.Lens.TH
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.Aeson as Aeson
import Data.Binary
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.ByteString.Lazy as LBS
import Data.List.NonEmpty hiding (map)
import Data.Monoid
import Data.String
import Data.Typeable
import Options.Applicative
import System.ZMQ4.Monadic hiding (async)

import Retcon.Core
import Retcon.Diff
import Retcon.Document
import Retcon.Monad
import Retcon.Options

-- | Values describing error states of the retcon API.
data RetconAPIError
    = UnknownServerError
    | TimeoutError
    | DecodeError
    | InvalidNumberOfMessageParts
  deriving (Show, Eq, Typeable)

instance Exception RetconAPIError

instance Enum RetconAPIError where
    fromEnum TimeoutError = 0
    fromEnum InvalidNumberOfMessageParts = 1
    fromEnum DecodeError = 2
    fromEnum UnknownServerError = maxBound

    toEnum 0 = TimeoutError
    toEnum 1 = InvalidNumberOfMessageParts
    toEnum 2 = DecodeError
    toEnum _ = UnknownServerError

-- | An opaque reference to a Diff, used to uniquely reference the conflicted
-- diff for resolveDiff.
newtype DiffID = DiffID
    { unDiffID :: Int }
  deriving (Binary, Eq, Show)

-- | A notification for Retcon that the document with 'ForeignID' which is an
-- 'EntityName' at the data source 'SourceName' has changed in some way.
data ChangeNotification = ChangeNotification
    { _notificationEntity    :: EntityName
    , _notificationSource    :: SourceName
    , _notificationForeignID :: ForeignID
    }
  deriving (Eq, Show)
makeLenses ''ChangeNotification

-- | An opaque reference to a DiffOp, used when sending the list of selected
-- DiffOps to resolveDiff
newtype ConflictedDiffOpID = ConflictedDiffOpID
    { unConflictedDiffOpID :: Int }
  deriving (Binary, Eq, Show)

getJSON
    :: FromJSON a
    => Get a
getJSON = do
    json <- Aeson.eitherDecode <$> get
    case json of
        Right x -> return x
        Left msg -> fail msg

instance Binary (Diff ()) where
    put = put . Aeson.encode
    get = getJSON

instance Binary (DiffOp ()) where
    put = put . Aeson.encode
    get = getJSON

instance Binary Document where
    put = put . Aeson.encode
    get = getJSON

data RequestConflicted = RequestConflicted
  deriving (Eq, Show)
data ResponseConflicted = ResponseConflicted
    [ ( Document
      , Diff ()
      , DiffID
      , [(ConflictedDiffOpID, DiffOp ())]
    )]
  deriving (Eq, Show)

instance Binary RequestConflicted where
    put _ = return ()
    get = return RequestConflicted
instance Binary ResponseConflicted where
    put (ResponseConflicted ds) = put ds
    get = ResponseConflicted <$> get

data RequestChange = RequestChange ChangeNotification
  deriving (Eq, Show)
data ResponseChange = ResponseChange
  deriving (Eq, Show)

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
  deriving (Eq, Show)
data ResponseResolve = ResponseResolve
  deriving (Eq, Show)

instance Binary RequestResolve where
    put (RequestResolve did conflicts) = put (did, conflicts)
    get = do
        (did, conflicts) <- get
        return $ RequestResolve did conflicts
instance Binary ResponseResolve where
    put _ = return ()
    get = return ResponseResolve

data InvalidRequest = InvalidRequest
  deriving (Eq, Show)
data InvalidResponse = InvalidResponse
  deriving (Eq, Show)

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
        :: Header request response
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
    { unRetconServer :: ExceptT RetconAPIError (ReaderT (Socket z Rep) (LoggingT (ZMQ z))) a
    }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader (Socket z Rep),
  MonadError RetconAPIError, MonadLogger, MonadCatch, MonadThrow)

-- | Run a handler in the 'RetconServer' monad using the ZMQ connection details.
runRetconServer
    :: forall a. ServerConfig
    -> (forall z. RetconServer z a)
    -> IO ()
runRetconServer cfg act = runZMQ $ runStdoutLoggingT $ do
    sock <- lift . socket $ Rep
    lift . bind sock $ cfg ^. cfgConnectionString
    void $ flip runReaderT sock . runExceptT . unRetconServer $ act

-- * Monads with ZMQ

liftZMQ :: ZMQ z a -> RetconServer z a
liftZMQ = RetconServer . lift . lift . lift

-- * Server actions

decodeStrict
    :: (MonadError RetconAPIError m, Binary a)
    => BS.ByteString
    -> m a
decodeStrict bs =
    case decodeOrFail . fromStrict $ bs of
        Left{} -> throwError DecodeError
        Right (_, _, x) -> return x

encodeStrict
    :: (Binary a)
    => a
    -> BS.ByteString
encodeStrict = toStrict . encode

-- | Implement the API protocol.
protocol
    :: RetconServer z ()
protocol = loop
  where
    loop = do
        sock <- ask
        cmd <- liftZMQ . receiveMulti $ sock
        -- Decode and process the message.
        (status, resp) <- case cmd of
            [hdr, req] -> catchAndInject $
                          join $ dispatch <$> (toEnum <$> decodeStrict hdr)
                                          <*> pure (fromStrict req)
            _          -> throwError InvalidNumberOfMessageParts
        -- Encode and send the response.
        liftZMQ . sendMulti sock . fromList $ [encodeStrict status, resp]
        -- LOOOOOP
        loop

    -- Decode a request and call the appropriate handler.
    dispatch
        :: SomeHeader
        -> LBS.ByteString
        -> RetconServer z (Bool, BS.ByteString)
    dispatch (SomeHeader hdr) body =
        ((True,) <$> case hdr of
            HeaderConflicted -> encodeStrict <$> listConflicts (decode body)
            HeaderResolve -> encodeStrict <$> resolveConflict (decode body)
            HeaderChange -> encodeStrict <$> notify (decode body)
            InvalidHeader -> return . encodeStrict $ InvalidResponse)

    -- Catch exceptions and inject them into the monad as errors.
    --
    -- TODO: Chain together the catching and handling of difference Exception
    -- types and return more specific errors, if available.
    catchAndInject
        :: RetconServer z (Bool, BS.ByteString)
        -> RetconServer z (Bool, BS.ByteString)
    catchAndInject act = catchError (catch act injectSomeException) reportAPIError
      where
        injectSomeException :: MonadError RetconAPIError m => SomeException -> m a
        injectSomeException _ = throwError UnknownServerError

    -- Handle an error in executing operations by sending it back to the client.
    reportAPIError
        :: RetconAPIError
        -> RetconServer z (Bool, BS.ByteString)
    reportAPIError e = do
        -- TODO Log this more properly.
        liftIO . putStrLn . fromString $ "Could not process message: " <> show e
        return (False, toStrict . encode . fromEnum $ e)

-- | Process a _notify_ message from the client, checking the
notify
    :: RequestChange
    -> RetconServer z ResponseChange
notify (RequestChange (ChangeNotification n d i)) = do
    logInfoN . fromString $
        "Processing change notification: " <> show (n,d,i)

    -- TODO: We're in the wrong monad to run these. How best shall we achieve
    -- this?

    -- addWork (WorkNotify (n,d,i))

    error "Retcon.Network.Server.notify is not implemented yet"

    return ResponseChange

-- | Process a _resolve conflict_ message from the client.
--
-- The selected diff is marked as resolved; and a new diff is composed from the
-- selected operations and added to the work queue.
resolveConflict
    :: RequestResolve
    -> RetconServer z ResponseResolve
resolveConflict (RequestResolve diff_id op_ids) = do
    logInfoN . fromString $
        "Resolving diff " <> show (unDiffID diff_id) <> " with " <>
        (show . map unConflictedDiffOpID $ op_ids)

    -- TODO: We're in the wrong monad to run these. How best shall we achieve
    -- this?

    -- new_diff <- composeNewDiff op_ids
    -- addWork (WorkApplyPatch (unDiffID diff_id) new_diff)

    error "Retcon.Network.Server.resolveConflict is not implemented yet"

    return ResponseResolve

-- | Fetch the details of outstanding conflicts and return them to the client.
listConflicts
    :: RequestConflicted
    -> RetconServer z ResponseConflicted
listConflicts RequestConflicted = do
    logInfoN . fromString $
        "Listing conflicts for client"

    -- TODO: We're in the wrong monad to run store operations. How best shall
    -- we achieve this?

    -- conflicts <- getConflictedDiffs

    error "Retcon.Network.Server.listConflicts is not implemented yet"

    let conflicts = []
    return $ ResponseConflicted conflicts

-- | Retrieve selected 'DiffOp's from the store and combine them into a new
-- 'Diff'.
composeNewDiff
    :: ReadableToken s
    => [ConflictedDiffOpID]
    -> RetconMonad e s l (Diff ())
composeNewDiff op_ids =
    error "Retcon.Network.Server.composeNewDiff is not implemented yet."

-- * API server

-- | Start a server running the retcon API over a ZMQ socket.
--
-- Open a ZMQ_REP socket and receive requests from it; unhandled errors are caught
-- and fed back through the socket to the client.
apiServer
    :: WritableToken store
    => RetconConfig SomeEntity store
    -> ServerConfig
    -> IO ()
apiServer retconCfg serverCfg = do
    -- TODO: We should probably do logging here just as in retcon proper.
    putStrLn . fromString $
        "Running server on " <> serverCfg ^. cfgConnectionString

    -- Start the API server thread.
    server <- async serverThread
    putStrLn . fromString $
        "Started API server in: " <> show (asyncThreadId server)

    -- Start the processing thread.
    retcon <- async retconThread
    putStrLn . fromString $
        "Started processing in: " <> show (asyncThreadId retcon)

    -- Wait for completion.
    let procs = [server, retcon]
    (done, _) <- waitAnyCancel procs

    putStrLn $ if done == server
        then "Server shutdown!"
        else "Retcon go boom!"
  where
    serverThread = do
        putStrLn "Starting server thread"
        runRetconServer serverCfg protocol
    retconThread = do
        putStrLn "Starting retcon thread"
        void $ runRetconMonadOnce retconCfg () loop
    loop
        :: WritableToken s
        => RetconHandler s ()
    loop = do
        void $ processWork processWorkItem
        loop

-- | Inspect a work item and perform whatever task is required.
processWorkItem
    :: WritableToken s
    => WorkItem
    -> RetconHandler s ()
processWorkItem work = do
    whenVerbose . logInfoN . fromString $
        "Processing work item: " <> show work
    case work of
        WorkNotify fkval -> do
            logDebugN . fromString $
                "Processing a notifcation: " <> show fkval
            -- TODO Something like this:
            --
            -- dispatch fkval

        WorkApplyPatch did new_diff -> do
            logDebugN . fromString $
                "Processing a diff: " <> show did
            -- TODO Something like this:
            --
            -- ik <- getIKForDiff did
            -- distributeDiff ik new_diff

    -- TODO This is not implemented yet.
    error "Retcon.Network.Server.processWorkItem is not implemented."
    return ()
