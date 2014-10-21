--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

-- | Server component for the retcon network API.
module Retcon.Network.Server where

import Control.Applicative
import Control.Lens.Operators
import Control.Lens.TH
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.ByteString as BS
import Data.Monoid
import Data.String
import Options.Applicative
import System.ZMQ4.Monadic

import Retcon.Core
import Retcon.Diff
import Retcon.Document
import Retcon.Network.WireFormat
import Retcon.Options

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
newtype ServerMonad z a = ServerMonad
    { unServerMonad :: ReaderT (Socket z Rep) (ZMQ z) a
    }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader (Socket z Rep))

-- | Run a handler in the 'ServerMonad' using the ZMQ connection details.
runRetconServer
    :: forall a. ServerConfig
    -> (forall z. ServerMonad z a)
    -> IO ()
runRetconServer cfg act = runZMQ $ do
    sock <- socket Rep
    bind sock $ cfg ^. cfgConnectionString
    void $ flip runReaderT sock . unServerMonad $ act
    liftIO $ print "Done it!"
    return ()

-- * Server actions

protocol
    :: ServerMonad z ()
protocol = do
    sock <- ask
    command <- ServerMonad . lift $ receive sock
    liftIO . BS.putStrLn $ command
    ServerMonad . lift . send sock [] $ "sed " <> command
    protocol

-- | Process a _notify_ message from the client, checking the
notify
    :: ChangeNotification
    -> ServerMonad z ()
notify _ = return ()

-- | Process a _resolve conflict_ message from the client.
--
-- The selected diff is marked as resolved; and a new diff is composed from the
-- selected operations and added to the work queue.
resolveConflict
    :: DiffID
    -> [ConflictedDiffOpID]
    -> ServerMonad z ()
resolveConflict diff_id op_ids = do
    liftIO . print . unDiffID $ diff_id
    liftIO . print . map unConflictedDiffOpID $ op_ids
    return ()

-- | Fetch the details of outstanding conflicts and return them to the client.
listConflicts
    :: ServerMonad z [(Document, Diff a, DiffID, [(ConflictedDiffOpID, DiffOp a)])]
listConflicts = do
    return []

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
    putStrLn . fromString $
        "Running server on " <> serverCfg ^. cfgConnectionString
    runRetconServer serverCfg protocol
