--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Server component for the retcon network API.
module Retcon.Network.Server where

import Control.Applicative
import Control.Lens.Operators
import Control.Lens.TH
import Data.Monoid
import Data.String
import Options.Applicative

import Retcon.Core
import Retcon.Network.WireFormat
import Retcon.Options

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

-- | Start a server running the retcon API over a ZMQ socket.
runAPIServer
    :: WritableToken store
    => RetconConfig InitialisedEntity store
    -> ServerConfig
    -> IO ()
runAPIServer retconCfg serverCfg = do
    putStrLn . fromString $
        "Running server on " <> serverCfg ^. cfgConnectionString 
