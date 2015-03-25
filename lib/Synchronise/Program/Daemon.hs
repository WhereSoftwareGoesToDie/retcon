--
-- Copyright © 2014-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

-- | Description: Run /Synchronise/ as a server.
module Synchronise.Program.Daemon where

import Control.Concurrent.Async
import System.Log.Logger

import Synchronise.Configuration
import Synchronise.Network.Server

-- | Start the synchronise daemon.
synchronise
    :: Configuration
    -> IO ()
synchronise cfg = do
    updateGlobalLogger rootLoggerName (setLevel . snd . configServer $ cfg)
    api <- spawnApiServer cfg
    wait api
