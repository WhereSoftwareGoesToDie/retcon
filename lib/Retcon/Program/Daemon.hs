-- | Description: Run /Retcon/ as a server.
module Retcon.Program.Daemon where

import           System.Log.Logger

import           Retcon.Configuration
import           Retcon.Network.Server

-- | Start the retcon daemon.
retcon
    :: Configuration
    -> IO ()
retcon cfg = do
    let (_, pri, _) = configServer cfg
    updateGlobalLogger rootLoggerName (setLevel pri)
    spawnServer cfg 1
