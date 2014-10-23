module Main where

import Control.Applicative
import Options.Applicative

import Retcon.Core
import Retcon.Options
import Retcon.Network.Server
import Retcon.Store.PostgreSQL

entities :: [SomeEntity]
entities = []

apiParser
    :: RetconOptions
    -> Parser (RetconOptions, ServerConfig)
apiParser defaults =
    (,) <$> optionsParser defaults
        <*> serverParser

main :: IO ()
main = do
    -- Initialise configuration.
    (opts, network) <- parseOptionsWithDefault apiParser "/etc/retcon.conf"
    cfg <- prepareConfig (opts, []) entities

    apiServer cfg network