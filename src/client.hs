{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import Control.Applicative
import Data.Monoid
import Options.Applicative hiding (command)
import qualified Options.Applicative as O
import System.Exit

import Retcon.Network.Client
import Retcon.Network.Server

data Options = Options
    { connection :: String
    , command    :: Command
    }

data Command
    = Notify
        { entity :: String
        , source :: String
        , key    :: String
        }

optionsParser :: Parser Options
optionsParser = Options
    <$> strOption (long "connection" <> help "Server ZMQ connection string")
    <*> subparser (
        O.command "notify" (info pNotify
            (progDesc "Notify retcon of a change to be processed."))
        )
  where
    pNotify = Notify
        <$> strArgument (metavar "ENTITY")
        <*> strArgument (metavar "SOURCE")
        <*> strArgument (metavar "FK")

run :: Options -> IO ()
run Options{..} = do
    val <- runRetconZMQ connection $ case command of
        -- Try to send a "something changed" notification.
        Notify{..} -> enqueueChangeNotification $
            ChangeNotification entity source key
    case val of
        Left  e -> do
            print e
            exitFailure
        Right _ -> do
            exitSuccess

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> optionsParser)
      ( fullDesc
     <> progDesc "Interact with a retcon server."
     <> header "retcon - interact with a retcon server" )
