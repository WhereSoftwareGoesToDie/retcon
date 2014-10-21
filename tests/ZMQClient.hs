{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import qualified Data.ByteString.Char8 as BS
import Data.Monoid

import System.Environment
import System.IO
import System.ZMQ4.Monadic

main :: IO ()
main = do
    conn:_ <- getArgs

    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    runZMQ $ do
        liftIO . putStrLn $ "Opening socket"
        sock <- socket Req
        liftIO . putStrLn $ "Connecting to " <> conn
        connect sock conn
        liftIO . putStrLn $ "Entering loop"
        loop sock
        liftIO . putStrLn $ "Disconnecting"
        disconnect sock conn
        liftIO . putStrLn $ "Closing socket"
        close sock
  where
    loop
        :: Socket z Req
        -> ZMQ z ()
    loop sock = do
        liftIO $ putStr "Command: "
        command <- liftIO $ BS.pack <$> getLine
        send sock [] command
        reply <- receive sock
        liftIO . BS.putStrLn $ "They said: " <> reply
        loop sock
