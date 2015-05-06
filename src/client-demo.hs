{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import qualified Data.ByteString.Char8 as BS
import Data.Monoid

import System.Environment
import System.IO
import System.ZMQ4.Monadic

import Retcon.Network.Client
import Retcon.Network.Server

main :: IO ()
main = do
    conn:_ <- getArgs

    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering

    -- Try to retrieve a list of conflicts.
    val <- runRetconZMQ conn getConflicted
    case val of
        Left  e -> putStr ":-( " >> print e
        Right l -> print l >> putStrLn ":-)"

    -- Try to send a "something changed" notification.
    let change = ChangeNotification "LOL" "no u" "123"
    val2 <- runRetconZMQ conn $ enqueueChangeNotification change
    case val2 of
        Left  e -> putStr ":-( " >> print e
        Right _ -> putStrLn ":-)"

-- | Squirt arbitrary bytes down a ZMQ channel.
sentBytes
    :: String
    -> IO ()
sentBytes conn =
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
