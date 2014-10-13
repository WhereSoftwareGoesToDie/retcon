--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Description: Command-line tool to process and dispatch retcon notifications.
module Main where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Data.Time
import Data.Time.Clock
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.FromRow as PG
import qualified Database.PostgreSQL.Simple.Notification as PG
import Network.Mail.Mime
import Options.Applicative
import qualified Options.Applicative as O
import Options.Applicative.Types (readerAsk)
import System.Locale
import Text.Trifecta hiding (Parser)

import Retcon.Notifications

-- | Configuration for 
data Config = Config
    { cfgDB :: BS.ByteString
    , cfgRules :: FilePath
    , cfgSubject :: Text
    , cfgTemplate :: FilePath
    , cfgFrom :: Text
    }
  deriving (Show, Ord, Eq)

-- | Command-line options parser for configuration.
configParser :: Parser Config
configParser = Config
    <$> dbParser
    <*> rulesParser
    <*> subjParser
    <*> tplParser
    <*> fromParser
  where
    dbParser = O.option (BS.pack <$> readerAsk)
        ( long "database"
        <> metavar "CONNECTION"
        <> help "retcon database connection string"
        )
    rulesParser = O.option str 
        ( long "rules"
        <> metavar "FILE"
        <> help "message dispatch rules path"
        )
    subjParser = O.option (T.pack <$> readerAsk)
        ( long "subject"
        <> metavar "STRING"
        <> help "email subject template"
        )
    tplParser = O.option str 
        ( long "message"
        <> metavar "FILE"
        <> help "email message template path"
        )
    fromParser = O.option (T.pack <$> readerAsk)
        ( long "from"
        <> metavar "ADDRESS"
        <> help "email from address"
        )

main :: IO ()
main = execParser opts >>= send
  where
    opts = info (helper <*> configParser)
        ( fullDesc
        <> progDesc "Send email message for queued retcon notifications"
        <> header "send-notifications - send notifications")

send :: Config -> IO ()
send Config{..} = do
    msg <- T.pack <$> readFile cfgTemplate
    Just rules <- parseFromFile rulesParser cfgRules
    let tpl = Template cfgSubject msg (Address Nothing cfgFrom)

    conn <- PG.connectPostgreSQL cfgDB
    t <- getCurrentTime

    res <- PG.query conn "SELECT created, entity, id, diff_id FROM retcon_notifications WHERE created < ?" (PG.Only t)
    process tpl rules res

    PG.close conn

-- | Process a batch of messages, dispatching messages which match each rule.
process
    :: Template
    -> [NotificationRule]
    -> [Notification]
    -> IO ()
process tpl rules res = do
    forM_ rules $ \rule -> do
        mapM_ (renderMail' >=> LBS.putStrLn) $ map (prepareMessage tpl rule) res

-- | Prepare and dispatch a message.
dispatch
    :: Template
    -> NotificationRule
    -> Notification
    -> IO ()
dispatch tpl rule note = return ()
