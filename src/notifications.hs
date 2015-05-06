--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Description: Command-line tool to process and dispatch retcon notifications.
module Main where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import qualified Database.PostgreSQL.Simple as PG
import Network.Mail.Mime
import Options.Applicative
import qualified Options.Applicative as O
import Options.Applicative.Types (readerAsk)
import Text.Trifecta hiding (Parser)

import Retcon.Notifications

-- | Configuration for
data Config = Config
    { cfgDB       :: BS.ByteString
    , cfgRules    :: FilePath
    , cfgSubject  :: Text
    , cfgTemplate :: FilePath
    , cfgFrom     :: Text
    }
  deriving (Show, Ord, Eq)

-- | Command-line options parser for configuration.
configParser :: Parser Config
configParser = Config
    <$> db_parser
    <*> rules_parser
    <*> subj_parser
    <*> tpl_parser
    <*> from_parser
  where
    db_parser = O.option (BS.pack <$> readerAsk)
        ( long "database"
        <> metavar "CONNECTION"
        <> help "retcon database connection string"
        )
    rules_parser = O.option str
        ( long "rules"
        <> metavar "FILE"
        <> help "message dispatch rules path"
        )
    subj_parser = O.option (T.pack <$> readerAsk)
        ( long "subject"
        <> metavar "STRING"
        <> help "email subject template"
        )
    tpl_parser = O.option str
        ( long "message"
        <> metavar "FILE"
        <> help "email message template path"
        )
    from_parser = O.option (T.pack <$> readerAsk)
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

    void $ PG.execute conn "DELETE FROM retcon_notifications WHERE created < ?" (PG.Only t)

    PG.close conn

-- | Process a batch of messages, dispatching messages which match each rule.
process
    :: Template
    -> [NotificationRule]
    -> [Notification]
    -> IO ()
process tpl rules res =
    forM_ rules $ \rule ->
        mapM_ (renderSendMail . prepareMessage tpl rule) res
