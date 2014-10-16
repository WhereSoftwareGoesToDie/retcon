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

-- | Description: Functionality related to notifications.
--
-- Through-out this module the term "notification" specifies a single event
-- which retcon has decided to notify a human being about whereas "message"
-- specifies a physical message to be delivered to a human (e.g. an email
-- message).
--
-- A "message" might include multiple "notifications".
module Retcon.Notifications where

import Control.Applicative
import Data.List
import qualified Data.Map as M
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Time
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Network.Mail.Mime
import System.Locale
import Text.StringTemplate
import Text.Trifecta hiding (render, source)

-- | Rules describe the notifications to be included and the processing to be
-- applied when generating messages.
data NotificationRule = NotificationRule
    { ruleEntity  :: Maybe Text
    , ruleSource  :: Maybe Text
    , ruleAddress :: Text
    }
  deriving (Show, Eq)

-- | Parse a list of 'NotificationRule's.
rulesParser
    :: Parser [NotificationRule]
rulesParser = some parseRule
  where
    parseRule = NotificationRule
        <$> (parseEntity <* char ',')
        <*> (parseSource <* char ',')
        <*> parseEmail
    quoted p = char '"' *> p <* char '"'
    parseEntity = optional $ spaces *> quoted (T.pack <$> some alphaNum) <* spaces
    parseSource = optional $ spaces *> quoted (T.pack <$> some alphaNum) <* spaces
    parseEmail  = spaces *> quoted (T.pack <$> some (noneOf "\"")) <* spaces

-- | Records the details of a notification message which may be sent to
-- recipients.
data Notification = Notification
    { noteCreated :: UTCTime -- ^ Timestamp of the event
    , noteEntity  :: Text -- ^ Name of the RetconEntity.
    , noteKey     :: Int  -- ^ 'InternalKey' of the affected object.
    , noteDiff    :: Int  -- ^ ID of the conflicted diff.
    }
  deriving (Show, Eq, Ord)

instance FromRow Notification where
    fromRow = Notification <$> field <*> field <*> field <*> field

-- | The template for a message.
data Template = Template
    { tplSubject :: Text -- ^ Message subject
    , tplMessage :: Text -- ^ Message body
    , tplFrom    :: Address -- ^ Message from address
    }

-- | Generate a human-readable description of a rule.
describeRule
    :: NotificationRule
    -> Text
describeRule NotificationRule{..} =
    case (ruleEntity, ruleSource) of
        (Nothing, Nothing) -> "all notifications"
        (Just entity, Nothing) -> "all " <> entity <> " notifications"
        (Nothing, Just source) -> "all notifications from " <> source
        (Just entity, Just source) -> entity <> " notifications from " <> source

-- | Prepare a 'Mail' message specific to a notification and the rule which
-- matched it.
prepareMessage
    :: Template -- ^ Template for the message.
    -> NotificationRule -- ^ Matching rule
    -> Notification -- ^ Notification
    -> Mail
prepareMessage Template{..} rule@NotificationRule{..} Notification{..} =
    let addr = Address Nothing ruleAddress
        attrs = M.map T.unpack $ M.fromList
            [ ("recipient", ruleAddress)
            , ("sender", addressEmail tplFrom)
            , ("rule", describeRule rule)
            , ("entity", noteEntity)
            , ("key" , T.pack $ show noteKey)
            , ("diff", T.pack $ show noteDiff)
            , ("url", T.pack . intercalate "/" $ [T.unpack noteEntity
                , show noteKey
                , show noteDiff
                ])
            ] :: M.Map String String
        sub = renderTpl tplSubject attrs
        txt = plainPart . LT.fromStrict $ renderTpl tplMessage attrs
        date = T.pack $ formatTime defaultTimeLocale rfc822DateFormat
                        noteCreated
    in addPart [txt] $ (emptyMail tplFrom)
        { mailTo = [addr]
        , mailHeaders = [ ("Subject", sub)
                        , ("Date", date)
                        ]
        }
  where
    renderTpl :: Text -> M.Map String String -> Text
    renderTpl t a = T.pack . render $ withContext (newSTMP . T.unpack $ t) a
