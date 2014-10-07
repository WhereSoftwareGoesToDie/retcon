--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

-- | Description: Options for running retcon from CLI and config files.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Retcon.Options where

import Control.Monad hiding (sequence)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable
import Options.Applicative hiding (Parser, option)
import Options.Applicative.Types(readerAsk)
import qualified Options.Applicative as O
import Prelude hiding (sequence)
import System.Directory
import System.FilePath
import Text.Trifecta
import Data.Traversable

import Utility.Configuration

-- | Logging destinations.
data Logging =
      LogStderr
    | LogStdout
    | LogNone
  deriving (Eq, Read)

instance Show Logging where
    show LogStderr = "stderr"
    show LogStdout = "stdout"
    show LogNone   = "none"

-- | Options to control the operation of retcon.
data RetconOptions =
    RetconOptions {
          optVerbose :: Bool
        , optLogging :: Logging
        , optDB      :: ByteString
        , optParams  :: Maybe (Either FilePath (Map (Text, Text) (Map Text Text)))
        , optArgs    :: [Text]
    }
  deriving (Show, Eq)

-- | Default options which probably won't let you do much of anything.
defaultOptions :: RetconOptions
defaultOptions = RetconOptions False LogNone "" Nothing []

-- * Configuration

-- | Parse options from a config file and/or the command line.
parseArgsWithConfig :: FilePath -> IO RetconOptions
parseArgsWithConfig = parseFile >=> execParser . helpfulParser >=> includeParams

-- | Load the parameters from the path specified in the options.
includeParams :: RetconOptions -> IO RetconOptions
includeParams opt = do
    params <- sequence $ (either readParams return) <$> optParams opt
    return $ opt { optParams = Right <$> params }
  where
    readParams :: FilePath -> IO (Map (Text, Text) (Map Text Text))
    readParams path = do
        results <- parseFromFile configParser path
        return $ maybe mempty convertConfig results

-- * Options parsers

-- | Parse options from the command line.
helpfulParser :: RetconOptions -> ParserInfo RetconOptions
helpfulParser os = info (helper <*> optionsParser os) fullDesc

-- | Applicative parser for 'RetconOptions', including entity details.
optionsParser :: RetconOptions -> O.Parser RetconOptions
optionsParser def = confOptionsParser def <*> parseID
  where
    parseID :: O.Parser [Text]
    parseID = sequenceA
        [ argument txt (metavar "ENTITY")
        , argument txt (metavar "SOURCE")
        , argument txt (metavar "ID")
        ]

    txt :: ReadM Text
    txt = fmap T.pack readerAsk

-- | Applicative parser for 'RetconOptions', including entity details.
optionsParser' :: RetconOptions -> O.Parser RetconOptions
optionsParser' def = confOptionsParser def <*> pure []

-- | Applicative parser for the configuration components of 'RetconOptions'.
confOptionsParser :: RetconOptions -> O.Parser ([Text] -> RetconOptions)
confOptionsParser RetconOptions{..} =
    RetconOptions <$> parseVerbose
                  <*> parseLogging
                  <*> parseDB
                  <*> parseParams
  where
    parseVerbose :: O.Parser Bool
    parseVerbose = switch $
           long "verbose"
        <> short 'v'
        <> help "Produce verbose output"
    parseDB :: O.Parser ByteString
    parseDB = O.option (BS.pack <$> readerAsk) $
           long "db"
        <> short 'd'
        <> metavar "DATABASE"
        <> O.value optDB
        <> showDefault
        <> help "PostgreSQL connection string"
    parseLogging :: O.Parser Logging
    parseLogging = O.option (readerAsk >>= readLog) $
           long "log"
        <> short 'l'
        <> metavar "stderr|stdout|none"
        <> help "Log messages to an output"
        <> O.value optLogging
        <> showDefault
    parseParams :: O.Parser (Maybe (Either FilePath (Map (Text, Text) (Map Text Text))))
    parseParams = O.option (Just . Left <$> readerAsk) $
           long "parameters"
        <> metavar "FILE"
        <> help "Data source parameters"
        <> showDefault

-- | Reader for logging options.
readLog :: (Monad m, MonadPlus m) => String -> m Logging
readLog "stderr" = return LogStderr
readLog "stdout" = return LogStdout
readLog "none"   = return LogNone
readLog _        = mzero

-- * Config file parsers

-- | Parse options from a config file.
parseFile :: FilePath -> IO RetconOptions
parseFile path = do
    exists <- doesFileExist path
    cfg <- if exists
            then parseFromFile simpleConfigParser path
            else return Nothing
    return $ maybe defaultOptions (`mergeConfig` defaultOptions) cfg
  where
    mergeConfig ls RetconOptions{..} = fromJust $
        RetconOptions <$> pure optVerbose
                      <*> (lookup "logging" ls >>= readLog) `mplus` pure optLogging
                      <*> liftM BS.pack (lookup "database" ls) `mplus` pure optDB
                      <*> (Just . Left <$> lookup "parameters" ls) `mplus` pure optParams
                      <*> pure []

    simpleConfigParser :: Parser [(String, String)]
    simpleConfigParser = some $ liftA2 (,)
        (spaces *> possibleKeys <* spaces <* char '=')
        (spaces *> (stringLiteral <|> stringLiteral'))

    possibleKeys :: Parser String
    possibleKeys = string "logging" <|> string "database" <|> string "parameters"
