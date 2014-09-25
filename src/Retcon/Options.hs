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

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative hiding (Parser, option)
import qualified Options.Applicative as O
import System.Directory
import Text.Trifecta

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
        , optArgs    :: [Text]
    }
  deriving (Show, Eq)

-- | Default options which probably won't let you do much of anything.
defaultOptions :: RetconOptions
defaultOptions = RetconOptions False LogNone "" []

-- * Configuration

-- | Parse options from a config file and/or the command line.
parseArgsWithConfig :: FilePath -> IO RetconOptions
parseArgsWithConfig = parseFile >=> execParser . helpfulParser

-- * Options parsers

-- | Parse options from the command line.
helpfulParser :: RetconOptions -> ParserInfo RetconOptions
helpfulParser os = info (helper <*> optionsParser os) fullDesc

-- | Applicative parser for 'RetconOptions', including entity details.
optionsParser :: RetconOptions -> O.Parser RetconOptions
optionsParser def = confOptionsParser def <*> parseID
  where
    parseID = (\x y z -> [x,y,z])
        <$> argument (return . T.pack) (metavar "ENTITY")
        <*> argument (return . T.pack) (metavar "SOURCE")
        <*> argument (return . T.pack) (metavar "ID")

-- | Applicative parser for 'RetconOptions', including entity details.
optionsParser' :: RetconOptions -> O.Parser RetconOptions
optionsParser' def = confOptionsParser def <*> pure []

-- | Applicative parser for the configuration components of 'RetconOptions'.
confOptionsParser :: RetconOptions -> O.Parser ([Text] -> RetconOptions)
confOptionsParser RetconOptions{..} =
    RetconOptions <$> parseVerbose
                  <*> parseLogging
                  <*> parseDB
  where
    parseVerbose :: O.Parser Bool
    parseVerbose = switch $
           long "verbose"
        <> short 'v'
        <> help "Produce verbose output"
    parseDB :: O.Parser ByteString
    parseDB = O.option (return . BS.pack) $
           long "db"
        <> short 'd'
        <> metavar "DATABASE"
        <> O.value optDB
        <> showDefault
        <> help "PostgreSQL connection string"
    parseLogging :: O.Parser Logging
    parseLogging = O.option readLog $
           long "log"
        <> short 'l'
        <> metavar "stderr|stdout|none"
        <> help "Log messages to an output"
        <> O.value optLogging
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
    if exists
    then maybe defaultOptions (`mergeConfig` defaultOptions) <$> parseFromFile configParser path
    else return defaultOptions
  where
    mergeConfig ls RetconOptions{..} = fromJust $
        RetconOptions <$> pure optVerbose
                      <*> (lookup "logging" ls >>= readLog) `mplus` pure optLogging
                      <*> liftM BS.pack (lookup "database" ls) `mplus` pure optDB
                      <*> pure []

    configParser :: Parser [(String, String)]
    configParser = some $ liftA2 (,)
        (spaces *> possibleKeys <* spaces <* char '=')
        (spaces *> (stringLiteral <|> stringLiteral'))

    possibleKeys :: Parser String
    possibleKeys = string "logging" <|> string "database"
