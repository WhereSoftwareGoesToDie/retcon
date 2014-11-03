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
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

module Retcon.Options where

import Control.Lens (to)
import Control.Lens.Operators
import Control.Lens.TH
import Control.Monad hiding (sequence)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable
import Options.Applicative hiding (Parser, option)
import qualified Options.Applicative as O
import Options.Applicative.Types (readerAsk)
import Prelude hiding (sequence)
import System.Directory
import Text.Trifecta as P

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
data RetconOptions = RetconOptions
    { _optVerbose :: Bool
    , _optLogging :: Maybe Logging
    , _optDB      :: BS.ByteString
    , _optParams  :: Maybe FilePath
    }
  deriving (Show, Eq)
makeLenses ''RetconOptions

-- | Default options which probably won't let you do much of anything.
defaultOptions :: RetconOptions
defaultOptions = RetconOptions False Nothing "" Nothing

-- | To combine multiple RectonOptions. This is left-preserving.
instance Monoid RetconOptions where
    mempty = defaultOptions
    opt1 `mappend` opt2 =
        opt1 & optVerbose %~ (|| (opt2 ^. optVerbose))
             & optLogging %~ (`mplus` (opt2 ^. optLogging))
             & optDB %~ (if opt1 ^. optDB . to BS.null
                         then const $ opt2 ^. optDB
                         else id)
             & optParams %~ (`mplus` (opt2 ^. optParams))

-- * Configuration

-- | Parse options from a config file and/or the command line.
parseArgsWithConfig :: FilePath -> IO (RetconOptions, [Text])
parseArgsWithConfig = parseOptionsWithDefault opts
  where
    opts = (,) <$> optionsParser
               <*> eventParser

-- | Run an options parser which takes its default arguments from a file.
parseOptionsWithDefault
    :: O.Parser (RetconOptions,a)
    -> FilePath
    -> IO (RetconOptions,a)
parseOptionsWithDefault p fp = do
    (fp',(retcon_cfg,x)) <- execParser . helpful $ (,) <$> parseConfig <*> p
    (,x) <$> (mappend retcon_cfg <$> parseFile fp')
  where
    helpful parser = info (helper <*> parser) fullDesc
    parseConfig :: O.Parser FilePath
    parseConfig = O.option readerAsk $
           long "config"
        <> metavar "CONFIG"
        <> help "Retcon config file"
        <> O.value fp
        <> showDefault

-- * Options parsers

-- | Applicative parser for 'RetconOptions', including entity details.
optionsParser
    :: O.Parser RetconOptions
optionsParser =
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
        <> O.value (mempty ^. optDB)
        <> showDefault
        <> help "PostgreSQL connection string"
    parseLogging :: O.Parser (Maybe Logging)
    parseLogging = O.option (readLog <$> readerAsk) $
           long "log"
        <> short 'l'
        <> metavar "stderr|stdout|none"
        <> help "Log messages to an output"
        <> O.value (mempty ^. optLogging)
        <> showDefault
    parseParams :: O.Parser (Maybe FilePath)
    parseParams = O.option (Just <$> readerAsk) $
           long "parameters"
        <> metavar "FILE"
        <> help "Data source parameters"
        <> O.value (mempty ^. optParams)
        <> showDefault

-- | Parse a description of a retcon event.
eventParser :: O.Parser [Text]
eventParser = sequenceA
    [ argument txt (metavar "ENTITY")
    , argument txt (metavar "SOURCE")
    , argument txt (metavar "ID")
    ]
  where
    txt :: ReadM Text
    txt = fmap T.pack readerAsk

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
            then parseFromFileEx simpleConfigParser path
            else error $ "specified config file doesn not exist: \"" ++ path ++ "\""
    case cfg of
        P.Success cfg' -> return $ mergeConfig cfg' defaultOptions
        P.Failure failure -> error $ show failure
  where
    mergeConfig ls RetconOptions{..} = fromJust $
        RetconOptions <$> pure _optVerbose
                      <*> (readLog <$> lookup "logging" ls) `mplus` pure _optLogging
                      <*> liftM BS.pack (lookup "database" ls) `mplus` pure _optDB
                      <*> (Just <$> lookup "parameters" ls) `mplus` pure _optParams

    simpleConfigParser :: Parser [(String, String)]
    simpleConfigParser = some $ liftA2 (,)
        (spaces *> possibleKeys <* spaces <* char '=')
        (spaces *> (stringLiteral <|> stringLiteral'))

    possibleKeys :: Parser String
    possibleKeys = string "logging" <|> string "database" <|> string "parameters"

-- * Run-time configuration
--
-- $ Retcon's run-time behaviour is controlled by a 'RetconConfig' value
-- created from the 'RetconOptions' and other details above.

-- | Configuration value for retcon.
data RetconConfig entity store =
    RetconConfig {
          _cfgVerbose  :: Bool
        , _cfgLogging  :: Logging
        , _cfgDB       :: store
        , _cfgParams   :: ParamMap
        , _cfgArgs     :: [Text]
        , _cfgEntities :: [entity]
    }
makeLenses ''RetconConfig
