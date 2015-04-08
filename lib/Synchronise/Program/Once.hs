{-# LANGUAGE RankNTypes #-}

--
-- Copyright © 2014-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

-- | Description: Run /Synchronise/ as a one-short command.
module Synchronise.Program.Once where

import Control.Lens (over, _Right)
import Control.Monad.IO.Class
import Data.Aeson

import Synchronise.Configuration
import Synchronise.DataSource
import Synchronise.Document
import Synchronise.Identifier
import Synchronise.Monad

-- | A request to be processed.
data Request
    = Create { commandKey :: ForeignKey }
    | Read   { commandKey :: ForeignKey }
    | Update { commandKey :: ForeignKey }
    | Delete { commandKey :: ForeignKey }
  deriving (Eq, Show)

-- | A universally wrapped response. No need for associated types here.
--
data Response
    = RespKey ForeignKey
    | RespDoc Document
    | RespUnit ()
  deriving (Eq)

instance Show Response where
  show (RespKey k) = show k
  show (RespDoc d) = show d
  show (RespUnit _) = ""

-- | Run a single command.
--
synchroniseOnce
    :: Request
    -> Configuration
    -> SynchroniseMonad (Either DataSourceError Response)
synchroniseOnce req cfg = do
    let rk = commandKey req
    ds <- either error return $ getDataSource cfg (fkEntity rk) (fkSource rk)

    case req of
        Create fk -> do
          d <- inputDocument fk
          k <- runDSMonad $ createDocument ds d
          return (over _Right RespKey k)
        Read   fk -> do
          d <- runDSMonad $ readDocument ds fk
          return (over _Right RespDoc d)
        Update fk -> do
          d <- inputDocument rk
          k <- runDSMonad $ updateDocument ds fk d
          return (over _Right RespKey k)
        Delete fk -> do
          u <- runDSMonad $ deleteDocument ds fk
          return (over _Right RespUnit u)

-- | Likes `synchroniseOnce`, but prints the results to stdout or aborts if error.
--
synchroniseOnce_
    :: Request
    -> Configuration
    -> SynchroniseMonad ()
synchroniseOnce_ req conf = do
  result <- synchroniseOnce req conf
  case result of
    Left  e -> error (show e)
    Right v -> liftIO (print v)

-- | Read JSON from standard input and produce a 'Document'.
inputDocument
    :: MonadIO m
    => ForeignKey
    -> m Document
inputDocument fk =
    let e = fkEntity fk
        s = fkSource fk
    in return $ Document e s Null
