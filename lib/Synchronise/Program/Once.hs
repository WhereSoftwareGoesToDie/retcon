{-# LANGUAGE ViewPatterns #-}

--
-- Copyright © 2014-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

-- | Description: Run /Synchronise/ as a one-shot command.
--
module Synchronise.Program.Once
  ( -- * One-shot `synchronise` on documents
    Request(..)
  , synchroniseOnce

    -- * Run store commands
  , runPSQL
  ) where

import Control.Exception
import Control.Monad.IO.Class
import Data.Aeson

import Synchronise.Configuration
import Synchronise.DataSource
import Synchronise.Document
import Synchronise.Identifier
import Synchronise.Store
import Synchronise.Store.PostgreSQL
import Synchronise.Monad


--------------------------------------------------------------------------------

-- * Operations on documents

data Request
    = Create { commandKey :: ForeignKey }
    | Read   { commandKey :: ForeignKey }
    | Update { commandKey :: ForeignKey }
    | Delete { commandKey :: ForeignKey }
  deriving (Eq, Show)

-- | Run a single command on documents.
--
synchroniseOnce
    :: Request
    -> Configuration
    -> SynchroniseMonad ()
synchroniseOnce req cfg = do
    let rk = commandKey req
    ds <- either error return $ getDataSource cfg (fkEntity rk) (fkSource rk)

    case req of
        Create fk -> inputDocument fk >>= exec . createDocument ds
        Read   fk -> exec $ readDocument ds fk
        Update fk -> inputDocument rk >>= exec . updateDocument ds fk
        Delete fk -> exec $ deleteDocument ds fk
  where
    exec :: (MonadIO m, Show a) => DSMonad m a -> m ()
    exec a = do
        res <- runDSMonad a
        case res of
            Left e -> error $ show e
            Right v -> liftIO $ print v

-- | Read JSON from standard input and produce a 'Document'.
inputDocument
    :: MonadIO m
    => ForeignKey
    -> m Document
inputDocument fk =
    let e = fkEntity fk
        s = fkSource fk
    in return $ Document e s Null


--------------------------------------------------------------------------------

-- * Low-level operations on a persistent store.

runPSQL :: (PGStore -> IO a) -> Configuration -> IO a
runPSQL act (configServer -> (_,_,pg_conn))
  = bracket (initBackend (PGOpts pg_conn))
            (closeBackend)
            act
