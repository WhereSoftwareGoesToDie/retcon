{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
------------------------------------------------------------------------
-- |
-- Module      : Retcon.Document
-- Description : Represent documents which will be processed by retcon.
-- Copyright   : Anchor Systems and others.
-- License     : BSD3
--
-- Maintainer  : Thomas Sutton <me@thomas-sutton.id.au>
-- Stability   : experimental
-- Portability : portable
--
-- This module implements the 'Document' data type which the retcon
-- system manipulates. Documents are, essentially, nested key/value
-- maps.
------------------------------------------------------------------------
module Retcon.Document where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import qualified Data.HashMap.Lazy   as H
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Text           (Text)
import qualified Data.Text           as T

type DocumentKey = Text

-- | A nested key/value document.
data Document = Document (Map DocumentKey DocValue)
  deriving (Show, Eq, Ord)

-- | Values in documents are either atomic values or sub-documents.
data DocValue
  = Value Text
  | Subdocument Document
  deriving (Show, Eq, Ord)

instance FromJSON Document where
  parseJSON (Object v) = do
    let kvs = H.toList v
    kvs' <- mapM (\(k,v') -> return . (k,) =<< parseJSON v') kvs
    return $ Document (M.fromList kvs')
  parseJSON _          = mzero

instance FromJSON DocValue where
  parseJSON x@Object{} = Subdocument <$> parseJSON x
  parseJSON Array{}    = mzero
  parseJSON (String t) = return $ Value t
  parseJSON (Number n) = return $ Value $ T.pack $ show n
  parseJSON (Bool b)   = return (Value $ T.pack $ show b)
  parseJSON Null       = return $ Value ""

-- | An empty document.
emptyDocument :: Document
emptyDocument = Document M.empty
