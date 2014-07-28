{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeSynonymInstances #-}
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
import qualified Data.Map            as M
import           Data.Text           (Text)
import qualified Data.Text           as T

import Data.Tree.EdgeLabelled

-- | A retcon 'Document' is an edge-labelled tree with 'Text' labels on
-- both nodes and edges.
type Document = Tree DocumentKey DocumentValue
type DocumentKey = Text
type DocumentValue = Text

instance FromJSON Document where
  parseJSON (String str) = pure $ Node (Just str) M.empty
  parseJSON (Number num) = pure $ Node (Just $ T.pack $ show num) M.empty
  parseJSON (Bool True)  = pure $ Node (Just "TRUE") M.empty
  parseJSON (Bool False) = pure $ Node (Just "FALSE") M.empty
  parseJSON (Null)       = pure $ Node Nothing M.empty
  parseJSON (Array _)    = mzero -- TODO Maybe convert into a map?
  parseJSON (Object v)   = do
    let kvs = H.toList v
    kvs' <- mapM (\(k,v') -> return . (k,) =<< parseJSON v') kvs
    return $ Node Nothing (M.fromList kvs')

-- TODO This instance will discard information when it encounters a
-- node with a label *and* children.
instance ToJSON Document where
  toJSON (Node Nothing  children)
    = object $ map (\(k,v) -> k .= v) $ M.toAscList children
  toJSON (Node (Just val) children)
    | M.null children = toJSON val
    | otherwise       = object $ map (\(k,v) -> k .= v) $ M.toAscList children

-- | An empty document.
emptyDocument :: Document
emptyDocument = emptyTree

