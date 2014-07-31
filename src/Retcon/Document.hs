--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

-- | Description : Represent documents which will be processed by retcon.
--
-- This module implements the 'Document' data type which the retcon
-- system manipulates. Documents are, essentially, nested key/value
-- maps.

{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Retcon.Document where

import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.HashMap.Lazy as H
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T

import Data.Monoid
import Data.Tree.GenericTrie

-- | A retcon 'Document' is an edge-labelled tree with 'Text' labels on
-- both nodes and edges.
newtype Document
    = Document { unDocument :: Tree DocumentKey DocumentValue }
  deriving (Monoid, Eq, Show)
type DocumentKey =  Text
type DocumentValue = Text

instance FromJSON Document where
  parseJSON (String str) = pure . Document $ Node (Just str) mempty
  parseJSON (Number num) = pure . Document $ Node (Just $ T.pack $ show num) mempty
  parseJSON (Bool True)  = pure . Document $ Node (Just "TRUE") mempty
  parseJSON (Bool False) = pure . Document $ Node (Just "FALSE") mempty
  parseJSON (Null)       = pure . Document $ Node Nothing mempty
  parseJSON (Array _)    = mzero -- TODO Maybe convert into a map?
  parseJSON (Object v)   = do
    let kvs = H.toList v
    kvs' <- mapM (\(k,v') -> return . (k,) . unDocument =<< parseJSON v') kvs
    return . Document $ Node Nothing (M.fromList kvs')

-- TODO This instance will discard information when it encounters a
-- node with a label *and* children.
instance ToJSON Document where
  toJSON (Document (Node Nothing  children))
    = object $ map (\(k,v) -> k .= Document v) $ M.toAscList children
  toJSON (Document (Node (Just val) children))
    | M.null children = toJSON val
    | otherwise       = object $ map (\(k,v) -> k .= Document v) $ M.toAscList children
