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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}

module Retcon.Document where

import Control.Applicative
import Control.Lens hiding ((.=))
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

mkNode :: Applicative f => Maybe DocumentValue -> f Document
mkNode x = pure . Document $ Node x mempty

instance FromJSON Document where
  parseJSON (String str) = mkNode $ Just str
  parseJSON (Number num) = mkNode . Just . T.pack $ show num
  parseJSON (Bool True)  = mkNode $ Just "TRUE"
  parseJSON (Bool False) = mkNode $ Just "FALSE"
  parseJSON (Null)       = mkNode Nothing
  parseJSON (Array _)    = mzero -- TODO Maybe convert into a map?
  parseJSON (Object v)   =
    Document . Node Nothing . M.fromList . H.toList <$>
        traverse (\x -> unDocument <$> parseJSON x) v

-- TODO This instance will discard information when it encounters a
-- node with a label *and* children.
instance ToJSON Document where
  toJSON (Document (Node Nothing childs))
    = object $ map (\(k,v) -> k .= Document v) $ M.toAscList childs
  toJSON (Document (Node (Just val) childs))
    | M.null childs = toJSON val
    | otherwise     = object $ map (\(k,v) -> k .= Document v) $ M.toAscList childs
