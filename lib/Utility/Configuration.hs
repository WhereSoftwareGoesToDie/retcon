--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

-- | Generic configuration handling.

{-# LANGUAGE OverloadedStrings #-}

module Utility.Configuration where

import Control.Applicative
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Text.Trifecta

-- | A map of parameters for DataSource initialisers.
type ParamMap = Map (Text, Text) (Map Text Text)

-- | Parse simple configuration file format.
configParser :: Parser [(Text, Text, Text, Text)]
configParser = some item
  where
    item = (,,,)
        <$> (spaces *> word <* spaces <* char '.')
        <*> (spaces *> word <* spaces <* char '.')
        <*> (spaces *> word <* spaces <* char '=')
        <*> (spaces *> stringLiteral)
    word = T.pack <$> some alphaNum

-- | Convert parsed configuration into a nested map.
convertConfig :: [(Text, Text, Text, Text)] -> ParamMap
convertConfig = foldl add M.empty
  where
    add m (e, s, k, v) =
        let new = M.singleton k v
        in M.alter (Just . maybe new (M.union new)) (e, s) m
