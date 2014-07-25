{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Retcon.Document where

import           Data.Aeson
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as H

import Control.Applicative
import Control.Monad

-- | Nested key/value documents containing 'Text' values.
data Document = Document (Map Text DocValue)
    deriving (Show, Eq, Ord)

-- | Values within documents. 
data DocValue
    = Value Text
    | Subdocument Document
    deriving (Show, Eq, Ord)

instance FromJSON Document where
  parseJSON (Object v) = do
    let kvs = H.toList v
    kvs' <- mapM (\(k,v') -> return . (k,) =<< parseJSON v') kvs
    return $ Document (M.fromList kvs')
  parseJSON _ = mzero

instance FromJSON DocValue where
  parseJSON x@Object{} = Subdocument <$> parseJSON x
  parseJSON Array{} = mzero
  parseJSON (String t) = return $ Value t
  parseJSON (Number n) = return $ Value $ T.pack $ show n
  parseJSON (Bool b) = return (Value $ T.pack $ show b)
  parseJSON Null = return $ Value ""
