module Retcon.Document where

import           Data.Aeson
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text as T

-- | Nested key/value documents containing 'Text' values.
data Document = Document (Map Text DocValue)
    deriving (Show, Eq, Ord)

-- | Values within documents. 
data DocValue
    = Value Text
    | Subdocument Document
    deriving (Show, Eq, Ord)

instance FromJSON Document where
  parseJSON (Object v) = Ducument 

