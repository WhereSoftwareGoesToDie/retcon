{-# LANGUAGE DeriveFunctor #-}
module Retcon.Diff where

import qualified Data.Text as T
import           Data.Text (Text)

data Diff l = Diff l [Operation l]
  deriving (Eq, Show, Functor)
data Operation l
  = InsOp l [Text] Text
  | DelOp l [Text]
  deriving (Eq, Show, Functor)
