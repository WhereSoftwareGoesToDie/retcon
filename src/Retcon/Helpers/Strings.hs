{-# LANGUAGE OverloadedStrings #-}

module Retcon.Helpers.Strings (
  bsLazyPackedW8s,
  bytesToString
  ) where

import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Char8 as Char8
import GHC.Word (Word8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BS8L
import Data.Word
import Data.Char

bsLazyPackedW8s :: String -> BSL.ByteString
bsLazyPackedW8s = BSL.pack . strToWord8s

strToWord8s :: String -> [Word8]
strToWord8s = BSI.unpackBytes . Char8.pack

-- | stolen from Language.Haskell.TH.Ppr
bytesToString :: [Word8] -> String
bytesToString = map (chr . fromIntegral)
