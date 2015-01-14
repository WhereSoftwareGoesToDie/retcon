{-# LANGUAGE OverloadedStrings #-}

-- | Description: Program to generate a diff between two JSON documents.
module Main where

import Control.Applicative
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import System.Environment
import Data.Aeson
import Data.Aeson.Diff

instance Show Patch where
    show = unlines . map show . patchOperations

main :: IO ()
main = do
    [f1,f2] <- getArgs
    d1 <- BSL.fromStrict <$> BS.readFile f1
    d2 <- BSL.fromStrict <$> BS.readFile f2

    let Just v1 = decode d1
    let Just v2 = decode d2

    print $ diff v1 v2

