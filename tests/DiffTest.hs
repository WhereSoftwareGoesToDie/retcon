{-# LANGUAGE OverloadedStrings #-}
module Main where

import Retcon.Diff
import System.Exit

testDiff :: Diff (Int, String)
testDiff = Diff (1, "hello")
    [ InsertOp (2, "never") ["name"] "Thomas"
    , InsertOp (2, "gonna") ["name"] "Thomas"
    , InsertOp (2, "give") ["name"] "Thomas"
    , InsertOp (2, "you") ["name"] "Thomas"
    , InsertOp (2, "up") ["name"] "Thomas"
    ]

main :: IO ()
main = do
    let diff' = fmap fst testDiff :: Diff Int
    putStrLn "Testing diff"

