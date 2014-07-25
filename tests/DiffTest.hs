{-# LANGUAGE OverloadedStrings #-}
module Main where

import Retcon.Diff
import System.Exit

testDiff :: Diff (Int, String)
testDiff = Diff (1, "hello")
    [ InsOp (2, "never") ["name"] "Thomas"
    , InsOp (2, "gonna") ["name"] "Thomas"
    , InsOp (2, "give") ["name"] "Thomas"
    , InsOp (2, "you") ["name"] "Thomas"
    , InsOp (2, "up") ["name"] "Thomas"
    ]

main :: IO ()
main = do
    let diff' = fmap fst testDiff :: Diff Int
    putStrLn "Testing diff"

