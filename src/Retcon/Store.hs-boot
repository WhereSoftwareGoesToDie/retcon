
module Retcon.Store where

class RetconStore s

class StoreToken s where
    restrictToken :: s -> ROToken

data ROToken

data RWToken
