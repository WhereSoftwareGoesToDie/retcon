module Retcon.Error where

import Control.Exception

-- * Errors

-- | Exceptions which can be raised by actions in the 'RetconHandler' or
-- 'DataSourceAction' monads.
data RetconError =
      RetconFailed --
    | RetconError SomeException --
  deriving (Show)


