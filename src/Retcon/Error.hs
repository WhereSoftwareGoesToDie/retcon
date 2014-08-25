module Retcon.Error where

import Control.Exception

-- * Errors

-- | Exceptions which can be raised by actions in the 'RetconHandler' or
-- 'DataSourceAction' monads.
data RetconError =
      RetconFailed
    | RetconDBError String -- ^ Describes an error with the retcon database.
    | RetconSourceError String -- ^ An error occured with a data source.
    | RetconError SomeException -- ^ An unexpected exception was raised.
  deriving (Show)


