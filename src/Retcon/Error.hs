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
    | RetconNotSupported String -- ^ An unsupported configuration.
    | RetconUnknown String -- ^ Unknown entity, source, or document.
  deriving (Show)

instance Eq RetconError where

    (RetconFailed)          == (RetconFailed)          = True
    (RetconError e1)        == (RetconError e2)        = False
    (RetconDBError s1)      == (RetconDBError s2)      = s1 == s2
    (RetconSourceError s1)  == (RetconSourceError s2)  = s1 == s2
    (RetconNotSupported s1) == (RetconNotSupported s2) = s1 == s2
    (RetconUnknown s1)      == (RetconUnknown s2)      = s1 == s2
    _ == _ = False

