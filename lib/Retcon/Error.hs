--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE DeriveDataTypeable #-}

module Retcon.Error where

import Control.Exception
import Data.Typeable

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
  deriving (Show, Typeable)

instance Exception RetconError

