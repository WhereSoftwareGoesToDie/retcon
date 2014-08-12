module Retcon.Config where

import Retcon.DataSource

data RetconConfig =
    RetconConfig { retconEntities :: [SomeEntity] }

