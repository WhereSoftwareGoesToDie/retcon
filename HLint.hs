{-# LANGUAGE PackageImports  #-}
{-# LANGUAGE TemplateHaskell #-}
module HLint.HLint where

import           "hint" HLint.Builtin.All
import           "hint" HLint.Default
import           "hint" HLint.Dollar
import           "hint" HLint.Generalise

ignore "Reduce duplication"
ignore "Use ."
ignore "Use fmap"
ignore "Use if"
ignore "Use import/export shortcut"
ignore "Use liftM"
ignore "Use section"
