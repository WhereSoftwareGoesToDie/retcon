--
-- Copyright © 2014-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

-- | Description: Run /Synchronise/ as a one-short command.
module Synchronise.Program.Once where

import Synchronise.Configuration

-- | Start the synchronise daemon.
synchroniseOnce
    :: Configuration
    -> IO ()
synchroniseOnce = print
