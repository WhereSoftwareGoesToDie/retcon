--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

-- | Server-side API implementation.
module Retcon.Network.Server where

import Retcon.Network.Types

handleHeader
    :: forall request response.
        (Read request, Show response, Handler request response)
    => Header request response
    -> IO ()
handleHeader _ = do
    let req = read "RequestA"
    x <- handle (req :: request)
    print (x :: response)

