{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
-- |
-- Module:       $HEADER$
-- Description:  Utility functions built on top of core API.
-- Copyright:    (c) 2009-2015, Peter Trško
-- License:      BSD3
--
-- Stability:    provisional
-- Portability:  NoImplicitPrelude, RankNTypes
--
-- Utility functions built on top of core API.
--
-- Functions from this module were moved in to
-- "Control.Monad.TaggedException.Core", and this module became deprecated.
--
-- For compatibility reasons this module re-exports all functions that were
-- previously defined here, but this module will be removed in the future.
-- Please update your code to use "Control.Monad.TaggedException.Core" instead.
module Control.Monad.TaggedException.Utilities
    {-# DEPRECATED "Use module Control.Monad.TaggedException.Core instead." #-}
    ( bracket
    , bracket'
    , bracket_
    , bracketOnError
    , bracketOnError'
    , finally
    , finally'
    )
  where

import Control.Monad.TaggedException.Core
    ( bracket
    , bracket'
    , bracket_
    , bracketOnError
    , bracketOnError'
    , finally
    , finally'
    )
