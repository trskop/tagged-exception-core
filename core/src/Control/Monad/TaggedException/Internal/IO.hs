{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
-- |
-- Module:       $HEADER$
-- Description:  Masking of asynchronous exceptions.
-- Copyright:    (c) 2011 - 2013 Peter Trsko
-- License:      BSD3
--
-- Stability:    provisional
-- Portability:  non-portable (CPP, RankNTypes)
--
-- Compatibility abstraction over extensible exception's interace.
module Control.Monad.TaggedException.Internal.IO (mask, mask')
    where

#if MIN_VERSION_base(4,3,0)
import qualified Control.Exception as IOE (mask)
#else
import qualified Control.Exception as IOE (block, unblock)
#endif

import Control.Monad.TaggedException.Internal (Throws)
import qualified Control.Monad.TaggedException.Internal as Unsafe
    (hideOne, throwsOne)


-- | Wrapper for @mask@ (or @\ f -> block $ f unblock@ for base <= 4.3) from
-- @Control.Exception@.
mask' :: ((forall a. IO a -> IO a) -> IO b) -> IO b
#if MIN_VERSION_base(4,3,0)
mask' = IOE.mask
#else
mask' f = IOE.block $ f IOE.unblock
#endif

-- | Lifted 'mask''.
mask
    :: ((forall a. Throws e IO a -> Throws e IO a) -> Throws e IO b)
    -> Throws e IO b
#if MIN_VERSION_base(4,3,0)
mask f = Unsafe.throwsOne . IOE.mask $ \ restore ->
    Unsafe.hideOne $ f (Unsafe.throwsOne . restore . Unsafe.hideOne)
#else
mask f = Unsafe.throwsOne . IOE.block . Unsafe.hideOne
    $ f (Unsafe.throwsOne . IOE.unblock . Unsafe.hideOne)
#endif
