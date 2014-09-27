{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
-- |
-- Module:       $HEADER$
-- Description:  MonadExceptionUtilities class and instances.
-- Copyright:    (c) 2009 - 2014 Peter Trsko.
-- License:      BSD3
--
-- Stability:    provisional
-- Portability:  non-portable (NoImplicitPrelude, RankNTypes)
--
-- Introduces 'MonadExceptionUtilities' type class that provides means for
-- overriding default implementation of functions like 'bracket'.
module Control.Monad.TaggedException.Utilities
    (
      bracket
    , bracket'
    , bracket_
    , bracketOnError
    , bracketOnError'
    , finally
    , finally'
    )
    where

import Control.Monad (Monad(return))
import Control.Exception (Exception)
import Data.Function (($), const)

import Control.Monad.Catch (MonadMask)
import qualified Control.Monad.Catch as Exceptions

import Control.Monad.TaggedException.Core
    ( liftT
    , onException
    , onException'
    )
import Control.Monad.TaggedException.Internal.Throws (Throws)
import qualified Control.Monad.TaggedException.Internal.Throws
    as Unsafe (liftMask)


mask' :: MonadMask m => ((forall a. m a -> m a) -> m b) -> m b
mask' = Exceptions.mask

mask
    :: (Exception e, MonadMask m)
    => ((forall a. Throws e m a -> Throws e m a) -> Throws e m b)
    -> Throws e m b
mask = Unsafe.liftMask Exceptions.mask

-- | Run computation afeter another even if exception was thrown. See also
-- 'finally'', 'onException' and 'onException''.
--
-- Default implementation:
--
-- > m `finally` n = mask $ \ restore -> do
-- >     r <- restore m `onException` n
-- >     _ <- liftT n
-- >     return r
finally
    :: (Exception e, MonadMask m)
    => Throws e m a
    -- ^ Computation to run first
    -> m b
    -- ^ Computation to run afterward (even if exception @e@ was raised)
    -> Throws e m a
    -- ^ Returns the result of the first computation
m `finally` n = mask $ \restore -> do
    r <- restore m `onException` n
    _ <- liftT n
    return r

-- | Run computation afeter another even if exception was thrown. See also
-- 'finally', 'onException' and 'onException''.
finally'
    :: MonadMask m
    => m a
    -- ^ Computation to run first
    -> m b
    -- ^ Computation to run afterward (even if some exception was raised)
    -> m a
    -- ^ Returns the result of the first computation
finally' = Exceptions.finally

-- | Run computation surrounded by acquire and release computations. The
-- release computation is executed even if \"in-between\" computation
-- raises exception. See also 'bracket'', 'bracket_', 'bracketOnError',
-- and 'bracketOnError''.
bracket
    :: (Exception e, MonadMask m)
    => m a
    -- ^ Computation to run before
    -> (a -> m b)
    -- ^ Computation to run after
    -> (a -> Throws e m c)
    -- ^ Computation to run in-between
    -> Throws e m c
    -- ^ Result of the in-between computation
bracket acq rel go = mask $ \ restore -> do
    x <- liftT acq
    r <- restore (go x) `onException` rel x
    _ <- liftT $ rel x
    return r

-- | Run computation surrounded by acquire and release computations. The
-- release computation is executed even if \"in-between\" computation
-- raises exception. See also 'bracket', 'bracket_', 'bracketOnError', and
-- 'bracketOnError''.
--
-- Default implementation:
--
-- > bracket' acq rel go = mask' $ \ restore -> do
-- >     x <- acq
-- >     r <- restore (go x) `onException'` rel x
-- >     _ <- rel x
-- >     return r
bracket'
    :: MonadMask m
    => m a
    -- ^ Computation to run before
    -> (a -> m b)
    -- ^ Computation to run after
    -> (a -> m c)
    -- ^ Computation to run in-between
    -> m c
    -- ^ Result of the in-between computation
bracket' = Exceptions.bracket

-- | Version of 'bracket' where \"after\" computation is executed only if
-- \"in-between\" computation raises exception.
--
-- Default implementation:
--
-- > bracketOnError acq rel go = mask $ \ restore -> do
-- >     x <- liftT acq
-- >     restore (go x) `onException` rel x
bracketOnError
    :: (Exception e, MonadMask m)
    => m a
    -- ^ Computation to run before
    -> (a -> m b)
    -- ^ Computation to run after if an exception was raised
    -> (a -> Throws e m c)
    -- ^ Computation to run in-between
    -> Throws e m c
    -- ^ Result of the in-between computation
bracketOnError acq rel go = mask $ \ restore -> do
    x <- liftT acq
    restore (go x) `onException` rel x

-- | Version of 'bracket' where \"after\" computation is executed only if
-- \"in-between\" computation raises exception.
--
-- Default implementation:
--
-- > bracketOnError' acq rel go = mask' $ \ restore -> do
-- >     x <- liftT acq
-- >     restore (go x) `onException'` rel x
bracketOnError'
    :: MonadMask m
    => m a
    -- ^ Computation to run before
    -> (a -> m b)
    -- ^ Computation to run after if an exception was raised
    -> (a -> m c)
    -- ^ Computation to run in-between
    -> m c
    -- ^ Result of the in-between computation
bracketOnError' acq rel go = mask' $ \ restore -> do
    x <- acq
    restore (go x) `onException'` rel x

-- | Variant of 'bracket'.
--
-- > bracket_ acq rel go = bracket acq (const rel) (const go)
bracket_
    :: (Exception e, MonadMask m)
    => m a
    -- ^ Computation to run before
    -> m b
    -- ^ Computation to run after
    -> Throws e m c
    -- ^ Computation to run in-between
    -> Throws e m c
    -- ^ Result of the in-between computation
bracket_ acq rel go = bracket acq (const rel) (const go)
