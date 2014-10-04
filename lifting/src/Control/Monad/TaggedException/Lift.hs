{-# LANGUAGE CPP #-}
-- |
-- Module:       $HEADER$
-- Description:  Commonly used lifting operations mostly from different kinds
--               of error handling.
-- Copyright:    (c) 2011 - 2014 Peter Trško.
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  CPP
module Control.Monad.TaggedException.Lift
    (
    -- * Exception lifting
      liftIOT
    , liftMaybe
    , liftEither
    , liftEitherWith

    -- * Applicative and Monadic lifting
    --
    -- | These functions are more generic and have only 'Applicative' or
    -- 'Monad' constraint. Be aware that they might be removed in the future
    -- releases.
    , fromMaybeA
    , fromMaybeM
    , fromEitherA
    , fromEitherM

    -- * Reexported
    , module Control.Monad.TaggedException
    )
    where

import Control.Applicative (Applicative(pure))
import Control.Exception (Exception)
import Control.Monad.IO.Class (MonadIO(liftIO))

import Control.Monad.Catch (MonadThrow)

import Control.Monad.TaggedException (Throws, throw)


-- {{{ Maybe ------------------------------------------------------------------

-- | Commonly occurring pattern of lifting @'Maybe' a@ in to monadic context.
-- You can think of it as a 'Applicative' variant of 'fromMaybe' function.
fromMaybeA
    :: (Applicative f)
    => f a
    -> Maybe a
    -> f a
fromMaybeA fa Nothing  = fa
fromMaybeA _  (Just a) = pure a
{-# INLINEABLE fromMaybeA #-}

-- | Commonly occurring pattern of lifting @'Maybe' a@ in to monadic context.
-- You can think of it as a 'Monad' variant of 'fromMaybe' function.
--
-- For /base >= 4.8/ this function is just type restricted variant of
-- 'fromMaybeA'.
fromMaybeM
    :: (Monad m)
    => m a
    -> Maybe a
    -> m a
#if MIN_VERSION_base(4,8,0)
fromMaybeM = fromMaybeA
#else
fromMaybeM ma Nothing  = ma
fromMaybeM _  (Just a) = return a
#endif
{-# INLINEABLE fromMaybeM #-}

-- | Lift 'Maybe' to some 'MonadThrow'. Exception as which 'Nothing' is
-- interpreted is provided as first argument.
--
-- It turned out that common pattern is to use 'Exception's with instances for
-- 'Default' class. In such cases usage of @liftMaybe@ looks like this:
--
-- @
-- liftMaybe 'def'
--     :: ('Default' e, 'Exception' e, 'MonadThrow' m)
--     => 'Maybe' a
--     -> Throws e m a
-- @
liftMaybe
    :: (Exception e, MonadThrow m)
    => e
    -> Maybe a
    -> Throws e m a
liftMaybe = fromMaybeM . throw
{-# INLINEABLE liftMaybe #-}

-- }}} Maybe ------------------------------------------------------------------

-- {{{ Either -----------------------------------------------------------------

-- | Commonly occurring pattern of lifting @'Either' a b@ in to applicative
-- context. You can think of it as a 'Applicative' variant of 'either'
-- function.
fromEitherA
    :: (Applicative f)
    => (a -> f b)
    -> Either a b
    -> f b
fromEitherA f (Left a)  = f a
fromEitherA _ (Right b) = pure b
{-# INLINEABLE fromEitherA #-}

-- | Commonly occurring pattern of lifting @'Either' a b@ in to monadic
-- context. You can think of it as a 'Monad' variant of 'either' function.
--
-- For /base >= 4.8/ this function is just type restricted variant of
-- 'fromEitherA'.
fromEitherM
    :: (Monad m)
    => (a -> m b)
    -> Either a b
    -> m b
#if MIN_VERSION_base(4,8,0)
fromEitherM = fromEitherA
#else
fromEitherM f (Left a)  = f a
fromEitherM _ (Right b) = return b
#endif
{-# INLINEABLE fromEitherM #-}

-- | As 'liftEither', but 'Left' value is mapped to exception using specified
-- function.  In fact 'liftEither' is @liftEitherWith 'id'@.
liftEitherWith
    :: (Exception e, MonadThrow m)
    => (a -> e)
    -> Either a b
    -> Throws e m b
liftEitherWith = fromEitherM . (throw .)
{-# INLINEABLE liftEitherWith #-}

-- | Lift 'Either' to some 'MonadThrow'.
--
-- Lift 'Either' result:
--
-- > (>>= liftEither) . liftT
-- >     :: (Exception e, MonadThrow m)
-- >     => m (Either e b)
-- >     -> Throws e m b
--
-- Lift @ErrorT@:
--
-- > (>>= liftEither) . liftT . runErrorT
-- >     :: (Exception e, MonadThrow m)
-- >     => ErrorT e m a
-- >     -> Throws e m a
liftEither
    :: (Exception e, MonadThrow m)
    => Either e a
    -> Throws e m a
liftEither = liftEitherWith id
{-# INLINEABLE liftEither #-}

-- }}} Either -----------------------------------------------------------------

-- | Type restricted variant of 'liftIO'.
liftIOT :: (Exception e, MonadThrow m, MonadIO m) => IO a -> Throws e m a
liftIOT = liftIO
{-# INLINEABLE liftIOT #-}
