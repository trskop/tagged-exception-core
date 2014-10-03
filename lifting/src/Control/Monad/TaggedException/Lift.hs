-- |
-- Module:       $HEADER$
-- Description:  Commonly used lifting operations mostly from different kinds
--               of error handling.
-- Copyright:    (c) 2011 - 2014 Peter Trško.
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  portable
module Control.Monad.TaggedException.Lift
    (
    -- * Exception lifting
      liftIOT
    , liftMaybe
    , liftEither
    , liftEitherWith

    -- * Monadic lifting
    --
    -- | These functions are more generic and have only 'Monad' constraint. Be
    -- aware that they might be removed in the future releases.
    , liftMaybeM
    , liftEitherM

    -- * Reexported
    , module Control.Monad.TaggedException
    )
    where

import Control.Exception (Exception)
import Control.Monad.IO.Class (MonadIO(liftIO))

import Control.Monad.Catch (MonadThrow)

import Control.Monad.TaggedException (Throws, throw)


-- {{{ Maybe ------------------------------------------------------------------

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
liftMaybe = liftMaybeM . throw
{-# INLINEABLE liftMaybe #-}

-- | Commonly occurring pattern of lifting @'Maybe' a@ in to monadic context.
-- You can think of it as a 'Monad' variant of 'maybe' function.
liftMaybeM
    :: (Monad m)
    => m a
    -> Maybe a
    -> m a
liftMaybeM = (`maybe` return)
{-# INLINEABLE liftMaybeM #-}

-- }}} Maybe ------------------------------------------------------------------

-- {{{ Either -----------------------------------------------------------------

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

-- | As 'liftEither', but 'Left' value is mapped to exception using specified
-- function.  In fact 'liftEither' is @liftEitherWith 'id'@.
liftEitherWith
    :: (Exception e, MonadThrow m)
    => (a -> e)
    -> Either a b
    -> Throws e m b
liftEitherWith = liftEitherM . (throw .)
{-# INLINEABLE liftEitherWith #-}

-- | Commonly occurring pattern of lifting @'Either' a b@ in to monadic
-- context. You can think of it as a 'Monad' variant of 'either' function.
liftEitherM
    :: (Monad m)
    => (a -> m b)
    -> Either a b
    -> m b
liftEitherM = (`either` return)
{-# INLINEABLE liftEitherM #-}

-- }}} Either -----------------------------------------------------------------

-- | Type restricted variant of 'liftIO'.
liftIOT :: (Exception e, MonadThrow m, MonadIO m) => IO a -> Throws e m a
liftIOT = liftIO
{-# INLINEABLE liftIOT #-}
