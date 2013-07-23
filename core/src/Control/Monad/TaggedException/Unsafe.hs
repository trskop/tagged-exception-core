-- |
-- Module:       $HEADER$
-- Description:  Unsafe exception tag cobinators and specific lifting
--               functions.
-- Copyright:    (c) 2009 - 2013 Peter Trsko.
-- License:      BSD3
--
-- Stability:    provisional
-- Portability:  non-portable (depends on non-portable module)
--
-- Unsafe exception tag cobinators and specific lifting functions. Import this
-- module if creating new 'MonadException' instance(s) that can not be created
-- otherwise.
--
-- Preferably import as:
--
-- > import qualified Control.Monad.TaggedException.Unsafe as Unsafe
module Control.Monad.TaggedException.Unsafe
    (

    -- * Unsafe exception tag combinators
      throwsOne
    , throwsTwo
    , throwsThree

    , hideOne
    , hideTwo
    , hideThree

    , liftT1
    , liftT2
    , liftT3

    , insideT
    , insideTf
    , insideTf2
    , insideT2
    , insideT3

    , joinT
    , joinT3
    , flipT

    , embedT

    -- * Specific unsafe lift operations
    , liftThrow
    , liftBindLike
    , liftFlipBindLike
    , liftKleisliLike
    , liftMask
    )
    where

import Control.Monad.Trans.Class (MonadTrans(lift))

import Control.Monad.TaggedException.Internal
    ( Throws
    , embedT
    , flipT
    , hideOne
    , hideThree
    , hideTwo
    , insideT
    , insideT2
    , insideT3
    , insideTf
    , insideTf2
    , joinT
    , joinT3
    , liftMask
    , liftT1
    , liftT2
    , liftT3
    , throwsOne
    , throwsThree
    , throwsTwo
    )


-- | Lift tagged @throw@ operation using @'MonadTrans'('lift')@.
--
-- To lift not tagged @throw@ operation just use:
--
-- > liftThrow . (throwsOne .)
-- >     :: (Monad m, MonadTrans t)
-- >     => (e -> m a)
-- >     -> e -> Throws e (t m) a
--
-- To lift function with type @(e -> m a) -> e -> 'Throws' e m a@ just use one
-- of:
--
-- > (liftT .)
-- >     :: (a -> m b) -> a -> Throws e m b
--
-- > ((liftT . lift) .)
-- >     :: (Monad m, MonadTrans t)
-- >     => (a -> m b)
-- >     -> a -> Throws e (t m) b
liftThrow
    :: (Monad m, MonadTrans t)
    => (e -> Throws e m a)
    -> e
    -> Throws e (t m) a
liftThrow = (insideT lift .)
{-# INLINEABLE liftThrow #-}

-- | Lift operations with type similar to monadic bind. In example:
--
-- * @('>>=') :: 'Monad' m => m a -> (a -> m b) -> m b@
--
-- * @Prelude.catch :: IO a -> (IOError -> IO a) -> IO a@.
--
-- * @Control.Exception.catch :: Exception e => IO a -> (e -> IO a) -> IO a@
--
-- Since @1.2.0.0@.
liftBindLike
    :: (m a -> (b -> m c) -> m d)
    -> Throws e m a
    -> (b -> Throws e m c)
    -> Throws e m d
liftBindLike f x g = throwsOne $ f (hideOne x) (hideOne . g)
{-# INLINE liftBindLike #-}

-- | Lift operations with type similar to flipped monadic bind. In example:
--
-- * @('=<<') :: 'Monad' m => (a -> m b) -> m a -> m b@
--
-- * @Control.Exception.handle :: Exception e => (e -> IO a) -> IO a -> IO a@
--
-- Since @1.2.0.0@.
liftFlipBindLike
    :: ((a -> m b) -> m c -> m d)
    -> (a -> Throws e m b) -> Throws e m c -> Throws e m d
liftBindLike f g x = throwsOne $ f (hideOne . g) (hideOne x)
{-# INLINE liftFlipBindLike #-}

-- | Lift klieisli composition like operations. In example:
--
-- * @(Control.Monad.>=>) :: 'Monad' m => (a -> m b) -> (b -> m c) -> a -> m c@
--
-- * @(Control.Monad.<=<) :: 'Monad' m => (b -> m c) -> (a -> m b) -> a -> m c@
--
-- Since @1.2.0.0@.
liftKleisliLike
    :: ((a -> m a') -> (b -> m b') -> c -> m c')
    -> (a -> Throws e m a') -> (b -> Throws e m b') -> c -> Throws e m c'
liftKleisliLike f g h = throwsOne . f (hideOne . g) (hideOne . h)
{-# INLINE liftKleisliLike #-}
