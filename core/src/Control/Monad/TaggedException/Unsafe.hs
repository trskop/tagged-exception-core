{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Unsafe exception tag cobinators and specific lifting
--               functions.
-- Copyright:    (c) 2009 - 2014 Peter Trsko
-- License:      BSD3
--
-- Stability:    provisional
-- Portability:  non-portable (NoImplicitPrelude)
--
-- Unsafe exception tag cobinators and specific lifting functions. Import this
-- module if creating new 'MonadException' instance(s) that can not be created
-- otherwise.
--
-- Preferably import as:
--
-- > import qualified Control.Monad.TaggedException.Unsafe as Unsafe
module Control.Monad.TaggedException.Unsafe
    ( Throws
    , throwsOne
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

    , liftMask
    , liftBindLike
    , liftFlipBindLike
    , liftKleisliLike
    )
    where

import Data.Function ((.), ($))
import Data.Functor (Functor(fmap))

import Control.Monad.TaggedException.Internal.Throws (Throws(..), liftMask)


-- | Construct exception tag, but without 'Control.Monad.Catch.MonadThrow'
-- restriction.
throwsOne :: m a -> Throws e m a
throwsOne = Throws

-- | Shorthand for @'throwsOne' . 'throwsOne'@.
throwsTwo :: m a -> Throws e' (Throws e m) a
throwsTwo = Throws . Throws

-- | Shorthand for @'throwsOne' . 'throwsOne' . 'throwsOne'@.
throwsThree :: m a -> Throws e'' (Throws e' (Throws e m)) a
throwsThree = Throws . Throws . Throws

-- | Hide one exceptions, but without 'Control.Monad.Catch.MonadThrow'
-- restriction.
hideOne :: Throws e m a -> m a
hideOne = hideException

-- | Hide two exceptions, but without 'Control.Monad.Catch.MonadThrow'
-- restriction.
hideTwo :: Throws e (Throws e' m) a -> m a
hideTwo = hideException . hideException

-- | Hide three exceptions, but without 'Control.Monad.Catch.MonadThrow'
-- restriction.
hideThree :: Throws e (Throws e' (Throws e'' m)) a -> m a
hideThree = hideException . hideException . hideException

-- | 'liftT' for functions with arity one. Isn't restricted just to
-- 'Control.Monad.Catch.MonadThrow' instances.
liftT1
    :: (m a -> m b)
    -> Throws e m a -> Throws e m b
liftT1 = (Throws .) . (. hideException)

-- | 'liftT' for functions with arity two. Isn't restricted just to
-- 'Control.Monad.Catch.MonadThrow' instances.
liftT2
    :: (m a -> m b -> m c)
    -> Throws e m a -> Throws e m b -> Throws e m c
liftT2 f m n = Throws $ f (hideException m) (hideException n)

-- | 'liftT' for functions with arity three. Isn't restricted just to
-- 'Control.Monad.Catch.MonadThrow' instances.
liftT3
    :: (m a -> m b -> m c -> m d)
    -> Throws e m a -> Throws e m b -> Throws e m c -> Throws e m d
liftT3 f m n o =
    Throws $ f (hideException m) (hideException n) (hideException o)

-- | Generalized 'liftT'.
insideT
    :: (m a -> m' b)
    -> Throws e m a -> Throws e m' b
insideT = (Throws .) . (. hideException)

-- | Variant 'insideT'.
insideTf
    :: (Functor f)
    => (f (m a) -> m' b)
    -> f (Throws e m a)
    -> Throws e m' b
insideTf = (Throws .) . (. fmap hideException)

-- | Variant 'insideT'.
insideTf2
    :: (Functor f, Functor f')
    => (f (f' (m a)) -> m' b)
    -> f (f' (Throws e m a))
    -> Throws e m' b
insideTf2 = (Throws .) . (. fmap  (fmap hideException))

-- | Generalized 'liftT2'.
insideT2
    :: (m1 a -> m2 b -> m3 c)
    -> Throws e m1 a -> Throws e m2 b -> Throws e m3 c
insideT2 f m n = Throws $ f (hideException m) (hideException n)

-- | Generalized 'liftT3'.
insideT3
    :: (m1 a -> m2 b -> m3 c -> m4 d)
    -> Throws e m1 a -> Throws e m2 b -> Throws e m3 c -> Throws e m4 d
insideT3 f m n o =
    Throws $ f (hideException m) (hideException n) (hideException o)

-- | Join two exception tags in to one. Isn't restricted just to
-- 'Control.Monad.Catch.MonadThrow' instances.
joinT
    :: Throws e (Throws e m) a
    -> Throws e m a
joinT = hideException

-- | Join three exception tags in to one. Isn't restricted just to
-- 'Control.Monad.Catch.MonadThrow' instances.
joinT3
    :: Throws e (Throws e (Throws e m)) a
    -> Throws e m a
joinT3 = hideTwo

-- | Flip two outermost exception tags. Isn't restricted just to
-- 'Control.Monad.Catch.MonadThrow' instances.
flipT
    :: Throws e' (Throws e m) a
    -> Throws e (Throws e' m) a
flipT = throwsTwo . hideTwo

-- | Since @1.2.0.0@.
embedT :: (m a -> Throws e n b) -> Throws e m a -> Throws e n b
embedT = (. hideException)

-- | Lift operations with type similar to monadic bind. In example:
--
-- @
-- ('Control.Monad.>>=') :: 'Control.Monad.Monad' m => m a -> (a -> m b) -> m b
-- @
--
-- @
-- 'Prelude.catch'
--     :: 'System.IO.IO' a
--     -> ('Control.Exception.IOError' -> 'System.IO.IO' a)
--     -> 'System.IO.IO' a
-- @
--
-- @
-- 'Control.Exception.catch'
--     :: 'Control.Exception.Exception' e
--     => 'System.IO.IO' a -> (e -> 'System.IO.IO' a) -> 'System.IO.IO' a
-- @
--
-- Since @1.2.0.0@.
liftBindLike
    :: (m a -> (b -> m c) -> m d)
    -> Throws e m a
    -> (b -> Throws e m c)
    -> Throws e m d
liftBindLike f x g = throwsOne $ f (hideException x) (hideException . g)

-- | Lift operations with type similar to flipped monadic bind. In example:
--
-- @
-- ('Control.Monad.=<<') :: 'Control.Monad.Monad' m => (a -> m b) -> m a -> m b
-- @
--
-- @
-- 'Control.Exception.handle'
--     :: 'Control.Exception.Exception' e
--     => (e -> 'System.IO.IO' a) -> 'System.IO.IO' a -> 'System.IO.IO' a
-- @
--
-- Since @1.2.0.0@.
liftFlipBindLike
    :: ((a -> m b) -> m c -> m d)
    -> (a -> Throws e m b) -> Throws e m c -> Throws e m d
liftFlipBindLike f g x = Throws $ f (hideException . g) (hideException x)

-- | Lift klieisli composition like operations. In example:
--
-- @
-- ('Control.Monad.>=>')
--     :: 'Control.Monad.Monad' m => (a -> m b) -> (b -> m c) -> a -> m c
-- @
--
-- @
-- ('Control.Monad.<=<')
--     :: 'Control.Monad.Monad' m => (b -> m c) -> (a -> m b) -> a -> m c
-- @
--
-- Since @1.2.0.0@.
liftKleisliLike
    :: ((a -> m a') -> (b -> m b') -> c -> m c')
    -> (a -> Throws e m a') -> (b -> Throws e m b') -> c -> Throws e m c'
liftKleisliLike f g h = Throws . f (hideException . g) (hideException . h)
