{-# LANGUAGE RankNTypes #-}
-- |
-- Module      :  $HEADER$
-- Description :  Internal definitions.
-- Copyright   :  (c) 2009 - 2013 Peter Trsko
-- License     :  BSD3
--
-- Stability   :  provisional
-- Portability :  non-portable (RankNTypes)
--
-- This module is not exposed, exported definitions are available in
-- "Control.Monad.TaggedException.Core" or
-- "Control.Monad.TaggedException.Unsafe".
module Control.Monad.TaggedException.Internal
    ( Throws(hideOne)
    , throwsOne
    , throwsTwo
    , throwsThree
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

    -- * Lift operations
    , liftMask
    )
    where

import Control.Applicative (Alternative(..), Applicative(..))
import Control.Monad (MonadPlus(..))

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Morph (MFunctor(hoist), MMonad(embed))
import Control.Monad.Trans.Class (MonadTrans(lift))


-- | Exception tag.
newtype Throws e m a = Throws
    { hideOne :: m a
    -- ^ Hide one exception.
    }

-- {{{ Instances --------------------------------------------------------------

instance (Functor f) => Functor (Throws e f) where
    fmap f = Throws . fmap f . hideOne
    {-# INLINEABLE fmap #-}

instance (Applicative f) => Applicative (Throws e f) where
    pure = Throws . pure
    {-# INLINEABLE pure #-}
    Throws x <*> Throws y = Throws $ x <*> y
    {-# INLINEABLE (<*>) #-}
    Throws x *> Throws y = Throws $ x *> y
    {-# INLINEABLE (*>) #-}
    Throws x <* Throws y = Throws $ x <* y
    {-# INLINEABLE (<*) #-}

instance (Alternative f) => Alternative (Throws e f) where
    empty = Throws empty
    {-# INLINEABLE empty #-}
    Throws x <|> Throws y = Throws $ x <|> y
    {-# INLINEABLE (<|>) #-}
    some = Throws . some . hideOne
    {-# INLINEABLE some #-}
    many = Throws . many . hideOne
    {-# INLINEABLE many #-}

instance (Monad m) => Monad (Throws e m) where
    return = Throws . return
    {-# INLINEABLE return #-}
    m >>= f = Throws $ hideOne m >>= hideOne . f
    {-# INLINEABLE (>>=) #-}
    fail = Throws . fail
    {-# INLINEABLE fail #-}

instance (MonadPlus m) => MonadPlus (Throws e m) where
    mzero = Throws mzero
    {-# INLINEABLE mzero #-}
    Throws m `mplus` Throws n = Throws $ m `mplus` n
    {-# INLINEABLE mplus #-}

instance (MonadIO m) => MonadIO (Throws e m) where
    liftIO = Throws . liftIO
    {-# INLINEABLE liftIO #-}

instance MonadTrans (Throws e) where
    lift = Throws
    {-# INLINE lift #-}

-- | Since @1.2.0.0@.
instance MFunctor (Throws e) where
    -- Monad m => (forall a. m a -> n a) -> Throws e m b -> Throws e n b
    hoist f x =  Throws (f (hideOne x))
    {-# INLINEABLE hoist #-}

-- | Since @1.2.0.0@.
instance MMonad (Throws e) where
    -- Monad n
    -- => (forall a. m a -> Throws e n a) -> Throws e m b -> Throws e n b
    embed f x = f (hideOne x)
    {-# INLINEABLE embed #-}

-- }}} Instances --------------------------------------------------------------

-- | Construct exception tag, but without 'MonadException' restriction.
throwsOne :: m a -> Throws e m a
throwsOne = Throws
{-# INLINE throwsOne #-}

-- | Shorthand for @throwsOne . throwsOne@.
throwsTwo :: m a -> Throws e' (Throws e m) a
throwsTwo = Throws . Throws
{-# INLINE throwsTwo #-}

-- | Shorthand for @throwsOne . throwsOne . throwsOne@.
throwsThree :: m a -> Throws e'' (Throws e' (Throws e m)) a
throwsThree = Throws . Throws . Throws
{-# INLINE throwsThree #-}

-- | Hide two exceptions, but without 'MonadException' restriction.
hideTwo :: Throws e (Throws e' m) a -> m a
hideTwo = hideOne . hideOne
{-# INLINE hideTwo #-}

-- | Hide three exceptions, but without 'MonadException' restriction.
hideThree :: Throws e (Throws e' (Throws e'' m)) a -> m a
hideThree = hideOne . hideOne . hideOne
{-# INLINE hideThree #-}

-- | @liftT@ for functions with arity one. Isn't restricted just to
-- 'MonadException' instances.
liftT1
    :: (m a -> m b)
    -> Throws e m a -> Throws e m b
liftT1 = (throwsOne .) . (. hideOne)
{-# INLINE liftT1 #-}

-- | @liftT@ for functions with arity two. Isn't restricted just to
-- 'MonadException' instances.
liftT2
    :: (m a -> m b -> m c)
    -> Throws e m a -> Throws e m b -> Throws e m c
liftT2 f m n = throwsOne $ f (hideOne m) (hideOne n)
{-# INLINE liftT2 #-}

-- | @liftT@ for functions with arity three. Isn't restricted just to
-- 'MonadException' instances.
liftT3
    :: (m a -> m b -> m c -> m d)
    -> Throws e m a -> Throws e m b -> Throws e m c -> Throws e m d
liftT3 f m n o = throwsOne $ f (hideOne m) (hideOne n) (hideOne o)
{-# INLINE liftT3 #-}

-- | Generalized 'liftT'.
insideT
    :: (m a -> m' b)
    -> Throws e m a -> Throws e m' b
insideT = (throwsOne .) . (. hideOne)
{-# INLINE insideT #-}

-- | Variant 'insideT'.
insideTf
    :: (Functor f)
    => (f (m a) -> m' b)
    -> f (Throws e m a)
    -> Throws e m' b
insideTf = (throwsOne .) . (. fmap hideOne)
{-# INLINEABLE insideTf #-}

-- | Variant 'insideT'.
insideTf2
    :: (Functor f, Functor f')
    => (f (f' (m a)) -> m' b)
    -> f (f' (Throws e m a))
    -> Throws e m' b
insideTf2 = (throwsOne .) . (. fmap  (fmap hideOne))
{-# INLINEABLE insideTf2 #-}

-- | Generalized 'liftT2'.
insideT2
    :: (m1 a -> m2 b -> m3 c)
    -> Throws e m1 a -> Throws e m2 b -> Throws e m3 c
insideT2 f m n = throwsOne $ f (hideOne m) (hideOne n)
{-# INLINE insideT2 #-}

-- | Generalized 'liftT3'.
insideT3
    :: (m1 a -> m2 b -> m3 c -> m4 d)
    -> Throws e m1 a -> Throws e m2 b -> Throws e m3 c -> Throws e m4 d
insideT3 f m n o = throwsOne $ f (hideOne m) (hideOne n) (hideOne o)
{-# INLINE insideT3 #-}

-- | Join two exception tags in to one. Isn't restricted just to
-- 'MonadException' instances.
joinT
    :: Throws e (Throws e m) a
    -> Throws e m a
joinT = hideOne
{-# INLINE joinT #-}

-- | Join three exception tags in to one. Isn't restricted just to
-- 'MonadException' instances.
joinT3
    :: Throws e (Throws e (Throws e m)) a
    -> Throws e m a
joinT3 = hideTwo
{-# INLINE joinT3 #-}

-- | Flip two outermost exception tags. Isn't restricted just to
-- 'MonadException' instances.
flipT
    :: Throws e' (Throws e m) a
    -> Throws e (Throws e' m) a
flipT = throwsTwo . hideTwo
{-# INLINE flipT #-}

-- | Since @1.2.0.0@.
embedT :: (m a -> Throws e n b) -> Throws e m a -> Throws e n b
embedT = (. hideOne)
{-# INLINE embedT #-}

-- | Lift @mask@ operation.
liftMask
    :: (((forall a. m a -> m a) -> m b) -> m b)
    -> ((forall a. Throws e m a -> Throws e m a) -> Throws e m b)
    -> Throws e m b
liftMask msk f = throwsOne . msk $ \ restore ->
    (hideOne . f) (throwsOne . restore . hideOne)
{-# INLINEABLE liftMask #-}
