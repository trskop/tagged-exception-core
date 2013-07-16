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

    -- * Lift operations
    , liftMask
    )
    where

import Control.Applicative (Alternative(..), Applicative(..))
import Control.Monad (MonadPlus(..))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (MonadTrans(lift))


-- | Exception tag.
newtype Throws e m a = Throws
    { hideOne :: m a
    -- ^ Hide one exception.
    }

-- {{{ Instances --------------------------------------------------------------

instance (Functor f) => Functor (Throws e f) where
    fmap f = Throws . fmap f . hideOne

instance (Applicative f) => Applicative (Throws e f) where
    pure = Throws . pure
    Throws x <*> Throws y = Throws $ x <*> y
    Throws x *> Throws y = Throws $ x *> y
    Throws x <* Throws y = Throws $ x <* y

instance (Alternative f) => Alternative (Throws e f) where
    empty = Throws empty
    Throws x <|> Throws y = Throws $ x <|> y
    some = Throws . some . hideOne
    many = Throws . many . hideOne

instance (Monad m) => Monad (Throws e m) where
    return = Throws . return
    m >>= f = Throws $ hideOne m >>= hideOne . f
    fail = Throws . fail

instance (MonadPlus m) => MonadPlus (Throws e m) where
    mzero = Throws mzero
    Throws m `mplus` Throws n = Throws $ m `mplus` n

instance (MonadIO m) => MonadIO (Throws e m) where
    liftIO = Throws . liftIO

instance MonadTrans (Throws e) where
    lift = Throws

-- }}} Instances --------------------------------------------------------------

throwsOne :: m a -> Throws e m a
throwsOne = Throws

throwsTwo :: m a -> Throws e' (Throws e m) a
throwsTwo = Throws . Throws

throwsThree :: m a -> Throws e'' (Throws e' (Throws e m)) a
throwsThree = Throws . Throws . Throws

-- | Hide two exceptions, but without 'MonadException' restriction.
hideTwo :: Throws e (Throws e' m) a -> m a
hideTwo = hideOne . hideOne

-- | Hide three exceptions, but without 'MonadException' restriction.
hideThree :: Throws e (Throws e' (Throws e'' m)) a -> m a
hideThree = hideOne . hideOne . hideOne

liftT1
    :: (m a -> m b)
    -> Throws e m a -> Throws e m b
liftT1 = (throwsOne .) . (. hideOne)

liftT2
    :: (m a -> m b -> m c)
    -> Throws e m a -> Throws e m b -> Throws e m c
liftT2 f m n = throwsOne $ f (hideOne m) (hideOne n)

liftT3
    :: (m a -> m b -> m c -> m d)
    -> Throws e m a -> Throws e m b -> Throws e m c -> Throws e m d
liftT3 f m n o = throwsOne $ f (hideOne m) (hideOne n) (hideOne o)

-- | Generalized 'liftT'.
insideT
    :: (m a -> m' b)
    -> Throws e m a -> Throws e m' b
insideT = (throwsOne .) . (. hideOne)

-- | Variant 'insideT'.
insideTf
    :: (Functor f)
    => (f (m a) -> m' b)
    -> f (Throws e m a)
    -> Throws e m' b
insideTf = (throwsOne .) . (. fmap hideOne)

-- | Variant 'insideT'.
insideTf2
    :: (Functor f, Functor f')
    => (f (f' (m a)) -> m' b)
    -> f (f' (Throws e m a))
    -> Throws e m' b
insideTf2 = (throwsOne .) . (. fmap  (fmap hideOne))

-- | Generalized 'liftT2'.
insideT2
    :: (m1 a -> m2 b -> m3 c)
    -> Throws e m1 a -> Throws e m2 b -> Throws e m3 c
insideT2 f m n = throwsOne $ f (hideOne m) (hideOne n)

-- | Generalized 'liftT3'.
insideT3
    :: (m1 a -> m2 b -> m3 c -> m4 d)
    -> Throws e m1 a -> Throws e m2 b -> Throws e m3 c -> Throws e m4 d
insideT3 f m n o = throwsOne $ f (hideOne m) (hideOne n) (hideOne o)

-- | Join two exception tags in to one. Isn't restricted just to
-- 'MonadException' instances.
joinT
    :: Throws e (Throws e m) a
    -> Throws e m a
joinT = hideOne

-- | Join three exception tags in to one. Isn't restricted just to
-- 'MonadException' instances.
joinT3
    :: Throws e (Throws e (Throws e m)) a
    -> Throws e m a
joinT3 = hideTwo

flipT
    :: Throws e' (Throws e m) a
    -> Throws e (Throws e' m) a
flipT = throwsTwo . hideTwo

-- | Lift @mask@ operation.
liftMask
    :: (((forall a. m a -> m a) -> m b) -> m b)
    -> ((forall a. Throws e m a -> Throws e m a) -> Throws e m b)
    -> Throws e m b
liftMask msk f = throwsOne . msk $ \ restore ->
    (hideOne . f) (throwsOne . restore . hideOne)
