-- |
-- Module:       Control.Monad.ExceptionT
-- Description:  Monad transformer with MonadException instance
-- Copyright:    (c) 2009 - 2011, 2013 Peter Trsko
-- License:      BSD3
--
-- Stability:    provisional
-- Portability:  portable
--
-- Monad transformer that adds the ability to throw exceptions to a monad.  The
-- 'ExceptionT' monad transformer can be used instead of @ErrorT@.
module Control.Monad.TaggedException.ExceptionT
    (
    -- * The ExceptionT monad transformer
      ExceptionT(..)
    , evalExceptionT
    , mapExceptionT
    , mapExceptionT2

    -- * Exception operations
    , throwException
    , catchException

    -- * Lifting
    , liftMonad

    -- ** Lift other operations
    , liftCallCC
    , liftCatch
    , liftListen
    , liftPass

    -- * Reexports
    , module Control.Monad.TaggedException
    , module Control.Monad.TaggedException.UserException
    )
    where

import Prelude hiding (catch)

import Control.Applicative (Alternative(..), Applicative(..))
import Control.Arrow (first)
import Control.Monad (MonadPlus(..), ap, liftM)

import Control.Monad.TaggedException
import Control.Monad.Trans.Class (MonadTrans(lift))

import Control.Monad.TaggedException.UserException


-- | The exception monad transformer.  It can be used to add exception
-- handling to other monads.
--
-- Computations are actions that may produce a value or exception.  The
-- 'return' function yields a successful computation, while '>>=' sequences two
-- subcomputations, failing on the first exception.  The 'fail' and 'mplus'
-- functions throws
-- "Control.Monad.ExceptionWitness.UserException"@.@'UserException', function
-- 'mplus' behaves as 'onException''.
newtype ExceptionT m a
    = ExceptionT { runExceptionT :: m (Either SomeException a) }

-- | Evaluate 'ExceptionT' in to underlying Monad.
--
-- > evalExceptionT m f = runExceptionT m >>= either f return
evalExceptionT :: (Monad m) => ExceptionT m a -> (SomeException -> m a) -> m a
evalExceptionT m f = runExceptionT m >>= either f return

-- | Transform the inner computation of 'ExceptionT'.
--
-- It holds: @'runExceptionT' (mapExceptionT f m) = f ('runExceptionT' m)@
mapExceptionT
    :: (m (Either SomeException a) -> n (Either SomeException b))
    -> ExceptionT m a -> ExceptionT n b
mapExceptionT = (ExceptionT .) . (. runExceptionT)
{-# INLINE mapExceptionT #-}

-- | Generalization of 'mapExceptionT2' to functions of arity two.
mapExceptionT2
    :: (m (Either SomeException a)
        -> n (Either SomeException b)
        -> o (Either SomeException c))
    -> ExceptionT m a -> ExceptionT n b -> ExceptionT o c
mapExceptionT2 f x y = ExceptionT $ f (runExceptionT x) (runExceptionT y)
{-# INLINE mapExceptionT2 #-}

-- | Lift monad to 'ExceptionT' monad transformer, it's also used as
-- implementation of @'MonadTrans'('lift')@.
liftMonad :: (Monad m) => m a -> ExceptionT m a
liftMonad = ExceptionT . liftM Right

-- | Raise an exception value @e@. This function is used to implement
-- @'MonadException'('throw')@ as @'liftT' . 'throwException'@.
throwException :: (Exception e, Monad m) => e -> ExceptionT m a
throwException = ExceptionT . return . Left . toException

-- | Catch an exception. This function is used as implementation of
-- @'MonadException'('catch'')@.
catchException
    :: (Exception e, Monad m)
    => ExceptionT m a
    -- ^ Computation that may raise exception
    -> (e -> ExceptionT m a)
    -- ^ Handler for exceptions in the computation
    -> ExceptionT m a
m `catchException` h = ExceptionT $ runExceptionT m >>= either
    (\ e -> maybe (return (Left e)) (runExceptionT . h) $ fromException e)
    (return . Right)

-- {{{ ExceptionT -- Instances ------------------------------------------------

instance (Functor m) => Functor (ExceptionT m) where
    -- fmap :: (a -> b) -> ExceptionT f a -> ExceptionT f b
    fmap f = mapExceptionT $ fmap (either Left (Right . f))

instance (Functor m, Monad m) => Applicative (ExceptionT m) where
    pure = return
    (<*>) = ap

instance (Functor m, Monad m) => Alternative (ExceptionT m) where
    empty = mzero
    (<|>) = mplus

instance (Monad m) => Monad (ExceptionT m) where
    return = ExceptionT . return . Right
    m >>= f = ExceptionT
        $ runExceptionT m >>= either (return . Left) (runExceptionT . f)
    fail = throwException . UserException

instance (Monad m) => MonadException (ExceptionT m) where
    throw = liftT . throwException
    catch' = catchException

instance (Monad m) => MonadExceptionUtilities (ExceptionT m)
    -- Default implementations for methods are used.

instance (Monad m) => MonadPlus (ExceptionT m) where
    mzero = throwException emptyUserException
    m `mplus` n = ExceptionT
        $ runExceptionT m >>= either (const $ runExceptionT n) (return . Right)

instance MonadTrans ExceptionT where
    lift = liftMonad

-- }}} ExceptionT -- Instances ------------------------------------------------

-- {{{ Lift other operations --------------------------------------------------

-- | Lift a @callCC@ operation.
liftCallCC
    :: (((Either SomeException a -> m (Either SomeException b))
        -> m (Either SomeException a)) -> m (Either SomeException a))
    -> ((a -> ExceptionT m b) -> ExceptionT m a) -> ExceptionT m a
liftCallCC callCC f = ExceptionT . callCC $ \ g ->
    runExceptionT . f $ ExceptionT . g . Right
{-# INLINE liftCallCC #-}

-- | Lift a @catch@ operation.
liftCatch
    :: (Monad m)
    => (m (Either SomeException a) -> (e -> m (Either SomeException a))
        -> m (Either SomeException a))
    -> ExceptionT m a -> (e -> ExceptionT m a) -> ExceptionT m a
liftCatch catchError m h =
    ExceptionT $ runExceptionT m `catchError` (runExceptionT . h)

-- | Lift a @listen@ operation.
liftListen
    :: (Monad m)
    => (m (Either SomeException a) -> m (Either SomeException a, w))
    -> ExceptionT m a -> ExceptionT m (a, w)
liftListen = mapExceptionT . (liftM (\ (x, w) -> fmap (\ y -> (y, w)) x) .)
    -- Conversion:
    --   (Either SomeException a, w) -> Either SomeException (a, w)

-- | Lift a @pass@ operation.
liftPass
    :: (Monad m)
    => (m (Either SomeException a, w -> w) -> m (Either SomeException a))
    -> ExceptionT m (a, w -> w) -> ExceptionT m a
liftPass = mapExceptionT
    -- . (. liftM (either (\ x -> (Left x, id)) (\ (x, f) -> (Right x, f))))
    . (. liftM (either (\ x -> (Left x, id)) (first Right)))
    -- Conversion:
    --   Either SomeException (a, w -> w) -> (Either SomeException a, w -> w)

-- }}} Lift other operations --------------------------------------------------
