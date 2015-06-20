{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Core functionality.
-- Copyright:    (c) 2009-2015, Peter Trško
-- License:      BSD3
--
-- Stability:    provisional
-- Portability:  NoImplicitPrelude, depends on non-portable modules
--
-- Core functionality.
module Control.Monad.TaggedException.Core
    (
    -- * Throw, Catch and Map Exceptions
      throw
    , catch
    , catch'
    , handle
    , handle'
    , mapException

    -- ** Specialized Exception Catching
    , try
    , onException
    , onException'

    -- * Exception tag
    , Throws

    -- ** Cobinators
    , liftT
    , lift2T
    , lift3T
    , liftT1
    , liftT2
    , liftT3
    , joinT
    , joinT3
    , flipT
    , insideT
    , insideT2
    , insideT3
    , insideTf
    , insideTf2
    , embedT
    )
    where

import Control.Exception (Exception)
import Data.Either (Either)
import Data.Function ((.), flip)
import Data.Functor (Functor)

import Control.Monad.Catch (MonadCatch, MonadThrow)
import qualified Control.Monad.Catch as Exceptions

import Control.Monad.TaggedException.Internal.Throws (Throws(Throws))
import qualified Control.Monad.TaggedException.Unsafe as Unsafe
    ( embedT
    , flipT
    , insideT
    , insideT2
    , insideT3
    , insideTf
    , insideTf2
    , joinT
    , joinT3
    , liftT1
    , liftT2
    , liftT3
    , throwsOne
    , throwsThree
    , throwsTwo
    )


-- | Throw an exception.  To raise an \"inner\" exception following can be
-- used:
--
-- @
-- 'liftT' . 'throw'
--     :: ('MonadCatch' m, 'Exception' e, 'Exception' e')
--     => e
--     -> 'Throws' e' ('Throws' e m) a
-- @
throw :: (Exception e, MonadThrow m) => e -> Throws e m a
throw = Throws . Exceptions.throwM

-- | Catch exception.
--
-- To catch inner exception following construct can be used:
--
-- @
-- 'catch' . 'flipT'
--     :: ('Exception' e, 'Exception' e', 'MonadCatch' m)
--     => 'Throws' e' ('Throws' e m) a
--     -> (e -> 'Throws' e' m a)
--     -> 'Throws' e' m a
-- @
catch :: (Exception e, MonadCatch m) => Throws e m a -> (e -> m a) -> m a
catch (Throws ma) = Exceptions.catch ma

-- | Catch any exception.
catch' :: (Exception e, MonadCatch m) => m a -> (e -> m a) -> m a
catch' = Exceptions.catch

-- | Flipped version of 'catch'. Usage example:
--
-- @
-- foo = 'handle' exceptionHandler $ do
--     ...
--   where exceptionHandler = ...
-- @
--
-- Handle \"inner\" exception:
--
-- @
-- 'insideT' . 'handle'
--     :: ('MonadCatch' m, 'Exception' e, 'Exception' e')
--     => (e' -> m a)
--     -> 'Throws' e ('Throws' e' m) a
--     -> 'Throws' e m a
-- @
handle
    :: (Exception e, MonadCatch m)
    => (e -> m a)
    -> Throws e m a
    -> m a
handle = flip catch
{-# INLINE handle #-}

-- | Flipped version of 'catch''
handle'
    :: (Exception e, MonadCatch m)
    => (e -> m a)
    -> m a
    -> m a
handle' = flip catch'
{-# INLINE handle' #-}

-- | Similar to 'catch', but returns 'Either' exception or result.
--
-- Inner try:
--
-- @
-- 'try' . 'flipT'
--     :: ('Exception' e, 'Exception' e', MonadCatch m)
--     => 'Throws' e' ('Throws' e m) a
--     -> 'Throws' e' m ('Either' e a)
-- @
try
    :: (Exception e, MonadCatch m)
    => Throws e m a
    -> m (Either e a)
try (Throws ma) = Exceptions.try ma

-- | Map one exception to another.
--
-- Mapping \"inner\" exception has generally two forms:
--
-- 1\. Modifying raised exception, but not changing its type:
--
-- @
-- 'liftT1' . 'mapException'
--     :: ('Exception' e, 'Exception' e', 'MonadCatch' m)
--     => (e -> e)
--     -> 'Throws' e' ('Throws' e m) a
--     -> 'Throws' e' ('Throws' e m) a
-- @
--
-- 2\. Modifying raised exception, including its type:
--
-- @
-- 'insideT' . 'mapException'
--     :: ('Exception' e, 'Exception' e1, 'Exception' e2, 'MonadCatch' m)
--     => (e1 -> e2)
--     -> 'Throws' e ('Throws' e1 m) a
--     -> 'Throws' e ('Throws' e2 m) a
-- @
--
-- Unhiding exception by mapping it in to a different type of exception:
--
-- @
-- \\f -> 'mapException' f . 'liftT'
--     :: ('Exception' e, 'Exception' e', 'MonadCatch' m)
--     => (e -> e')
--     -> m a
--     -> 'Throws' e' m a
-- @
mapException
    :: (Exception e, Exception e', MonadCatch m)
    => (e -> e')
    -> Throws e m a
    -> Throws e' m a
mapException = flip (catch . flipT . liftT) . (throw .)

-- | Run computation if exception was raised. Basically:
--
-- @
-- m `onException` n = 'liftT' m `catch` \\e -> 'liftT' n >> 'throw' e
-- @
onException
    :: (Exception e, MonadCatch m)
    => Throws e m a
    -- ^ Computation that may raise exception
    -> m b
    -- ^ The computation to run if an exception @e@ is raised
    -> Throws e m a
onException (Throws ma) = Throws . Exceptions.onException ma

-- | Same as 'onException', but uses 'catch'' and so second computation is
-- executed if any exception is raised.
onException'
    :: (MonadCatch m)
    => m a
    -- ^ Computation that may raise exception
    -> m b
    -- ^ The computation to run if an exception is raised
    -> m a
onException' = Exceptions.onException

-- {{{ Exception tag -- Combinators -------------------------------------------

-- | Construct exception tag, with type restrictions.
--
-- Reflect raised exception in function's type:
--
-- @
-- import Control.Monad.TaggedException ('Throws', 'liftT')
-- import System.IO (Handle, IOMode)
-- import qualified System.IO as IO (openFile)
--
--
-- openFile :: FilePath -> IOMode -> 'Throws' IOError IO Handle
-- openFile = ('liftT' .) . IO.openFile
-- @
--
-- Lifting @m@ to @'Throws' e m@:
--
-- @
-- import "Control.Exception" ('Exception')
--
-- import Control.Monad.TaggedException ('Throws', 'liftT', 'throw')
-- import "Data.Typeable" ('Typeable')
--
--
-- data EmptyString = EmptyString
--     deriving (Show, 'Typeable')
--
-- instance 'Exception' EmptyString
--
-- writeIfNotEmpty
--     :: FilePath
--     -> String
--     -> 'Throws' EmptyString IO ()
-- writeIfNotEmpty filename str = do
--     when (null str) $ 'throw' EmptyString
--     'liftT' $ writeFile filename str
-- @
--
-- We have a some commonly used patterns:
--
-- @
-- ('liftT' .)
--     :: ('Exception' e, 'MonadThrow' m)
--     => (a -> m b)
--     -> a -> 'Throws' e m b
-- @
--
-- Above is also usable for lifting throw-like functions:
--
-- @
-- import "Control.Monad.Trans.Class" ('Control.Monad.Trans.Class.MonadTrans'('Control.Monad.Trans.Class.lift'))
--
-- (('liftT' . 'Control.Monad.Trans.Class.lift') .)
--     ::  ( 'Exception' e
--         , 'MonadThrow' m
--         , 'MonadThrow' (t m)
--         , 'Control.Monad.Trans.Class.MonadTrans' t)
--     => (a -> m b)
--     -> a -> 'Throws' e (t m) b
-- @
liftT :: (Exception e, MonadThrow m) => m a -> Throws e m a
liftT = Unsafe.throwsOne
{-# INLINE liftT #-}

-- | Shorthand for @'liftT' . 'liftT'@.
lift2T
    :: (Exception e, Exception e', MonadThrow m)
    => m a
    -> Throws e' (Throws e m) a
lift2T = Unsafe.throwsTwo
{-# INLINE lift2T #-}

-- | Shorthand for @'liftT' . 'liftT' . 'liftT'@.
lift3T
    :: (Exception e, Exception e', Exception e'', MonadThrow m)
    => m a
    -> Throws e'' (Throws e' (Throws e m)) a
lift3T = Unsafe.throwsThree
{-# INLINE lift3T #-}

-- | 'liftT' for functions with arity one.
liftT1
    :: (Exception e, MonadThrow m)
    => (m a -> m b)
    -> Throws e m a -> Throws e m b
liftT1 = Unsafe.liftT1
{-# INLINE liftT1 #-}

-- | 'liftT' for functions with arity two.
liftT2
    :: (Exception e, MonadThrow m)
    => (m a -> m b -> m c)
    -> Throws e m a -> Throws e m b -> Throws e m c
liftT2 = Unsafe.liftT2
{-# INLINE liftT2 #-}

-- | 'liftT' for functions with arity three.
liftT3
    :: (Exception e, MonadThrow m)
    => (m a -> m b -> m c -> m d)
    -> Throws e m a -> Throws e m b -> Throws e m c -> Throws e m d
liftT3 = Unsafe.liftT3
{-# INLINE liftT3 #-}

-- | Join two outermost exception tags.
joinT
    :: (Exception e, MonadThrow m)
    => Throws e (Throws e m) a
    -> Throws e m a
joinT = Unsafe.joinT
{-# INLINE joinT #-}

-- | Join three outermost exception tags.
joinT3
    :: (Exception e, MonadThrow m)
    => Throws e (Throws e (Throws e m)) a
    -> Throws e m a
joinT3 = Unsafe.joinT3
{-# INLINE joinT3 #-}

-- | Flip two outermost exception tags.
flipT
    :: (Exception e, Exception e', MonadThrow m)
    => Throws e' (Throws e m) a
    -> Throws e (Throws e' m) a
flipT = Unsafe.flipT
{-# INLINE flipT #-}

-- | Generalized 'liftT'. Usage examples:
--
-- @
-- 'insideT' lift
--    :: ('MonadThrow' (t m), 'MonadThrow' m, 'Exception' e, 'Control.Monad.Trans.Class.MonadTrans' t)
--    => 'Throws' e m b
--    -> 'Throws' e (t m) b
-- @
--
-- This is variation on the first example that explicitly lifts monad:
--
-- 'insideT' 'Control.Monad.Trans.Writer.Lazy.WriterT'
--     :: ('Exception' e, 'MonadThrow' m, 'Data.Monoid.Monoid' w)
--     => 'Throws' e m (b, w)
--     -> 'Throws' e ('Control.Monad.Trans.Writer.Lazy.WriterT' w m) b
--
-- Some useful compositions of exception tag combinators:
--
-- @
-- 'insideT' 'flipT'
--     :: ('Exception' e0, 'Exception' e1, 'Exception' e2, 'MonadThrow' m)
--     => 'Throws' e0 ('Throws' e1 ('Throws' e2 m)) a
--     -> 'Throws' e0 ('Throws' e2 ('Throws' e1 m)) a
-- @
--
-- @
-- 'flipT' . 'insideT' 'flipT'
--     :: ('Exception' e0, 'Exception' e1, 'Exception' e2, 'MonadThrow' m)
--     => 'Throws' e0 ('Throws' e1 ('Throws' e2 m)) a
--     -> 'Throws' e2 ('Throws' e0 ('Throws' e1 m)) a
-- @
insideT
    :: (Exception e, MonadThrow m, MonadThrow m')
    => (m a -> m' b)
    -> Throws e m a -> Throws e m' b
insideT = Unsafe.insideT
{-# INLINE insideT #-}

-- | Variant of 'insideT'.
--
-- Usage example:
--
-- @
-- 'insideTf' 'Control.Monad.Trans.State.Lazy.StateT'
--     :: ('Exception' e, 'MonadThrow' m)
--     => (s -> 'Throws' e m (a, s))
--     -> 'Throws' e ('Control.Monad.Trans.State.Lazy.StateT' s m) a
-- @
insideTf
    :: (Exception e, Functor f, MonadThrow m, MonadThrow m')
    => (f (m a) -> m' b)
    -> f (Throws e m a)
    -> Throws e m' b
insideTf = Unsafe.insideTf
{-# INLINE insideTf #-}

-- | Variant of 'insideT'.
--
-- Usage example:
--
-- @
-- 'insideTf2' 'Control.Monad.Trans.RWS.Lazy.RWST'
--     :: ('Exception' e, 'MonadThrow' m)
--     => (r -> s -> 'Throws' e m (a, s, w))
--     -> 'Throws' e ('Control.Monad.Trans.RWS.Lazy.RWST' r w s m) a
-- @
insideTf2
    :: (Exception e, Functor f, Functor f', MonadThrow m, MonadThrow m')
    => (f (f' (m a)) -> m' b)
    -> f (f' (Throws e m a))
    -> Throws e m' b
insideTf2 = Unsafe.insideTf2
{-# INLINE insideTf2 #-}

-- | Generalized 'liftT2'.
insideT2
    :: (Exception e, MonadThrow m1, MonadThrow m2, MonadThrow m3)
    => (m1 a -> m2 b -> m3 c)
    -> Throws e m1 a -> Throws e m2 b -> Throws e m3 c
insideT2 = Unsafe.insideT2
{-# INLINE insideT2 #-}

-- | Generalized 'liftT3'.
insideT3
    :: (Exception e, MonadThrow m1, MonadThrow m2, MonadThrow m3,
        MonadThrow m4)
    => (m1 a -> m2 b -> m3 c -> m4 d)
    -> Throws e m1 a -> Throws e m2 b -> Throws e m3 c -> Throws e m4 d
insideT3 = Unsafe.insideT3
{-# INLINE insideT3 #-}

-- |
--
-- Since @1.2.0.0@
embedT :: (Exception e, MonadThrow m, MonadThrow m')
    => (m a -> Throws e m' b)
    -> Throws e m a -> Throws e m' b
embedT = Unsafe.embedT
{-# INLINE embedT #-}

-- }}} Exception tag -- Combinators -------------------------------------------
