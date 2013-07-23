-- |
-- Module:       $HEADER$
-- Description:  Core functionality.
-- Copyright:    (c) 2009 - 2013 Peter Trsko.
-- License:      BSD3
--
-- Stability:    provisional
-- Portability:  non-portable (depends on non-portable modules)
--
-- Core functionality.
module Control.Monad.TaggedException.Core
    (
    -- * Exception tag
      Throws

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

    -- * MonadException
    , MonadException(..)
    , handle
    , handle'
    , try
    , mapException
    , onException
    , onException'
    )
    where

import Prelude hiding (catch)

import Control.Exception (Exception)
import qualified Control.Exception as IOE
    ( SomeException
    , catch
    , throwIO
    )
import Control.Monad (liftM)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Identity (IdentityT(..))
import qualified Control.Monad.Trans.Identity as Identity (liftCatch)
import Control.Monad.Trans.List (ListT(..))
import qualified Control.Monad.Trans.List as List (liftCatch)
import Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Control.Monad.Trans.Maybe as Maybe (liftCatch)
import Control.Monad.Trans.Reader (ReaderT(..))
import qualified Control.Monad.Trans.Reader as Reader (liftCatch)
import qualified Control.Monad.Trans.RWS.Lazy as RWSL (RWST(..), liftCatch)
import qualified Control.Monad.Trans.RWS.Strict as RWSS (RWST(..), liftCatch)
import qualified Control.Monad.Trans.State.Lazy as SL (StateT(..), liftCatch)
import qualified Control.Monad.Trans.State.Strict as SS (StateT(..), liftCatch)
import qualified Control.Monad.Trans.Writer.Lazy as WL (WriterT(..), liftCatch)
import qualified Control.Monad.Trans.Writer.Strict as WS
    (WriterT(..), liftCatch)
import Data.Monoid (Monoid)

import Control.Monad.TaggedException.Internal (Throws)
import qualified Control.Monad.TaggedException.Internal as Unsafe
    ( embedT
    , flipT
    , hideOne
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


-- {{{ MonadException ---------------------------------------------------------

-- | Minimal complete definition consists of 'throw' and 'catch'' methods.
class (Monad m) => MonadException m where
    -- | Throw an exception.  To raise an \"inner\" exception following can be
    -- used:
    --
    -- > liftT . throw
    -- >     :: (MonadException m, Exception e, Exception e')
    -- >     => e
    -- >     -> Throws e' (Throws e m) a
    throw
        :: (Exception e)
        => e
        -> Throws e m a

    -- | Catch exception. Default implementation provided using 'catch''.  To
    -- catch inner exception following construct can be used:
    --
    -- > catch . flipT
    -- >     :: (Exception e, Exception e', MonadException m)
    -- >     => Throws e' (Throws e m) a
    -- >     -> (e -> Throws e' m a)
    -- >     -> Throws e' m a
    catch
        :: (Exception e)
        => Throws e m a
        -> (e -> m a)
        -> m a
    catch = catch' . Unsafe.hideOne
    {-# INLINE catch #-}

    -- | Catch any exception. For @IO@ monad this method behaves as
    -- @Control.Exception.catch@.
    catch'
        :: (Exception e)
        => m a
        -> (e -> m a)
        -> m a

-- | Flipped version of 'catch'. Usage example:
--
-- > foo = handle exceptionHandler $ do
-- >     ...
-- >   where exceptionHandler = ...
--
-- Handle \"inner\" exception:
--
-- > insideT . handle
-- >     :: (MonadException m, Exception e, Exception e')
-- >     => (e' -> m a)
-- >     -> Throws e (Throws e' m) a
-- >     -> Throws e m a
handle
    :: (Exception e, MonadException m)
    => (e -> m a)
    -> Throws e m a
    -> m a
handle = flip catch
{-# INLINE handle #-}

-- | Flipped version of 'catch''
handle'
    :: (Exception e, MonadException m)
    => (e -> m a)
    -> m a
    -> m a
handle' = flip catch'
{-# INLINE handle' #-}

-- | Similar to 'catch', but returns 'Either' exception or result.
--
-- Inner try:
--
-- > try . flipT
-- >     :: (Exception e, Exception e', MonadException m)
-- >     => Throws e' (Throws e m) a
-- >     -> Throws e' m (Either e a)
try
    :: (Exception e, MonadException m)
    => Throws e m a
    -> m (Either e a)
try m = liftM Right m `catch` (return . Left)

-- | Map one exception to another.
--
-- Mapping \"inner\" exception has generally two forms:
--
-- 1. Modifying raised exception, but not changing its type:
--
-- > liftT1 . mapException
-- >     :: (Exception e, Exception e', MonadException m)
-- >     => (e -> e)
-- >     -> Throws e' (Throws e m) a
-- >     -> Throws e' (Throws e m) a
--
-- 2. Modifying raised exception, including its type:
--
-- > insideT . mapException
-- >     :: (Exception e, Exception e1, Exception e2, MonadException m)
-- >     => (e1 -> e2)
-- >     -> Throws e (Throws e1 m) a
-- >     -> Throws e (Throws e2 m) a
--
-- Unhiding exception by mapping it in to a different type of exception:
--
-- > \ f -> mapException f . liftT
-- >     :: (Exception e, Exception e', MonadException m)
-- >     => (e -> e')
-- >     -> m a
-- >     -> Throws e' m a
mapException
    :: (Exception e, Exception e', MonadException m)
    => (e -> e')
    -> Throws e m a
    -> Throws e' m a
mapException = flip (catch . flipT . liftT) . (throw .)

-- | Run computation if exception was raised. Implemented as:
--
-- > m `onException` n = liftT m `catch` \ e -> liftT n >> throw e@
onException
    :: (Exception e, MonadException m)
    => Throws e m a
    -- ^ Computation that may raise exception
    -> m b
    -- ^ The computation to run if an exception @e@ is raised
    -> Throws e m a
m `onException` n = liftT m `catch` \ e -> liftT n >> throw e

-- | Same as 'onException', but uses 'catch'' and so second computation is
-- executed if any exception is raised.
onException'
    :: (MonadException m)
    => m a
    -- ^ Computation that may raise exception
    -> m b
    -- ^ The computation to run if an exception is raised
    -> m a
m `onException'` n = m `catch'` \ e ->
    n >> Unsafe.hideOne (throw (e :: IOE.SomeException))

-- {{{ MonadException -- Instances --------------------------------------------

instance (Exception e, MonadException m) => MonadException (Throws e m) where
    -- throw :: (Exception e') => e' -> Throws e' (Throws e m) a
    throw = Unsafe.throwsTwo . Unsafe.hideOne . throw
    {-# INLINE throw #-}
    -- catch'
    --     :: (Exception e')
    --     => Throws e m a
    --     -> (e' -> Throws e m a)
    --     -> Throws e m a
    catch' m =
        Unsafe.throwsOne .  catch' (Unsafe.hideOne m) . (Unsafe.hideOne .)
    {-# INLINE catch' #-}

instance MonadException IO where
    throw = Unsafe.throwsOne . IOE.throwIO
    {-# INLINE throw #-}
    catch' = IOE.catch
    {-# INLINE catch' #-}

-- {{{ MonadException -- Instances -- transformers ----------------------------
-- (sorted alphabetically)

instance (MonadException m) => MonadException (IdentityT m) where
    throw = insideT lift . throw
    {-# INLINE throw #-}
    catch' = Identity.liftCatch catch'
    {-# INLINE catch' #-}

instance (MonadException m) => MonadException (ListT m) where
    throw = insideT lift . throw
    {-# INLINE throw #-}
    catch' = List.liftCatch catch'
    {-# INLINE catch' #-}

instance (MonadException m) => MonadException (MaybeT m) where
    throw = insideT lift . throw
    {-# INLINE throw #-}
    catch' = Maybe.liftCatch catch'
    {-# INLINE catch' #-}

instance (MonadException m) => MonadException (ReaderT r m) where
    throw = insideT lift . throw
    {-# INLINE throw #-}
    catch' = Reader.liftCatch catch'
    {-# INLINE catch' #-}

instance (Monoid w, MonadException m) => MonadException (RWSL.RWST r w s m)
  where
    throw = insideT lift . throw
    {-# INLINE throw #-}
    catch' = RWSL.liftCatch catch'
    {-# INLINE catch' #-}

instance (Monoid w, MonadException m) => MonadException (RWSS.RWST r w s m)
  where
    throw = insideT lift . throw
    {-# INLINE throw #-}
    catch' = RWSS.liftCatch catch'
    {-# INLINE catch' #-}

instance (MonadException m) => MonadException (SL.StateT s m) where
    throw = insideT lift . throw
    {-# INLINE throw #-}
    catch' = SL.liftCatch catch'
    {-# INLINE catch' #-}

instance (MonadException m) => MonadException (SS.StateT s m) where
    throw = insideT lift . throw
    {-# INLINE throw #-}
    catch' = SS.liftCatch catch'
    {-# INLINE catch' #-}

instance (Monoid w, MonadException m) => MonadException (WL.WriterT w m) where
    throw = insideT lift . throw
    {-# INLINE throw #-}
    catch' = WL.liftCatch catch'
    {-# INLINE catch' #-}

instance (Monoid w, MonadException m) => MonadException (WS.WriterT w m) where
    throw = insideT lift . throw
    {-# INLINE throw #-}
    catch' = WS.liftCatch catch'
    {-# INLINE catch' #-}

-- }}} MonadException -- Instances -- transformers ----------------------------
-- }}} MonadException -- Instances --------------------------------------------
-- }}} MonadException ---------------------------------------------------------

-- {{{ Exception tag -- Combinators -------------------------------------------

-- | Construct exception tag, with type restrictions.
--
-- Reflect raised exception in function's type:
--
-- > import Control.Monad.TaggedException (Throws, liftT)
-- > import System.IO (Handle, IOMode)
-- > import qualified System.IO as IO (openFile)
-- >
-- >
-- > openFile :: FilePath -> IOMode -> Throws IOError IO Handle
-- > openFile = (liftT .) . IO.openFile
--
-- Lifting @m@ to @'Throws' e m@:
--
-- > import Control.Monad.TaggedException (Exception, Throws, liftT, throw)
-- > import Data.Typeable (Typeable)
-- >
-- >
-- > data EmptyString = EmptyString
-- >     deriving (Show, Typeable)
-- >
-- > instance Exception EmptyString
-- >
-- > writeIfNotEmpty
-- >     :: FilePath
-- >     -> String
-- >     -> Throws EmptyString IO ()
-- > writeIfNotEmpty filename str = do
-- >     when (null str) $ throw EmptyString
-- >     liftT $ writeFile filename str
--
-- We have a some commonly used patterns:
--
-- > (liftT .)
-- >     :: (Exception e, MonadException m)
-- >     => (a -> m b)
-- >     -> a -> Throws e m b
--
-- Above is also usable for lifting throw-like functions:
--
-- > import Control.Monad.Trans.Class (MonadTrans(lift))
-- >
-- > ((liftT . lift) .)
-- >     :: (Exception e, MonadException m, MonadException (t m), MonadTrans t)
-- >     => (a -> m b)
-- >     -> a -> Throws e (t m) b
liftT :: (Exception e, MonadException m) => m a -> Throws e m a
liftT = Unsafe.throwsOne
{-# INLINE liftT #-}

-- | Shorthand for @liftT . liftT@.
lift2T
    :: (Exception e, Exception e', MonadException m)
    => m a
    -> Throws e' (Throws e m) a
lift2T = Unsafe.throwsTwo
{-# INLINE lift2T #-}

-- | Shorthand for @liftT . liftT . liftT@.
lift3T
    :: (Exception e, Exception e', Exception e'', MonadException m)
    => m a
    -> Throws e'' (Throws e' (Throws e m)) a
lift3T = Unsafe.throwsThree
{-# INLINE lift3T #-}

-- | @liftT@ for functions with arity one.
liftT1
    :: (Exception e, MonadException m)
    => (m a -> m b)
    -> Throws e m a -> Throws e m b
liftT1 = Unsafe.liftT1
{-# INLINE liftT1 #-}

-- | @liftT@ for functions with arity two.
liftT2
    :: (Exception e, MonadException m)
    => (m a -> m b -> m c)
    -> Throws e m a -> Throws e m b -> Throws e m c
liftT2 = Unsafe.liftT2
{-# INLINE liftT2 #-}

-- | @liftT@ for functions with arity three.
liftT3
    :: (Exception e, MonadException m)
    => (m a -> m b -> m c -> m d)
    -> Throws e m a -> Throws e m b -> Throws e m c -> Throws e m d
liftT3 = Unsafe.liftT3
{-# INLINE liftT3 #-}

-- | Join two outermost exception tags.
joinT
    :: (Exception e, MonadException m)
    => Throws e (Throws e m) a
    -> Throws e m a
joinT = Unsafe.joinT
{-# INLINE joinT #-}

-- | Join three outermost exception tags.
joinT3
    :: (Exception e, MonadException m)
    => Throws e (Throws e (Throws e m)) a
    -> Throws e m a
joinT3 = Unsafe.joinT3
{-# INLINE joinT3 #-}

-- | Flip two outermost exception tags.
flipT
    :: (Exception e, Exception e', MonadException m)
    => Throws e' (Throws e m) a
    -> Throws e (Throws e' m) a
flipT = Unsafe.flipT
{-# INLINE flipT #-}

-- | Generalized 'liftT'. Usage examples:
--
-- > insideT lift
-- >    :: (MonadException (t m), MonadException m, Exception e, MonadTrans t)
-- >    => Throws e m b
-- >    -> Throws e (t m) b
--
-- Above example can be used when defining 'MonadException' instance for some
-- monad transformer:
--
-- > insideT lift . throw
-- >     :: (MonadException (t m), MonadException m, Exception e, MonadTrans t)
-- >     => e
-- >     -> Throws e (t m) b
--
-- This is variation on the first example that explicitly lifts monad:
--
-- > insideT WriterT
-- >     :: (Exception e, MonadException m, Monoid w)
-- >     => Throws e m (b, w)
-- >     -> Throws e (WriterT w m) b
--
-- Some useful compositions of exception tag combinators:
--
-- > insideT flipT
-- >     :: (Exception e0, Exception e1, Exception e2, MonadException m)
-- >     => Throws e0 (Throws e1 (Throws e2 m)) a
-- >     -> Throws e0 (Throws e2 (Throws e1 m)) a
--
-- > flipT . insideT flipT
-- >     :: (Exception e0, Exception e1, Exception e2, MonadException m)
-- >     => Throws e0 (Throws e1 (Throws e2 m)) a
-- >     -> Throws e2 (Throws e0 (Throws e1 m)) a
insideT
    :: (Exception e, MonadException m, MonadException m')
    => (m a -> m' b)
    -> Throws e m a -> Throws e m' b
insideT = Unsafe.insideT
{-# INLINE insideT #-}

-- | Variant of 'insideT'.
--
-- Usage example:
--
-- > insideTf StateT
-- >     :: (Exception e, MonadException m)
-- >     => (s -> Throws e m (a, s))
-- >     -> Throws e (StateT s m) a
insideTf
    :: (Exception e, Functor f, MonadException m, MonadException m')
    => (f (m a) -> m' b)
    -> f (Throws e m a)
    -> Throws e m' b
insideTf = Unsafe.insideTf
{-# INLINE insideTf #-}

-- | Variant of 'insideT'.
--
-- Usage example:
--
-- > insideTf RWST
-- >     :: (Exception e, MonadException m)
-- >     => (r -> s -> Throws e m (a, s, w))
-- >     -> Throws e (RWST r w s m) a
insideTf2
    :: (Exception e, Functor f, Functor f', MonadException m, MonadException m')
    => (f (f' (m a)) -> m' b)
    -> f (f' (Throws e m a))
    -> Throws e m' b
insideTf2 = Unsafe.insideTf2
{-# INLINE insideTf2 #-}

-- | Generalized 'liftT2'.
insideT2
    :: (Exception e, MonadException m1, MonadException m2, MonadException m3)
    => (m1 a -> m2 b -> m3 c)
    -> Throws e m1 a -> Throws e m2 b -> Throws e m3 c
insideT2 = Unsafe.insideT2
{-# INLINE insideT2 #-}

-- | Generalized 'liftT3'.
insideT3
    :: (Exception e, MonadException m1, MonadException m2, MonadException m3,
        MonadException m4)
    => (m1 a -> m2 b -> m3 c -> m4 d)
    -> Throws e m1 a -> Throws e m2 b -> Throws e m3 c -> Throws e m4 d
insideT3 = Unsafe.insideT3
{-# INLINE insideT3 #-}

-- |
--
-- Since @1.2.0.0@
embedT :: (Exception e, MonadException m, MonadException m')
    => (m a -> Throws e m' b)
    -> Throws e m a -> Throws e m' b
embedT = Unsafe.embedT

-- }}} Exception tag -- Combinators -------------------------------------------
