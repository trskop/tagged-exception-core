{-# LANGUAGE RankNTypes #-}
-- |
-- Module:       $HEADER$
-- Description:  MonadExceptionUtilities class and instances.
-- Copyright:    (c) 2009 - 2013 Peter Trsko.
-- License:      BSD3
--
-- Stability:    provisional
-- Portability:  non-portable (RankNTypes)
--
-- Introduces 'MonadExceptionUtilities' type class that provides means for
-- overriding default implementation of functions like 'bracket'.
module Control.Monad.TaggedException.Utilities
    (
    -- * MonadExceptionUtilities
      MonadExceptionUtilities(..)
    , bracket_
    )
    where

import Prelude hiding (catch)

import Control.Exception (Exception)
import qualified Control.Exception as IOE
    ( bracket
    , bracketOnError
    , finally
    )
import Control.Monad ((>=>))
import Control.Monad.Trans.Identity (IdentityT(..))
import Control.Monad.Trans.List (ListT(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Reader (ReaderT(..))
import qualified Control.Monad.Trans.RWS.Lazy as RWSL (RWST(..))
import qualified Control.Monad.Trans.RWS.Strict as RWSS (RWST(..))
import qualified Control.Monad.Trans.State.Lazy as SL (StateT(..))
import qualified Control.Monad.Trans.State.Strict as SS (StateT(..))
import qualified Control.Monad.Trans.Writer.Lazy as WL (WriterT(..))
import qualified Control.Monad.Trans.Writer.Strict as WS (WriterT(..))
import Data.Monoid (Monoid)

import Control.Monad.TaggedException.Core
    ( MonadException(..)
    , flipT
    , insideT
    , insideT2
    , insideTf
    , liftT
    , liftT2
    , onException
    , onException'
    )
import Control.Monad.TaggedException.Internal (Throws)
import qualified Control.Monad.TaggedException.Internal as Unsafe
    ( hideOne
    , liftMask
    )
import qualified Control.Monad.TaggedException.Internal.IO as InternalIO
    (mask, mask')


-- {{{ MonadExceptionUtilities ------------------------------------------------

-- | Default implementations are provided for all methods.  In most cases at
-- least 'mask'' should be implemented in terms of underlying monad.
class (MonadException m) => MonadExceptionUtilities m where
    -- | Mask asynchronous exceptions, see "Control.Exception".
    --
    -- Default implementation:
    --
    -- > mask' f = f id
    mask' :: ((forall a. m a -> m a) -> m b) -> m b
    mask' f = f id

    -- | Lifted variant of 'mask''.
    --
    -- Default implementation provided using 'mask''.
    mask
        :: (Exception e)
        => ((forall a. Throws e m a -> Throws e m a) -> Throws e m b)
        -> Throws e m b
    mask = Unsafe.liftMask mask'

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
        :: (Exception e)
        => Throws e m a
        -- ^ Computation to run first
        -> m b
        -- ^ Computation to run afterward (even if exception @e@ was raised)
        -> Throws e m a
        -- ^ Returns the result of the first computation
    m `finally` n = mask $ \ restore -> do
        r <- restore m `onException` n
        _ <- liftT n
        return r

    -- | Run computation afeter another even if exception was thrown. See also
    -- 'finally', 'onException' and 'onException''.
    --
    -- Default implementation:
    --
    -- > m `finally'` n = mask' $ \ restore -> do
    -- >     r <- restore m `onException'` n
    -- >     _ <- n
    -- >     return r
    finally'
        :: m a
        -- ^ Computation to run first
        -> m b
        -- ^ Computation to run afterward (even if some exception was raised)
        -> m a
        -- ^ Returns the result of the first computation
    m `finally'` n = mask' $ \ restore -> do
        r <- restore m `onException'` n
        _ <- n
        return r

    -- | Run computation surrounded by acquire and release computations. The
    -- release computation is executed even if \"in-between\" computation
    -- raises exception. See also 'bracket'', 'bracket_', 'bracketOnError',
    -- and 'bracketOnError''.
    --
    -- Default implementation:
    --
    -- > bracket acq rel go = mask $ \ restore -> do
    -- >     x <- liftT acq
    -- >     r <- restore (go x) `onException` rel x
    -- >     _ <- liftT $ rel x
    -- >     return r
    bracket
        :: (Exception e)
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
        :: m a
        -- ^ Computation to run before
        -> (a -> m b)
        -- ^ Computation to run after
        -> (a -> m c)
        -- ^ Computation to run in-between
        -> m c
        -- ^ Result of the in-between computation
    bracket' acq rel go = mask' $ \ restore -> do
        x <- acq
        r <- restore (go x) `onException'` rel x
        _ <- rel x
        return r

    -- | Version of 'bracket' where \"after\" computation is executed only if
    -- \"in-between\" computation raises exception.
    --
    -- Default implementation:
    --
    -- > bracketOnError acq rel go = mask $ \ restore -> do
    -- >     x <- liftT acq
    -- >     restore (go x) `onException` rel x
    bracketOnError
        :: (Exception e)
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
        :: m a
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
    :: (Exception e, MonadExceptionUtilities m)
    => m a
    -- ^ Computation to run before
    -> m b
    -- ^ Computation to run after
    -> Throws e m c
    -- ^ Computation to run in-between
    -> Throws e m c
    -- ^ Result of the in-between computation
bracket_ acq rel go = bracket acq (const rel) (const go)

-- }}} MonadExceptionUtilities ------------------------------------------------

-- {{{ MonadExceptionUtilities -- Instances -----------------------------------

-- | Not exported.
liftBracket
    :: (Exception e, Exception e', MonadException m)
    => (m a -> (a -> m b) -> (a -> Throws e m c) -> Throws e m c)
    -> Throws e' m a
    -> (a -> Throws e' m b)
    -> (a -> Throws e (Throws e' m) c)
    -> Throws e (Throws e' m) c
liftBracket f m n = flipT . liftT
    . f (Unsafe.hideOne m) (Unsafe.hideOne . n) . ((Unsafe.hideOne . flipT) .)

-- | Not exported.
liftBracket'
    :: (Exception e, MonadException m)
    => (m a -> (a -> m b) -> (a -> m c) -> m c)
    -> Throws e m a
    -> (a -> Throws e m b)
    -> (a -> Throws e m c)
    -> Throws e m c
liftBracket' f m n = liftT . f (Unsafe.hideOne m) (Unsafe.hideOne . n)
    . (Unsafe.hideOne .)

instance (Exception e, MonadExceptionUtilities m)
    => MonadExceptionUtilities (Throws e m) where
    mask' = Unsafe.liftMask mask'
    mask = Unsafe.liftMask mask
    finally = (flipT .) . insideT2 finally . flipT
    finally' = liftT2 finally'
    bracket = liftBracket bracket
    bracket' = liftBracket' bracket'
    bracketOnError = liftBracket bracketOnError
    bracketOnError' = liftBracket' bracketOnError'

instance MonadExceptionUtilities IO where
    mask' = InternalIO.mask'
    mask = InternalIO.mask
    bracket' = IOE.bracket
    bracketOnError' = IOE.bracketOnError
    finally' = IOE.finally

-- }}} MonadExceptionUtilities -- Instances -----------------------------------

-- {{{ MonadExceptionUtilities -- Instances -- transformers -------------------
-- (sorted alphabetically)

instance (MonadExceptionUtilities m)
    => MonadExceptionUtilities (IdentityT m)
  where
    mask' f = IdentityT . mask' $ \ restore ->
        runIdentityT $ f (IdentityT . restore . runIdentityT)

    finally' m n = IdentityT $ runIdentityT m `finally'` runIdentityT n

    bracket' acq rel = IdentityT
        . bracket' (runIdentityT acq) (runIdentityT . rel) . (runIdentityT .)

    bracketOnError' acq rel = IdentityT
        . bracketOnError' (runIdentityT acq) (runIdentityT . rel)
        . (runIdentityT .)

    finally m = insideT IdentityT
        . finally (insideT runIdentityT m) . runIdentityT

    bracket acq rel = insideT IdentityT
        . bracket (runIdentityT acq) (runIdentityT . rel)
        . (insideT runIdentityT .)

    bracketOnError acq rel = insideT IdentityT
        . bracketOnError (runIdentityT acq) (runIdentityT . rel)
        . (insideT runIdentityT .)

instance (MonadExceptionUtilities m) => MonadExceptionUtilities (ListT m) where
    mask' f = ListT . mask' $ \ restore ->
        runListT $ f (ListT . restore . runListT)

    finally' m n = ListT $ runListT m `finally'` runListT n

    bracket' acq rel go = ListT $ bracket'
        (runListT acq)
        (runListT . (ListT . return >=> rel))
        (runListT . (ListT . return >=> go))

    bracketOnError' acq rel go = ListT $ bracketOnError'
        (runListT acq)
        (runListT . (ListT . return >=> rel))
        (runListT . (ListT . return >=> go))

    finally m = insideT ListT . finally (insideT runListT m) . runListT

    bracket acq rel go = insideT ListT $ bracket
        (runListT acq)
        (runListT . (ListT . return >=> rel))
        (insideT runListT . (insideT ListT . return >=> go))

    bracketOnError acq rel go = insideT ListT $ bracketOnError
        (runListT acq)
        (runListT . (ListT . return >=> rel))
        (insideT runListT . (insideT ListT . return >=> go))

instance (MonadExceptionUtilities m)
    => MonadExceptionUtilities (MaybeT m)
  where
    mask' f = MaybeT . mask' $ \ restore ->
        runMaybeT $ f (MaybeT . restore . runMaybeT)

    finally' m n = MaybeT $ runMaybeT m `finally'` runMaybeT n

    bracket' acq rel go = MaybeT $ bracket'
        (runMaybeT acq)
        (runMaybeT . (MaybeT . return >=> rel))
        (runMaybeT . (MaybeT . return >=> go))

    bracketOnError' acq rel go = MaybeT $ bracketOnError'
        (runMaybeT acq)
        (runMaybeT . (MaybeT . return >=> rel))
        (runMaybeT . (MaybeT . return >=> go))

    finally m = insideT MaybeT . finally (insideT runMaybeT m) . runMaybeT

    bracket acq rel go = insideT MaybeT $ bracket
        (runMaybeT acq)
        (runMaybeT . (MaybeT . return >=> rel))
        (insideT runMaybeT . (insideT MaybeT . return >=> go))

    bracketOnError acq rel go = insideT MaybeT $ bracketOnError
        (runMaybeT acq)
        (runMaybeT . (MaybeT . return >=> rel))
        (insideT runMaybeT . (insideT MaybeT . return >=> go))

instance (MonadExceptionUtilities m)
    => MonadExceptionUtilities (ReaderT s m)
  where
    mask' f = ReaderT $ \ r ->
        mask' $ \ restore ->
            (`runReaderT` r) $ f (\ x -> ReaderT $ restore . runReaderT x)

    finally' m n = ReaderT $ \ r -> runReaderT m r `finally'` runReaderT n r

    bracket' acq rel go = ReaderT $ \ r -> bracket'
        (runReaderT acq r)
        ((`runReaderT` r) . rel)
        ((`runReaderT` r) . go)

    bracketOnError' acq rel go = ReaderT $ \ r -> bracketOnError'
        (runReaderT acq r)
        ((`runReaderT` r) . rel)
        ((`runReaderT` r) . go)

    finally m n = insideTf ReaderT $ \ r ->
        insideT (`runReaderT` r) m `finally` runReaderT n r

    bracket acq rel go = insideTf ReaderT $ \ r -> bracket
        (runReaderT acq r)
        ((`runReaderT` r) . rel)
        (insideT (`runReaderT` r) . go)

    bracketOnError acq rel go = insideTf ReaderT $ \ r -> bracketOnError
        (runReaderT acq r)
        ((`runReaderT` r) . rel)
        (insideT (`runReaderT` r) . go)

instance (MonadExceptionUtilities m, Monoid w)
    => MonadExceptionUtilities (RWSL.RWST r w s m)
  where
    mask' f = RWSL.RWST $ \ r s ->
        mask' $ \ restore ->
            (\ x -> RWSL.runRWST x r s)
                $ f (\ x -> RWSL.RWST $ (restore .) . RWSL.runRWST x)

instance (MonadExceptionUtilities m, Monoid w)
    => MonadExceptionUtilities (RWSS.RWST r w s m) where

    mask' f = RWSS.RWST $ \ r s ->
        mask' $ \ restore ->
            (\ x -> RWSS.runRWST x r s)
                $ f (\ x -> RWSS.RWST $ (restore .) . RWSS.runRWST x)

instance (MonadExceptionUtilities m)
    => MonadExceptionUtilities (SL.StateT s m)
  where
    mask' f = SL.StateT $ \ s ->
        mask' $ \ restore ->
            (`SL.runStateT` s)
                $ f (\ x -> SL.StateT $ restore . SL.runStateT x)

instance (MonadExceptionUtilities m)
    => MonadExceptionUtilities (SS.StateT s m)
  where
    mask' f = SS.StateT $ \ s ->
        mask' $ \ restore ->
            (`SS.runStateT` s)
                $ f (\ x -> SS.StateT $ restore . SS.runStateT x)

instance (Monoid w, MonadExceptionUtilities m)
    => MonadExceptionUtilities (WL.WriterT w m)
  where
    mask' f = WL.WriterT . mask' $ \ restore ->
        WL.runWriterT $ f (WL.WriterT . restore . WL.runWriterT)

    finally' m n = WL.WriterT $ WL.runWriterT m `finally'` WL.runWriterT n

    bracket' acq rel go = WL.WriterT $ bracket'
        (WL.runWriterT acq)
        (WL.runWriterT . (WL.WriterT . return >=> rel))
        (WL.runWriterT . (WL.WriterT . return >=> go))

    bracketOnError' acq rel go = WL.WriterT $ bracketOnError'
        (WL.runWriterT acq)
        (WL.runWriterT . (WL.WriterT . return >=> rel))
        (WL.runWriterT . (WL.WriterT . return >=> go))

    finally m = insideT WL.WriterT
        . finally (insideT WL.runWriterT m) . WL.runWriterT

    bracket acq rel go = insideT WL.WriterT $ bracket
        (WL.runWriterT acq)
        (WL.runWriterT . (WL.WriterT . return >=> rel))
        (insideT WL.runWriterT . (insideT WL.WriterT . return >=> go))

    bracketOnError acq rel go = insideT WL.WriterT $ bracketOnError
        (WL.runWriterT acq)
        (WL.runWriterT . (WL.WriterT . return >=> rel))
        (insideT WL.runWriterT . (insideT WL.WriterT . return >=> go))

instance (Monoid w, MonadExceptionUtilities m)
    => MonadExceptionUtilities (WS.WriterT w m)
  where
    mask' f = WS.WriterT . mask' $ \ restore ->
        WS.runWriterT $ f (WS.WriterT . restore . WS.runWriterT)

    finally' m n = WS.WriterT $ WS.runWriterT m `finally'` WS.runWriterT n

    bracket' acq rel go = WS.WriterT $ bracket'
        (WS.runWriterT acq)
        (WS.runWriterT . (WS.WriterT . return >=> rel))
        (WS.runWriterT . (WS.WriterT . return >=> go))

    bracketOnError' acq rel go = WS.WriterT $ bracketOnError'
        (WS.runWriterT acq)
        (WS.runWriterT . (WS.WriterT . return >=> rel))
        (WS.runWriterT . (WS.WriterT . return >=> go))

    finally m = insideT WS.WriterT
        . finally (insideT WS.runWriterT m) . WS.runWriterT

    bracket acq rel go = insideT WS.WriterT $ bracket
        (WS.runWriterT acq)
        (WS.runWriterT . (WS.WriterT . return >=> rel))
        (insideT WS.runWriterT . (insideT WS.WriterT . return >=> go))

    bracketOnError acq rel go = insideT WS.WriterT $ bracketOnError
        (WS.runWriterT acq)
        (WS.runWriterT . (WS.WriterT . return >=> rel))
        (insideT WS.runWriterT . (insideT WS.WriterT . return >=> go))

-- }}} MonadExceptionUtilities -- Instances -- transformers -------------------
