{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
#ifdef KIND_POLYMORPHIC_TYPEABLE
{-# LANGUAGE DeriveDataTypeable #-}
#endif
#ifdef GHC_GENERICS
{-# LANGUAGE DeriveGeneric #-}
#endif

-- Following extensions are required for mtl type classes:
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
-- Originally there was a separate package that defined these instances and it
-- used orphan instances.

-- |
-- Module:       $HEADER$
-- Description:  Data type for associating monadic value with phantom type.
-- Copyright:    (c) 2009-2016, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    stable
-- Portability:  CPP, NoImplicitPrelude, PolyKinds, RankNTypes,
--               DeriveDataTypeable, DeriveGeneric
--
-- Data type for associating monadic value with phantom type. In case of this
-- library it will always be associated with a type of exception it may throw.
module Control.Monad.TaggedException.Internal.Throws
    (
      Throws(..)

    , liftBindLike
    , liftCCLike
    , liftEmbedLike
    , liftHoistLike
    , liftMask
    )
  where

import Control.Applicative
    ( Alternative((<|>), empty, many, some)
    , Applicative((<*), (<*>), (*>), pure)
    )
import Control.Monad
    ( Monad((>>), (>>=), fail, return)
    , MonadPlus(mplus, mzero)
    )
import Data.Functor (Functor(fmap))
import Data.Function ((.))
import Data.Monoid (Monoid)
#ifdef KIND_POLYMORPHIC_TYPEABLE
import Data.Typeable (Typeable)
#endif
#ifdef GHC_GENERICS
import GHC.Generics (Generic, Generic1)
#endif

import Control.Monad.Cont.Class (MonadCont(callCC))
import Control.Monad.Error.Class (MonadError(catchError, throwError))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader.Class (MonadReader(ask, local, reader))
import Control.Monad.RWS.Class (MonadRWS)
import Control.Monad.State.Class (MonadState(get, put, state))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Writer.Class (MonadWriter(listen, pass, tell, writer))

import Control.Monad.Catch
    ( MonadCatch(catch)
    , MonadMask(mask, uninterruptibleMask)
    , MonadThrow(throwM)
    )
import Control.Monad.Morph (MFunctor(hoist), MMonad(embed))


-- | Exception tag.
newtype Throws e m a = Throws
    { hideException :: m a
    -- ^ Hide one exception.
    }
#ifdef GHC_GENERICS
    -- GHC Generics were introduced earlier then polykinded Typeable.
  deriving
    ( Generic
    , Generic1
#ifdef KIND_POLYMORPHIC_TYPEABLE
    , Typeable
#endif
    )
#endif

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
liftBindLike f x g = Throws (f (hideException x) (hideException . g))
{-# INLINE liftBindLike #-}

-- | Lift operation with type similar to 'Control.Monad.Cont.Class.liftCC':
--
-- @
-- 'Control.Monad.Cont.Class.liftCC'
--     :: 'Control.Monad.Monad' m => ((a -> m b) -> m a) -> m a
-- @
--
-- Since @2.1.0.0@
liftCCLike
    :: (((a -> m b) -> m' c) -> m'' d)
    -> ((a -> Throws e m b) -> Throws e m' c) -> Throws e m'' d
liftCCLike f g = Throws (f (\h -> hideException (g (Throws . h))))
  -- f :: ((a -> m b) -> m c) -> m d
  -- g :: (a -> Throws e m b) -> Throws e m c
  -- \h -> hideException (g (Throws . h) :: (a -> m b) -> m c
{-# INLINE liftCCLike #-}

-- | Lift operation similar to the one of 'Control.Exception.mask'.
liftMask
    :: (((forall a. m a -> m a) -> m b) -> m b)
    -> ((forall a. Throws e m a -> Throws e m a) -> Throws e m b)
    -> Throws e m b
liftMask msk f = Throws (msk (\restore -> hideException (f (liftT restore))))
  where
    liftT :: (m a -> m a) -> (Throws e m a -> Throws e m a)
    liftT g (Throws m) = Throws (g m)
{-# INLINABLE liftMask #-}

-- {{{ Instances --------------------------------------------------------------

instance Functor f => Functor (Throws e f) where
    -- (a -> b) -> Throws e f a -> Throws e f b
    fmap f (Throws ma) = Throws (fmap f ma)

instance Applicative f => Applicative (Throws e f) where
    pure = Throws . pure
    Throws x <*> Throws y = Throws (x <*> y)
    Throws x *> Throws y = Throws (x *> y)
    Throws x <* Throws y = Throws (x <* y)

instance Alternative f => Alternative (Throws e f) where
    empty = Throws empty
    Throws x <|> Throws y = Throws (x <|> y)
    some (Throws ma) = Throws (some ma)
    many (Throws ma) = Throws (many ma)

instance Monad m => Monad (Throws e m) where
    return = Throws . return
    Throws ma >>= f = Throws (ma >>= hideException . f)
    Throws ma >> Throws na = Throws (ma >> na)
    fail = Throws . fail

instance MonadPlus m => MonadPlus (Throws e m) where
    mzero = Throws mzero
    Throws m `mplus` Throws n = Throws (m `mplus` n)

-- {{{ Instances: transformers ------------------------------------------------

instance MonadIO m => MonadIO (Throws e m) where
    liftIO = Throws . liftIO

instance MonadTrans (Throws e) where
    lift = Throws

-- }}} Instances: transformers ------------------------------------------------

-- {{{ Instances: mtl ---------------------------------------------------------

-- | Since @2.1.0.0@
instance (Monoid w, MonadWriter w m) => MonadWriter w (Throws e m) where
    writer = Throws . writer
    tell   = Throws . tell
    listen (Throws x) = Throws (listen x)
    pass   (Throws x) = Throws (pass   x)

-- | Since @2.1.0.0@
instance MonadState s m => MonadState s (Throws e m) where
    get   = Throws get
    put   = Throws . put
    state = Throws . state

-- | Since @2.1.0.0@
instance MonadReader r m => MonadReader r (Throws e m) where
    ask = Throws ask
    local f (Throws x) = Throws (local f x)
    reader = Throws . reader

-- | Since @2.1.0.0@
instance (Monoid w, MonadReader r m, MonadWriter w m, MonadState s m)
    => MonadRWS r w s (Throws e m)

-- | Since @2.1.0.0@
instance MonadError e m => MonadError e (Throws e' m) where
    throwError = Throws . throwError
    catchError = liftBindLike catchError

-- | Since @2.1.0.0@
instance MonadCont m => MonadCont (Throws e m) where
    -- :: ((a -> Throws e m b) -> Throws e m a) -> Throws e m a
    callCC = liftCCLike callCC

-- {{{ Instances: mtl ---------------------------------------------------------

-- {{{ Instances: mmorph ------------------------------------------------------

-- | Generalized form of 'embed' from @instance 'MFunctor' ('Throws' e)@.
--
-- Since @2.1.0.0@
liftHoistLike :: (forall a. m a -> n a) -> Throws e m b -> Throws e' n b
liftHoistLike f x = Throws (f (hideException x))
{-# INLINE liftHoistLike #-}

-- | Generalized form of 'embed' from @instance 'MMonad' ('Throws' e)@.
--
-- Since @2.1.0.0@
liftEmbedLike
    :: (forall a. m a -> Throws e n a)
    -> Throws e' m b -> Throws e n b
liftEmbedLike f x = f (hideException x)
{-# INLINE liftEmbedLike #-}

-- | Since @1.2.0.0@.
instance MFunctor (Throws e) where
    -- :: Monad m => (forall a. m a -> n a) -> Throws e m b -> Throws e n b
    hoist = liftHoistLike

-- | Since @1.2.0.0@.
instance MMonad (Throws e) where
    -- :: Monad n
    -- => (forall a. m a -> Throws e n a)
    -- -> Throws e m b -> Throws e n b
    embed = liftEmbedLike

-- }}} Instances: mmorph ------------------------------------------------------

-- {{{ Instances: exceptions --------------------------------------------------

-- | Since @2.0.0.0@.
instance MonadThrow m => MonadThrow (Throws e m) where
    -- throwM :: Exception e' => e' -> Throws m a
    throwM = Throws . throwM

-- | Since @2.0.0.0@.
instance MonadCatch m => MonadCatch (Throws e m) where
    -- :: Exception e' => Throws m a -> (e' -> Throws m a) -> Throws m a
    catch = liftBindLike catch

-- | Since @2.0.0.0@.
instance MonadMask m => MonadMask (Throws e m) where
    -- :: ((forall a. Throws e m a -> Throws e m a) -> Throws e m b)
    -- -> Throws e m b
    mask = liftMask mask

    -- :: ((forall a. Throws e m a -> Throws e m a) -> Throws e m b)
    -- -> Throws e m b
    uninterruptibleMask = liftMask uninterruptibleMask

-- }}} Instances: exceptions --------------------------------------------------
-- }}} Instances --------------------------------------------------------------
