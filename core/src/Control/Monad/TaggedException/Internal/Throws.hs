{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
#if MIN_VERSION_base(4,7,0)
{-# LANGUAGE DeriveDataTypeable #-}
#endif
{-# LANGUAGE DeriveGeneric #-}
-- |
-- Module:       $HEADER$
-- Description:  Data type for associating monadic value with phantom type.
-- Copyright:    (c) 2009 - 2014 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    stable
-- Portability:  non-portable (CPP, NoImplicitPrelude, PolyKinds, RankNTypes
--               DeriveDataTypeable, DeriveGeneric)
--
-- Data type for associating monadic value with phantom type. In case of this
-- library it will always be associated with a type of exception it may throw.
module Control.Monad.TaggedException.Internal.Throws
    (
      Throws(..)
    , liftMask
    )
  where

import Control.Applicative (Alternative(..), Applicative(..))
import Control.Monad (Monad((>>), (>>=), fail, return), MonadPlus(..))
import Data.Functor (Functor(fmap))
import Data.Function ((.))
#if MIN_VERSION_base(4,7,0)
import Data.Typeable (Typeable)
#endif
import GHC.Generics (Generic)

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (MonadTrans(lift))

import Control.Monad.Catch (MonadCatch(..), MonadMask(..), MonadThrow(..))
import Control.Monad.Morph (MFunctor(hoist), MMonad(embed))


-- | Exception tag.
newtype Throws e m a = Throws
    { hideException :: m a
    -- ^ Hide one exception.
    }
  deriving
    ( Generic
#if MIN_VERSION_base(4,7,0)
    , Typeable
#endif
    )

-- | Lift @mask@ operation in to 'Throws' context.
liftMask
    :: (((forall a. m a -> m a) -> m b) -> m b)
    -> ((forall a. Throws e m a -> Throws e m a) -> Throws e m b)
    -> Throws e m b
liftMask msk f = (Throws . msk)
    (\restore -> (hideException . f) (Throws . restore . hideException))

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

instance MonadIO m => MonadIO (Throws e m) where
    liftIO = Throws . liftIO

instance MonadTrans (Throws e) where
    lift = Throws

-- {{{ Instances: mmorph ------------------------------------------------------

-- | Since @1.2.0.0@.
instance MFunctor (Throws e) where
    -- :: Monad m => (forall a. m a -> n a) -> Throws e m b -> Throws e n b
    hoist f x =  Throws (f (hideException x))

-- | Since @1.2.0.0@.
instance MMonad (Throws e) where
    -- :: Monad n
    -- => (forall a. m a -> Throws e n a)
    -- -> Throws e m b -> Throws e n b
    embed f x = f (hideException x)

-- }}} Instances: mmorph ------------------------------------------------------

-- {{{ Instances: exceptions --------------------------------------------------

-- | Since @2.0.0.0@.
instance MonadThrow m => MonadThrow (Throws e m) where
    -- throwM :: Exception e' => e' -> Throws m a
    throwM = Throws . throwM

-- | Since @2.0.0.0@.
instance MonadCatch m => MonadCatch (Throws e m) where
    -- :: Exception e' => Throws m a -> (e' -> Throws m a) -> Throws m a
    catch (Throws ma) f = Throws (catch ma (hideException . f))

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
