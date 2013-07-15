{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module:       $HEADER$
-- Description:  Comonad and ComonadApply instances for Throws.
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  portable
--
-- Comonad and ComonadApply instances for @'Throws' e w@.
--
-- Importing:
--
-- > import Control.Monad.TaggedException.Instances.Comonad ()
module Control.Monad.TaggedException.Instances.Comonad ()
    where

import Control.Comonad
import Control.Monad.TaggedException.Core (Throws)
import qualified Control.Monad.TaggedException.Unsafe as Unsafe
    ( hideOne
    , insideT
    , liftT1
    , liftT2
    , throwsOne
    )


instance Comonad w => Comonad (Throws e w) where
    -- extract :: Throws e w a -> a
    extract = extract . Unsafe.hideOne

    -- duplicate :: Throws e w a -> Throws e w (Throws e w a)
    duplicate = Unsafe.insideT (extend Unsafe.throwsOne)
    -- ^ Definition depends on "duplicate = extend id" law being valid.

    -- extend :: (Throws e w a -> b) -> Throws e w a -> Throws e w b
    extend f = Unsafe.liftT1 (extend (f . Unsafe.throwsOne))

instance ComonadApply w => ComonadApply (Throws e w) where
    -- (<@>) :: Throws e w (a -> b) -> Throws e w a -> Throws e w b
    (<@>) = Unsafe.liftT2 (<@>)

    -- (@>) :: Throws e w a -> Throws e w b -> Throws e w b
    (@>) = Unsafe.liftT2 (@>)

    -- (<@) :: Throws e w a -> Throws e w b -> Throws e w a
    (<@) = Unsafe.liftT2 (<@)
