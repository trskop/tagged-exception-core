-- |
-- Module:       $HEADER$
-- Description:  Unsafe functions.
-- Copyright:    (c) 2009 - 2013 Peter Trsko.
-- License:      BSD3
--
-- Stability:    provisional
-- Portability:  non-portable (depends on non-portable module)
--
-- Unsafe functions. Import this module if creating new 'MonadException'
-- instance(s) that can not be created otherwise.
--
-- Preferably import as:
--
-- > import qualified Control.Monad.TaggedException.Unsafe as Unsafe
module Control.Monad.TaggedException.Unsafe
    (
      throwsOne
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

    -- * Lift operations
    , liftThrow
    , liftMask
    )
    where

import Control.Monad.Trans.Class (MonadTrans(lift))

import Control.Monad.TaggedException.Internal
    ( Throws
    , flipT
    , hideOne
    , hideThree
    , hideTwo
    , insideT
    , insideT2
    , insideT3
    , insideTf
    , insideTf2
    , joinT
    , joinT3
    , liftMask
    , liftT1
    , liftT2
    , liftT3
    , throwsOne
    , throwsThree
    , throwsTwo
    )


-- | Lift @throw@ operation using @'MonadTrans'('lift')@.
liftThrow
    :: (Monad m, MonadTrans t)
    => (e -> Throws e m a)
    -> e
    -> Throws e (t m) a
liftThrow = (insideT lift .)
