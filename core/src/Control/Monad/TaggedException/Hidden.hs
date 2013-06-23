{-# LANGUAGE CPP #-}
-- |
-- Module:       $HEADER$
-- Description:  Support for hidden exceptions.
-- Copyright:    (c) 2009 - 2013 Peter Trsko
-- License:      BSD3
--
-- Stability:    provisional
-- Portability:  non-portable (CPP, depends on non-portable module)
--
-- Support for hidden exceptions.
module Control.Monad.TaggedException.Hidden
    ( HidableException(..)
    , hideWith
    , throwHidden
    , throw'
    )
    where

import Control.Exception (Exception)
import qualified Control.Exception as E
    ( ArithException
    , ArrayException
    , AssertionFailed
    , AsyncException
#if MIN_VERSION_base(4,2,0)
    , BlockedIndefinitelyOnMVar
    , BlockedIndefinitelyOnSTM
#else
    , BlockedIndefinitely
    , BlockedOnDeadMVar
#endif
    , Deadlock
    , ErrorCall
    , IOException
    , NestedAtomically
    , NoMethodError
    , NonTermination
    , PatternMatchFail
    , RecConError
    , RecSelError
    , RecUpdError
    , SomeException
    )

import Data.Dynamic (Dynamic)
import System.Exit (ExitCode)

import Control.Monad.TaggedException.Internal as Unsafe (hideOne)
import Control.Monad.TaggedException.Core (MonadException, Throws)
import qualified Control.Monad.TaggedException.Core as Core (catch, throw)


class (Exception e) => HidableException e where
    -- | Hide exception tag.
    hide :: (MonadException m) => Throws e m a -> m a
    hide = Unsafe.hideOne
    {-# INLINE hide #-}

-- {{{ HidableException -- Instances ------------------------------------------
-- (sorted alphabeticaly)

instance HidableException Dynamic
instance HidableException E.ArithException
instance HidableException E.ArrayException
instance HidableException E.AssertionFailed
instance HidableException E.AsyncException
#if MIN_VERSION_base(4,2,0)
instance HidableException E.BlockedIndefinitelyOnMVar
instance HidableException E.BlockedIndefinitelyOnSTM
#else
instance HidableException E.BlockedIndefinitely
instance HidableException E.BlockedOnDeadMVar
#endif
instance HidableException E.Deadlock
instance HidableException E.ErrorCall
instance HidableException E.IOException
instance HidableException E.NestedAtomically
instance HidableException E.NoMethodError
instance HidableException E.NonTermination
instance HidableException E.PatternMatchFail
instance HidableException E.RecConError
instance HidableException E.RecSelError
instance HidableException E.RecUpdError
instance HidableException E.SomeException
instance HidableException ExitCode

-- }}} HidableException -- Instances ------------------------------------------

-- | Map exception before hiding it.
--
-- This is the preferred way to do exception hiding, by mapping it in to a
-- different exception that better describes its fatality.
hideWith
    :: (Exception e, HidableException e', MonadException m)
    => (e -> e')
    -> Throws e m a
    -> m a
hideWith f = (`Core.catch` \ e -> throwHidden (f e))

-- | Throw exceptions and then disregard type tag.
throwHidden
    :: (HidableException e, MonadException m)
    => e
    -> m a
throwHidden = hide . Core.throw
{-# INLINE throwHidden #-}

-- | Alias for @throwHidden@.
throw'
    :: (HidableException e, MonadException m)
    => e
    -> m a
throw' = throwHidden
{-# INLINE throw' #-}
