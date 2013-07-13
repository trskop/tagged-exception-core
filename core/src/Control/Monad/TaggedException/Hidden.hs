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
    ( HiddenException(..)
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


-- | Class for exception that can be removed from the type signature. Default
-- implementation for 'hide' method is provided.
class (Exception e) => HiddenException e where
    -- | Hide exception tag.
    hide :: (MonadException m) => Throws e m a -> m a
    hide = Unsafe.hideOne
    {-# INLINE hide #-}

-- {{{ HiddenException -- Instances -------------------------------------------
-- (sorted alphabeticaly)

instance HiddenException Dynamic
instance HiddenException E.ArithException
instance HiddenException E.ArrayException
instance HiddenException E.AssertionFailed
instance HiddenException E.AsyncException
#if MIN_VERSION_base(4,2,0)
instance HiddenException E.BlockedIndefinitelyOnMVar
instance HiddenException E.BlockedIndefinitelyOnSTM
#else
instance HiddenException E.BlockedIndefinitely
instance HiddenException E.BlockedOnDeadMVar
#endif
instance HiddenException E.Deadlock
instance HiddenException E.ErrorCall
instance HiddenException E.IOException
instance HiddenException E.NestedAtomically
instance HiddenException E.NoMethodError
instance HiddenException E.NonTermination
instance HiddenException E.PatternMatchFail
instance HiddenException E.RecConError
instance HiddenException E.RecSelError
instance HiddenException E.RecUpdError
instance HiddenException E.SomeException
instance HiddenException ExitCode

-- }}} HiddenException -- Instances -------------------------------------------

-- | Map exception before hiding it.
--
-- This is the preferred way to do exception hiding, by mapping it in to a
-- different exception that better describes its fatality.
hideWith
    :: (Exception e, HiddenException e', MonadException m)
    => (e -> e')
    -> Throws e m a
    -> m a
hideWith f = (`Core.catch` \ e -> throwHidden (f e))

-- | Throw exceptions and then disregard type tag.
throwHidden
    :: (HiddenException e, MonadException m)
    => e
    -> m a
throwHidden = hide . Core.throw
{-# INLINE throwHidden #-}

-- | Alias for @throwHidden@.
throw'
    :: (HiddenException e, MonadException m)
    => e
    -> m a
throw' = throwHidden
{-# INLINE throw' #-}
