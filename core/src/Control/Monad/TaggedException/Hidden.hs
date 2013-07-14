{-# LANGUAGE CPP #-}
-- |
-- Module:       $HEADER$
-- Description:  Support for hidden exceptions.
-- Copyright:    (c) 2009 - 2013 Peter Trsko
-- License:      BSD3
--
-- Stability:    provisional
-- Portability:  non-portable (CPP, depends on non-portable module)
module Control.Monad.TaggedException.Hidden
    (
    -- * HiddenException class
    --
    -- | Since 'HiddenException' provides default implementation for 'hide'
    -- method making instances of it is trivial. Example of how to create
    -- instance of HiddenException:
    --
    -- > data MyException = MyException String
    -- >   deriving (Typeable)
    -- >
    -- > instance Show MyException where
    -- >     showsPrec _ (MyException msg) =
    -- >         showString "MyException: " . shows msg
    -- >
    -- > instance Exception MyException
    -- > instance HiddenException MyException
      HiddenException(..)

    -- ** Mapping existing visible exception to hidden ones
    --
    -- | This is a prefered way of hiding exceptions. Difference from just
    -- hiding the type tag and mapping it in to hidden exception is that in
    -- later case we can provide additional information. Most important is to
    -- specify why that particluar exception was hidden.
    --
    -- Example:
    --
    -- > data UnrecoverableException
    -- >     = UnrecoverableIOException String IOException
    -- >   deriving (Typeable)
    -- >
    -- > instance Show UnrecoverableException where
    -- >     showsPrec _ (UnrecoverableIOException info e)
    -- >         showString "Unrecoverable exception occurred in "
    -- >         . showString info . showString ": " . shows e
    -- >
    -- > instance Exception UnrecoverableException
    -- > instance HiddenException UnrecoverableException
    -- >
    -- > hideIOException
    -- >     :: (MonadException e)
    -- >     => String
    -- >     -> Throws IOException m a
    -- >     -> m a
    -- > hideIOException = hideWith . UnrecoverableIOException
    , hideWith

    -- ** Raising hidden exceptions
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
