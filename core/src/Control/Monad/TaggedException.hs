{-# LANGUAGE CPP #-}
-- |
-- Module:       $HEADER$
-- Copyright:    (c) 2009 - 2013 Peter Trsko
-- License:      BSD3
--
-- Stability:    provisional
-- Portability:  non-portable (CPP, depends on non-portable module)
module Control.Monad.TaggedException
    (
    -- * Introduction
    --
    -- | This library provides interface that is similar to
    -- /extensible-exceptions/. It introduces 'MonadException' class, which
    -- uses phantom type to tag code that may raise exception.  Intention is to
    -- make exceptions explicit and to enforce exception handling.
    --
    -- This approach is based on commonly used techniques:
    --
    -- * Phantom Types <http://www.haskell.org/haskellwiki/Phantom_type>
    --
    -- * Type Witnesses <http://www.haskell.org/haskellwiki/Type_witness>

    -- ** Why use this?
    --
    -- | Exceptions are one of the fastest and most scalable ways of handling
    -- failures and errors. One of the downsides of exceptions as defined in
    -- Haskell is that they aren't visible in type signatures as in example
    -- when using @Maybe@ or @ErrorT@.
    --
    -- This library tries to get rid of this issue by making exceptions
    -- visible. On the other hand it makes things little more complicated, but
    -- fortunatelly not too complicated.
    --
    -- Some of the benefits of this approach are listed bellow.

    -- *** Unification of exception handling
    --
    -- | Raising and handling exception becomes the same for all
    -- 'MonadException' instance including @IO@ Code that used exception in
    -- @IO@ monad or @ErrorT@ style error handling can be easily modified to
    -- use API defined by this library.
    --
    -- For ilustration there is a great summary of varios ways of error
    -- handling in Haskell:
    --
    -- * /8 ways to report errors in Haskell revisited/
    --   <http://blog.ezyang.com/2011/08/8-ways-to-report-errors-in-haskell-revisited/>
    --
    -- * <http://www.randomhacks.net/articles/2007/03/10/haskell-8-ways-to-report-errors/>
    --
    -- Above ilustrates that any unification or framework for transforming one
    -- error handling technique to another are very benefitial in practice.

    -- *** Avoiding fail
    --
    -- | Sometimes @'Monad'('fail')@ is used to generalize exception handling.
    -- While it provides a generalized interface it also introduces controversy
    -- that sorrounds 'fail'.
    --
    -- This library allows usege of similar approach without using 'fail' and
    -- with explicitly visible exception.
    --
    -- Instead of function like:
    --
    -- > lookup
    -- >     :: (Monad m)
    -- >     => Container Key Value
    -- >     -> Key
    -- >     -> m Value
    --
    -- this library allows to write:
    --
    -- > lookup
    -- >     :: (MonadException m)
    -- >     => Container Key Value
    -- >     -> Key
    -- >     -> Throw LookupFailure m Value
    --
    -- where @LookupFailure@ is instance of 'Exception' class. While in some
    -- ways it's similar to using @ErrorT@, it has all the flexibility of
    -- /extensible-exceptions/ for arbitrary 'MonadException' instance.

    -- ** Dependencies
    --
    -- | This package is trying to keep dependencies at minimum.  Here is list
    -- of current dependencies:
    --
    -- * /base/
    --
    -- * /extensible-exceptions/ for /4 >= base < 4.2/
    --
    -- * /transformers >= 0.2 && 0.4/: De facto current standard for monad
    --   transformers.  Included in newer versions of HaskellPlatform.

    -- ** Naming conventions
    --
    -- | Names of basic functions are the same as those in @Control.Exception@
    -- module, but differ in it's type signature.  They operate on tagged code
    -- and are therefore limited to operate only on exceptions specified by the
    -- phantom type.
    --
    -- Exception, to above rule, is 'throw' function which does not throw
    -- exception from pure code, as does @Control.Exception.throw@, but from
    -- monadic code.  So, it is more equivalent to @Control.Exception.throwIO@.

    -- *** \<function\> vs. \<function\>'
    --
    -- | Functions with prime at the end of there name aren't restricted by the
    -- phantom type while those without it are.  Functions with prime can
    -- therefore operate on arbitrary exceptions.  Use such functions when
    -- operating on exceptions that are different from exception phantom type,
    -- i.e. hidden ones.
    --
    -- In case of @IO@ monad, primed functions behave as those from
    -- @Control.Exception@ module with the same name, but without prime of
    -- course.

    -- *** lift\<n\>T vs. liftT\<n\>
    --
    -- | The @lift\<n\>T@ are basicaly saying lift \<n\> times (e.g. @'lift2T'
    -- = 'liftT' . 'liftT'@) while @liftT\<n\>@ says lift one time but operate
    -- on function with arity \<n\>. This was choosen to be consistent with
    -- @liftM@, @liftM2@, @liftA@, @liftA2@, etc.

    -- * Usage
    -- $usage

    -- ** Importing
    --
    -- | Function 'catch' clashes with @Prelude(catch)@, so either import with
    -- hidden @Prelude(catch)@:
    --
    -- > import Prelude hiding (catch)
    -- > import Control.Monad.ExceptionWitness
    --
    -- or import:
    --
    -- > import Control.Monad.ExceptionWitness as E
    --
    -- and then use @E.catch@, in later case you can also use qualified import:
    --
    -- > import qualified Control.Monad.ExceptionWitness as E
    --
    -- It is recomended to use explicit import list or, as mentioned before,
    -- qualified import. See also /Import modules properly/ on /Haskell Wiki/:
    -- <http://www.haskell.org/haskellwiki/Import_modules_properly>.

    -- ** Defining new monad transformer
    -- $definingMonadTransformer

    -- * Library core
    --
    -- | Basic library interface.  Main idea behind it is to provide very
    -- stable API that can be imported directly from
    -- "Control.Monad.ExceptionWitness.Core" module or as part of this one.
    --
    -- Among others it provides:
    --
    -- * 'Throws' newtype that is used for tagging monadic code with exception
    --   type.
    --
    -- * A lot of combinators for tagged monadic code. In example \"@'liftT' ::
    --   ('Exception' e, 'MonadException' m) => m a -> 'Throws' e m a@\" lifts
    --   monadic code in to tagged monadic code.
    --
    -- * 'MonadException' type class describes basic operations for raising and
    --   catching exceptions.
    --
    -- * Functions defined on top of 'MonadException' methods ('throw', 'catch'
    --   and 'catch''). In example \"@'handle' :: ('Exception' e,
    --   'MonadException' m) => (e -> m a) -> 'Throws' e m a -> m a@\" which is
    --   just flipped version of 'catch'.
      module Control.Monad.TaggedException.Core

    -- * Hidable exceptions
    --
    -- | Support for hidden/uncaught exceptions.  The ideas behind hiding
    -- thrown exception is:
    --
    -- 1. Be compatible with /extensible-exceptions/ (@Control.Exception@),
    --    in sense that all current @IO@ code doesn't reflect raised
    --    exceptions in it's type.  All standard exceptions, exported by
    --    @Control.Exception@ module, are instances of 'HidableException'.
    --
    -- 2. Programs, and their code, are multilayered things.  Sometimes
    --    exceptions aren't ment to be caught in certain layers.  See also
    --    /Error vs. Exception/ on /Haskell Wiki/
    --    (<http://www.haskell.org/haskellwiki/Error_vs._Exception>).
    , module Control.Monad.TaggedException.Hidden

    -- * Asynchronous exceptions and bracket family of functions
    , module Control.Monad.TaggedException.Utilities

    -- * Some related work
    --
    -- | There is already more then one package that introduces similar
    -- interfaces and also many others that are dealing with the same problem
    -- domain.  Just to list some:
    --
    -- * /control-monad-attempt/
    --   (<http://hackage.haskell.org/package/control-monad-attempt>).
    --
    -- * /control-monad-exception/
    --   (<http://hackage.haskell.org/package/control-monad-exception>):
    --   Exception monad transformer with explicitly typed exceptions.
    --
    -- * /explicit-exception/
    --   (<http://hackage.haskell.org/package/explicit-exception>): Synchronous
    --   and Asynchronous exceptions which are explicit in the type signature.
    --
    -- * /failure/ (<http://hackage.haskell.org/package/failure>) with
    --   instances for /transformers/
    --   (<http://hackage.haskell.org/package/transformers>) defined in
    --   /control-monad-failure/
    --   (<http://hackage.haskell.org/package/control-monad-failure>).
    --
    -- * /MonadCatchIO-mtl/
    --   (<http://hackage.haskell.org/package/MonadCatchIO-mtl>) and
    --   /MonadCatchIO-transformers/
    --   (<http://hackage.haskell.org/package/MonadCatchIO-transformers>): This
    --   libraries export @class MonadIO m => MonadCatchIO m@ that catches
    --   lifting neccessary for exception handling in encapsulated @IO@ monad.
    --
    -- * /monad-control/
    --   (<http://hackage.haskell.org/package/monad-control>):
    --   Based on /monad-peel/.
    --
    -- * /monad-peel/
    --   (<http://hackage.haskell.org/package/monad-peel>)

    -- * Reexported from @Control.Exception@ module
    , E.Exception(..)
        -- Exception class
    , E.SomeException(..)
        -- Root exception
    , E.ArithException(..)
    , E.ArrayException(..)
    , E.AssertionFailed(..)
    , E.AsyncException(..)
#if MIN_VERSION_base(4,2,0)
    , E.BlockedIndefinitelyOnMVar(..)
    , E.BlockedIndefinitelyOnSTM(..)
#else
    , E.BlockedIndefinitely(..)
    , E.BlockedOnDeadMVar(..)
#endif
    , E.Deadlock(..)
    , E.ErrorCall(..)
    , E.IOException
    , E.NestedAtomically(..)
    , E.NoMethodError(..)
    , E.NonTermination(..)
    , E.PatternMatchFail(..)
    , E.RecConError(..)
    , E.RecSelError(..)
    , E.RecUpdError(..)
    )
    where

import Control.Exception as E
    ( ArithException(..)
    , ArrayException(..)
    , AssertionFailed(..)
    , AsyncException(..)
#if MIN_VERSION_base(4,2,0)
    , BlockedIndefinitelyOnMVar(..)
    , BlockedIndefinitelyOnSTM(..)
#else
    , BlockedIndefinitely(..)
    , BlockedOnDeadMVar(..)
#endif
    , Deadlock(..)
    , ErrorCall(..)
    , Exception(..)
    , IOException
    , NestedAtomically(..)
    , NoMethodError(..)
    , NonTermination(..)
    , PatternMatchFail(..)
    , RecConError(..)
    , RecSelError(..)
    , RecUpdError(..)
    , SomeException(..)
    )

import Control.Monad.TaggedException.Core
import Control.Monad.TaggedException.Hidden

-- $usage
--
-- Example of reflecting reised exception in type:
--
-- > {-# LANGUAGE DeriveDataTypeable #-}
-- >
-- > import Control.Monad.TaggedException (Exception, Throws)
-- > import qualified Control.Monad.TaggedException as E (liftT, throw)
-- > import Data.Typeable (Typeable)
-- >
-- >
-- > data NotReady = NotReady String
-- >     deriving (Show, Typeable)
-- >         -- Both required by Exception class
-- >
-- > instance Exception NotReady
-- >
-- > myFunction :: Input -> Throws NotReady IO Output
-- > myFunction input = do
-- >
-- >     ... some stuff ...
-- >
-- >     -- isReady :: Input -> IO Bool
-- >     ready <- E.liftT $ isReady input
-- >     unless ready
-- >         . E.throw $ NotReady "Resource of myFunction is not ready."
-- >
-- >     ... some other stuff ...
--
-- Caller of this function is forced to catch/handle this exception or reflect
-- it in it's type too.
--
-- See "Control.Monad.TaggedException.Core" and
-- "Control.Monad.TaggedException.Hidden" for more examples.

-- $definingMonadTransformer
--
-- When creating new monad transformer that will be used in /tagged-exception/
-- context, and won't have 'MonadException' and 'MonadExceptionUtilities'
-- instances, then provide atleast @liftThrow@, @liftCatch@, and @liftMask@
-- functions.
--
-- Lets say we are defining something like:
--
-- > newtype T x m a = ...
--
-- then function for lifting 'throw' will have type:
--
-- > liftThrow
-- >     :: (e -> Throws e m a)
-- >     -> e -> Throws e (T x m) a
--
-- or define @instance MonadTrans (T x)@ and then
-- 'Control.Monad.TaggedException.Unsafe.liftThrow' can be used instead.
--
-- Function @liftCatch@ for lifting 'catch'' (can be also used for @ErrorT@'s
-- @catchError@) will have type:
--
-- > liftCatch
-- >     :: (m a -> (e -> m a) -> m a)
-- >     -> T x m a -> (e -> T x m a) -> T x m a
--
-- And type of lifting function for masking asynchronous exception will be
-- something like:
--
-- > liftMask
-- >     :: (forall b. ((forall a. m a -> m a) -> m b) -> m b)
-- >     -> ((forall c. T x m c -> T x m c) -> T x m d) -> T x m d
--
-- Implementation of @liftMask@ for @IdentityT@, @WriterT@ and @StateT@ looks
-- like:
--
-- > liftMask msk f = IdentityT . msk $ \ restore ->
-- >     runIdentityT $ f (IdentityT . restore . runIdentityT)
-- >
-- > liftMask msk f = WriterT . msk $ \ restore ->
-- >     runWriterT $ f (WriterT . restore . runWriterT)
-- >
-- > liftMask msk f = StateT $ \ s -> msk $ \ restore ->
-- >     (`runStateT` s) $ f (\ x -> StateT $ restore . runStateT x)
--
-- If @liftFinally'@, @liftFinally@, @liftBracket'@, and @liftBracket@ can be
-- provided without violating axioms, then these should be provided as well.
-- This functions will have types similar to:
--
-- > liftFinally'
-- >     :: (forall a b. m a -> m b -> m a)
-- >     -> T x m a' -> T x m b' -> T x m a'
-- >
-- > liftFinally
-- >     :: (forall a b. Throws e m a -> m b -> Throws e m a)
-- >     -> Throws e (T x m) a' -> m b' -> Throws e (T x m) a'
-- >
-- > liftBracket'
-- >     :: (forall a b c. m a -> (a -> m b) -> (a -> m c) -> m c)
-- >     -> T x m a' -> (a' -> T x m b') -> (a' -> T x m c') -> T x m c'
-- >
-- > liftBracket
-- >     => (forall a b c.
-- >         m a -> (a -> m b) -> (a -> Throws e m c) -> Throws e m c)
-- >     -> T x m a'
-- >     -> (a' -> T x m b')
-- >     -> (a' -> Throws e (T x m) c')
-- >     -> Throws e (T x m) c'
