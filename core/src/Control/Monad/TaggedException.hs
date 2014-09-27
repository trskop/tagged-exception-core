{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
-- |
-- Module:       $HEADER$
-- Copyright:    (c) 2009 - 2014 Peter Trsko
-- License:      BSD3
--
-- Stability:    provisional
-- Portability:  non-portable (CPP, NoImplicitPrelude, depends on non-portable
--               modules)
module Control.Monad.TaggedException
    (
    -- * Introduction
    --
    -- | This library provides interface that is similar to base's /Extensible
    -- Exceptions/. It introduces 'Throws' monad transformer that uses phantom
    -- type to tag code that may raise exception.  Intention is to make
    -- exceptions explicit and to enforce exception handling.
    --
    -- This approach is based on commonly used techniques:
    --
    -- * /Phantom Types/ <http://www.haskell.org/haskellwiki/Phantom_type>
    --
    -- * /Type Witnesses/ <http://www.haskell.org/haskellwiki/Type_witness>

    -- ** Why use this?
    --
    -- | Exceptions are one of the fastest and most scalable ways of handling
    -- failures and errors. One of the downsides of exceptions as defined in
    -- Haskell is that they aren't visible in type signatures as in example
    -- when using @Maybe@ or @ErrorT@.
    --
    -- This library tries to get rid of this issue by making exceptions
    -- visible. On the other hand it makes things little more complicated, but
    -- fortunatelly not too much.
    --
    -- Some of the benefits of this approach are listed bellow.

    -- *** Unification of exception handling

    -- | Raising and handling exception becomes the same for all
    -- 'Control.Monad.Catch.MonadThrow' and 'Control.Monad.Catch.MonadCatch'
    -- instances. This includes code that uses exceptions in @IO@ monad and
    -- @ErrorT@ style error handling. All that can be easily modified to use
    -- API defined by this library.
    --
    -- For ilustration there is a great summary of various ways of error
    -- handling in Haskell:
    --
    -- * <http://blog.ezyang.com/2011/08/8-ways-to-report-errors-in-haskell-revisited/ 8 ways to report errors in Haskell revisited>
    --
    -- * <http://www.randomhacks.net/articles/2007/03/10/haskell-8-ways-to-report-errors/ 8 ways to report errors in Haskell>
    --
    -- Posts mentioned above show that any unification or framework for
    -- transforming one error handling technique to another are very benefitial
    -- in practice.

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
    -- >     :: Monad m
    -- >     => Container Key Value
    -- >     -> Key
    -- >     -> m Value
    --
    -- this library allows to write:
    --
    -- > lookup
    -- >     :: MonadThrow m
    -- >     => Container Key Value
    -- >     -> Key
    -- >     -> Throw LookupFailure m Value
    --
    -- where @LookupFailure@ is instance of 'Exception' class. While in some
    -- ways it's similar to using @ErrorT@, it has all the flexibility of
    -- /extensible-exceptions/ for arbitrary 'MonadThrow' instance.

    -- ** Dependencies
    --
    -- | This package is trying to keep dependencies at minimum.  Here is list
    -- of current dependencies:
    --
    -- * /base/
    --
    -- * /exceptions/: Provides 'Control.Monad.Catch.MonadThrow',
    --   'Control.Monad.Catch.MonadCatch' and 'Control.Monad.Catch.MonadMask'
    --   type classes.
    --
    -- * /extensible-exceptions/ for /4 >= base < 4.2/
    --
    -- * /transformers >= 0.2 && < 0.4/: De facto current standard for monad
    --   transformers.  Included in newer versions of HaskellPlatform.
    --
    -- * /mmorph/ >= 1.0.0 && < 1.1: Monad morphism utilities.  Currently not
    --   in HaskellPlatform.

    -- ** Naming conventions
    --
    -- | Names of basic functions are the same as those in "Control.Exception"
    -- module, but differ in it's type signature.  They operate on tagged code
    -- and are therefore limited to operate only on exceptions specified by the
    -- phantom type.
    --
    -- Exception, to above rule, is 'throw' function which does not throw
    -- exception from pure code, as does 'Control.Exception.throw', but from
    -- monadic code.  So, it is more equivalent to 'Control.Exception.throwIO'.

    -- *** \<function\> vs. \<function\>'
    --
    -- | Functions with prime at the end of there name aren't restricted by the
    -- phantom type while those without it are.  Functions with prime can
    -- therefore operate on arbitrary exceptions.  Use such functions when
    -- operating on exceptions that are different from exception specified by a
    -- phantom type, i.e. hidden ones.
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
    -- > import Control.Monad.TaggedException
    --
    -- or import:
    --
    -- > import Control.Monad.TaggedException as E
    --
    -- and then use @E.catch@, in later case you can also use qualified import:
    --
    -- > import qualified Control.Monad.TaggedException as E
    --
    -- It is recomended to use explicit import list or, as mentioned before,
    -- qualified import. See also /Import modules properly/ on /Haskell Wiki/:
    -- <http://www.haskell.org/haskellwiki/Import_modules_properly>.

    -- * API documentation

    -- ** Library core
    --
    -- | Basic library interface.  Main idea behind it is to provide very
    -- stable API that can be imported directly from
    -- "Control.Monad.TaggedException.Core" module or as part of this one.
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
    -- * Functions defined on top of 'MonadThrow' and 'MonadCatch' methods
    --   ('throw', 'catch' and 'catch''). In example \"@'handle' ::
    --   ('Exception' e, 'MonadException' m) => (e -> m a) -> 'Throws' e m a
    --   -> m a@\" which is just flipped version of 'catch'.
      module Control.Monad.TaggedException.Core

    -- ** Hidable exceptions
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
    --
    -- See "Control.Monad.TaggedException.Hidden" for examples.
    , module Control.Monad.TaggedException.Hidden

    -- ** Asynchronous exceptions and bracket family of functions
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
    , Exception(..)
        -- Exception class
    , SomeException(..)
        -- Root exception
    )
  where

import Control.Exception
    ( Exception(..)
    , SomeException(..)
    )

import Control.Monad.TaggedException.Core
import Control.Monad.TaggedException.Hidden
import Control.Monad.TaggedException.Utilities

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
