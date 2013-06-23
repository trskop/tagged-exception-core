{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module:       $HEADER$
-- Description:  UserException data type
-- Copyright:    (c) 2009 - 2011, 2013 Peter Trsko
-- License:      BSD3
--
-- Stability:    provisional
-- Portability:  non-portable (CPP, DeriveDataTypeable)
--
-- 'UserException' has instances for 'Exception', and 'HidableException'
-- classes.   Useful for implementation of @'Monad'('fail')@, and is
-- done so in "Control.Monad.ExceptionT".
module Control.Monad.TaggedException.UserException
    (
    -- * UserException
      UserException(..)
    , emptyUserException
    , mapUserException
    , mapUserException2
    )
    where

import Control.Exception (Exception)
import Data.Data (Data)
import Data.String (IsString(fromString))
import Data.Typeable (Typeable)

#ifdef VERSION_data_default
import Data.Default (Default(def))
#elif defined VERSION_data_default_class
import Data.Default.Class (Default(def))
#endif
import Data.Monoid (Monoid(mempty, mappend))
#ifdef VERSION_semigroups
#if MIN_VERSION_semigroups(0,5,0)
import Data.List.NonEmpty (toList)
#endif
import Data.Semigroup (Semigroup(..))
#endif
import Control.Monad.Trans.Error (Error(noMsg, strMsg))

import Control.Monad.TaggedException.Hidden (HidableException)


-- | This exception type can be used for implementation of @'Monad'('fail')@.
--
-- Notes on instances:
--
-- * 'Default'('def') is defined as @'def' = 'emptyUserException'@.
--
-- * Show instance returns text "User exception" followed by ": " and message
--   if not empty.
--
-- * 'IsString'('fromString') is defined as @'fromString' = 'UserException'@.
--
-- * 'Monoid'('mempty') is defined as @'mempty' = 'emptyUserException'@ and
--   'Monoid'('mappend') as @'mappend' = 'mapUserException2' ('++')@.
--
-- * If compiled with
--   /semigroups/ <http://hackage.haskell.org/package/semigroups> then
--   'Semigroup'(('<>')) is defined the same way as 'Monoid'('mappend').
--
-- Examples:
--
-- > > show emptyUserException
-- > "User exception"
-- > > show (UserException "foo")
-- > "User exception: foo"
-- > > show (fromString "foo" :: UserException)
-- > "User exception: foo"
-- > > show $ emptyUserException `mappend` UserException "foo"
-- > "User exception: foo"
-- > > show $ UserException "foo" `mappend` UserException "bar"
-- > "User exception: foobar"
newtype UserException = UserException String
    deriving (Data, Eq, Ord, Typeable)

-- | @emptyUserException = UserException \"\"@
emptyUserException :: UserException
emptyUserException = UserException ""

-- | Modify message contained in 'UserException'.
mapUserException :: (String -> String) -> UserException -> UserException
mapUserException f (UserException s) = UserException $ f s

-- | Combine two 'UserException's.
--
-- Used to implement Monoid('mappend') and Semigroup(('<>')) as:
-- @mapUserException2 ('++')@
mapUserException2
    :: (String -> String -> String)
    -> UserException
    -> UserException
    -> UserException
mapUserException2 f (UserException s1) (UserException s2)
    = UserException $ s1 `f` s2

instance Show UserException where
    showsPrec _ e = (showString "User exception" .) $ case e of
        UserException ""  -> id
        UserException msg -> showString ": " . showString msg

instance Default UserException where
    def = emptyUserException

instance Error UserException where
    noMsg = emptyUserException
    strMsg = UserException

instance Exception UserException

instance HidableException UserException

instance IsString UserException where
    fromString = UserException

instance Monoid UserException where
    mempty = emptyUserException
    mappend = mapUserException2 (++)

#ifdef VERSION_semigroups
instance Semigroup UserException where
    (<>) = mapUserException2 (++)
#if MIN_VERSION_semigroups(0,5,0)
    sconcat = foldr1 (mapUserException2 (++)) . toList
#endif
    -- MIN_VERSION_semigroups(0,5,0)
#if MIN_VERSION_semigroups(0,8,0)
    times1p n (UserException s) =
        UserException . concat $ replicate (1 + fromIntegral n) s
#elif MIN_VERSION_semigroups(0,7,0)
    replicate1p n (UserException s) =
        UserException . concat $ replicate (1 + fromIntegral n) s
#endif
    -- MIN_VERSION_semigroups(0,8,0) elif MIN_VERSION_semigroups(0,7,0)
#endif
    -- VERSION_semigroups
