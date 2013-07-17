Tagged Exceptions
=================

Description
-----------

Set of Haskell libraries that are centered around the idea of using phantom
types to tag code that may raise exception(s).  Intention is to make exceptions
explicit and to enforce exception handling.


Packages
--------

* *tagged-exception-core* (directory `core/`) -- Provides interface similar to
  *extensible-exceptions*.  It introduces `MonadException` class, which uses
  phantom types to tag code that may raise exception.
* *tagged-exception-instances-comonad* (directory `instances-comonad/`) --
  Comonad instances for *tagged-excpeption-core* package.
* *tagged-exception-transformer* (directory `transformer/`) -- Monad
  transformer based on API provided by *tagged-excpeption-core* package.
