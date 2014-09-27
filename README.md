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
  *extensible-exceptions*.  `Throws` monad transformer that uses phantom type
  to tag code that may raise exception.  Intention is to make exceptions
  explicit and to enforce exception handling.
* *tagged-exception-instances-comonad* (directory `instances-comonad/`) --
  Comonad instances for *tagged-excpeption-core* package.
* *tagged-exception-lifting* (directory `lifting/`) -- Commonly used lifting
  operations mostly from different kinds of error handling in to API provided
  by *tagged-exception-core* package.
* *tagged-exception-transformer* (directory `transformer/`) -- Monad
  transformer based on API provided by *tagged-excpeption-core* package.
