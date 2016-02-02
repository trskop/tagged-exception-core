# Tagged Exception Core

[![Hackage](http://img.shields.io/hackage/v/tagged-exception-core.svg)][Hackage: tagged-exception-core]
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/tagged-exception-core.svg)](http://packdeps.haskellers.com/reverse/tagged-exception-core)
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]

[![Build](https://travis-ci.org/trskop/tagged-exception-core.svg)](https://travis-ci.org/trskop/tagged-exception-core)


## Description

Reflect exceptions using phantom types. This library provides core API and
others may build on top of it.


## Usage Example

Example of reflecting reised exception in type:

```Haskell
{-# LANGUAGE DeriveDataTypeable #-}

import Control.Exception (Exception)

import Control.Monad.TaggedException (Throws)
import qualified Control.Monad.TaggedException as E (liftT, throw)
import Data.Typeable (Typeable)


data NotReady = NotReady String
    deriving (Show, Typeable)
        -- Both required by Exception class

instance Exception NotReady

myFunction :: Input -> Throws NotReady IO Output
myFunction input = do

    -- ... some stuff ...

    -- isReady :: Input -> IO Bool
    ready <- E.liftT $ isReady input
    unless ready
        . E.throw $ NotReady "Resource of myFunction is not ready."

    -- ... some other stuff ...
```

## License

The BSD 3-Clause License, see [LICENSE][] file for details.


## Contributions

Contributions, pull requests and bug reports are welcome! Please don't be
afraid to contact author using GitHub or by e-mail.



[Hackage: tagged-exception-core]:
  http://hackage.haskell.org/package/tagged-exception-core
  "tagged-exception-core package on Hackage"
[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[LICENSE]:
  https://github.com/trskop/tagged-exception-core/blob/master/LICENSE
  "License of endo package."
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"
