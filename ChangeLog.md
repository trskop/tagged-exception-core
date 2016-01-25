# ChangeLog / ReleaseNotes


## Version 2.2.0.0

* Deprecating module `Control.Monad.TaggedException.Utilities`, definitions
  from it were moved in to `Control.Monad.TaggedException.Core`. (**change**)
* Type `Throws` has now instance for `Generic1`, but only on GHC >=7.10, due to
  `PolyKinds`. (**new**)
* instance HiddenException AllocationLimitExceeded
* `HiddenException` instances for `AllocationLimitExceeded` and `Void`, both
  defined in base >=4.8. (**new**)
* `HiddenException` instance for `TypeError`, defined in base >=4.9. (**new**)
* Bumped upper bound of [transformers][] package to include 0.5.\* branch.
  (**change**)


## Version 2.1.0.1

* Relaxing upper bound on [exceptions][] package to include versions up to
  0.8.\*. (**new**)
* Not uploaded on to [Hackage][], instead opted to modify package metadata of
  [2.1.0.0](http://hackage.haskell.org/package/tagged-exception-core-2.1.0.0)
  release.


## Version 2.1.0.0

* Builds on GHC from 7.4 to 7.10, later with base 4.8. (**change**)
* Bumping [transformers][] bounds to include 0.4.\* versions. (**change**)
* Dropped last threads of support for base <= 4.5; it hadn't worked anyway
  since [exceptions][] ==0.6 depend on base >=4.5 && <5. (breaking change)
* Instances for [mtl][] >=2.1 package. Package [exceptions][] already depends
  on [mtl][], so it doesn't make sense to shy away from them. (**new**)
* Introduced unsafe functions (**new**):

    ```Haskell
    liftCCLike
        :: (((a -> m b) -> m' c) -> m'' d)
        -> ((a -> Throws e m b) -> Throws e m' c) -> Throws e m'' d

    liftEmbedLike
        :: (forall a. m a -> Throws e n a)
        -> Throws e' m b -> Throws e n b

    liftHoistLike
        :: (forall a. m a -> n a)
        -> Throws e m b -> Throws e' n b
    ```

* Documentation updates. (**new**)
* Uploaded to [Hackage][]:
  <http://hackage.haskell.org/package/tagged-exception-core-2.1.0.0>


## Version 2.0.0.0

* First public release.
* Rewritten to use classes from [exceptions][] package.
* Uploaded to [Hackage][]:
  <http://hackage.haskell.org/package/tagged-exception-core-2.0.0.0>



[exceptions]:
  http://hackage.haskell.org/package/exceptions
  "exceptions package on Hackage"
[Hackage]:
  http://hackage.haskell.org/
  "HackageDB (or just Hackage) is a collection of releases of Haskell packages."
[mtl]:
  http://hackage.haskell.org/package/mtl
  "mtl package on Hackage"
[transformers]:
  http://hackage.haskell.org/package/transformers
  "transformers package on Hackage"
