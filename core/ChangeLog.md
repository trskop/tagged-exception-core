# ChangeLog / ReleaseNotes


## Version 2.1.0.0

* Bumping transformers bounds to include 0.4.\* versions.
* Dropped last threads of support for base <= 4.5; it hadn't worked anyway
  since [exceptions][] ==0.6 depend on base >=4.5 && <5.
* Instances for [mtl][] >=2.1 package. Package [exceptions][] already depends
  on [mtl][], so it doesn't make sense to shy away from them.
* Introduced unsafe function:

    ````Haskell
    liftCCLike
        :: (((a -> m b) -> m' c) -> m'' d)
        -> ((a -> Throws e m b) -> Throws e m' c) -> Throws e m'' d
    ```

* Documentation updates.


## Version 2.0.0.0

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
