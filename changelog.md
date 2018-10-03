# Changelog for [`multistate` package](https://hackage.haskell.org/package/multistate)

## 0.8.0.1 *October 2018*

  * Adapt for ghc-8.6 (really, this time)
  * Make package -Wcompat-ible

## 0.8.0.0 *April 2018*

  * Adapt for ghc-8.4
  * Drop support for ghc<8.0
  * Add class `MonadMultiGet` that roughly translates to "any read access"
    (instances for Reader and State)
  * Add data-type `MultiGST` that has a single taggified HList instead of the
    three r, w, s lists with `MultiRWS`

## 0.7.1.2 *August 2017*

  * Adapt for ghc-8.2
  
  * Minor strictness fix for MultiRWS

## 0.7.1.1 *May 2016*

  * Adapt for ghc-8

## 0.7.1.0 *March 2016*

  * Add new method `withoutMultiFoo`, inverse of `withMultiFoo`

## 0.7.0.0 *February 2016*

  * Add instances:

    + MonadIO
    + Alternative
    + MonadPlus
    + MonadBase
    + MonadTransControl
    + MonadBaseControl

## 0.6.2.0 *June 2015*

  * Add MonadFix instances

## 0.6.1.0 *June 2015*

  * Export classes from transformer modules

## 0.6.0.0 *June 2015*

  * Add `MultiRWST`

  * Add inflate functions (e.g. `StateT _ -> MultiStateT _`)

  * Improve lazyness

  * Move changelog from `README.md` to `changelog.md`

## 0.5.0.0 *March 2015*
    
  * Breaking changes (!):

    Refactor some parts of the interface, see "naming scheme" in  the README;
    The changes are:

      | old | new |
      | --- | --- |
      | `withMultiFoo` | `withMultiFooA` |
      | `withMultiFoos` | `withMultiFoosA` |
      | `mAskRaw` | `mGetRaw` |
      | | `mPutRaw` |
      | `evalMultiStateT` | `runMultiStateTNil` |
      | `evalMultiStateTWithInitial` | `runMultiStateTA` |
      | `evalMultiReaderT` | `runMultiReaderTNil` |
      | `evalMultiReaderTWithInitial` | `runMultiReaderTA` |
      | `execMultiWriterT` | `runMultiWriterTW` |

  * Start using hspec; Add proper cabal test-suite.

## 0.4.0.0: *March 2015*

  * Refactor from `Control.Monad.*` to `Control.Monad.Trans.*`

  * Put classes (`MonadMulti*`) into separate modules

  * Add Strict and Lazy variants

  * Deprecate previous modules

## 0.3.0.0 *January 2015*

  * Add `MultiWriter`

  * Fixity for `(:+:)`

  * support ghc-7.10

## 0.2.0.0 *January 2015*

  * Start using DataKinds and TypeOperators to make the HList
    representation more readable. The translation roughly is:

    > ~~~~
    > Null        -> '[]
    > Cons a Null -> '[a]
    > Cons a b    -> a ': b
    > TNull       -> HNil
    > TCons a b   -> a :+: b
    > ~~~~

  * Remove dependency on `tfp` package.

## 0.1.3.2 *September 2014*
  
  * Add example

  * Clean up / Add dependencies

  * More documentation

## 0.1.2 *September 2014*

  * Expose `HList` module

  * Add haddocks

## 0.1.1 *June 2014*

  * First version published on hackage
