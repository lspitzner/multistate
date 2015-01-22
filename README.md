# Introduction

When using multiple Reader/Writer/State transformers in the same monad
stack, it becomes necessary to lift the operations in order to affect a
specific transformer.
Using heterogenous lists (and all kinds of GHC extensions magic),
this package provides transformers that remove that necessity:
MultiReaderT/MultiWriterT/MultiStateT can contain a heterogenous
list of values.

The type inferred for the getter/setter determines which value is
read/written.

# Example

~~~~
simpleExample :: IO ()
simpleExample = evalMultiStateT          -- start with an empty state,
                                         --   i.e. :: MultiStateT '[] IO
              $ withMultiState 'H'       -- "adding" a char to the state
              $ withMultiState "ello, World!" -- and a string
              $ do                       -- so:
  -- the monad here is MultiStateT '[String, Char] IO
  let combinedPrint = do       -- no type signature necessary
        c  <- mGet             -- type of mGet inferred to be m Char
        cs <- mGet             --              inferred to be m String
        lift $ putStrLn (c:cs)
  combinedPrint
  mSet 'J'                     -- we modify the Char in the state.
                               -- again, the type is inferred,
                               -- without any manual lifting.
  combinedPrint
~~~~

The output is:

~~~~
Hello, World!
Jello, World!
~~~~

( you can find both this and a more complex example
  in an executable in the package. )

# Error Messages

If you try to execute an action that requires a specific type in the state,
but the current state does not contain that type, the error message is
something like

~~~~
No instance for (Control.Monad.MultiState.ContainsType Foo '[]) x
~~~~

where `Foo` is the missing type.

# Known Deficits

This package currently lacks a complete set of "lifting instances", i.e.
instance definitions for classes such as mtl's MonadWriter "over" the newly
introduced monad transformers, as in

~~~~
instance (MonadWriter w m) => MonadWriter w (MultiStateT c m) where ..
~~~~

These "lifting instances" would be necessary
to achieve full compatability with existing transformers. Ping me if you
find anything specific missing.

# Changelog

* 0.2.0.0:

    * Start using DataKinds and TypeOperators to make the HList
      representation more readable. The translation roughly is:

        ~~~~
        Null        -> '[]
        Cons a Null -> '[a]
        Cons a b    -> a ': b
        TNull       -> HNil
        TCons a b   -> a :+: b
        ~~~~

    * Remove dependency on `tfp` package.

* 0.3.0.0:
    
    * Add MultiWriter
    * support ghc-7.10
