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

# Naming Scheme

(Will refer to StateT in this paragraph, but equally valid for Reader/Writer)
The mtl monad transformers make use of primarily three methods to "unwrap"
a transformed value:
`runStateT`, `evalStateT`, `execStateT`. These three all have a type
matching the pattern `s -> t m a -> m b`, they differ in what `b` is.
We will use a different naming scheme, for three reaons:

1) "run", "eval" and "exec" are not in any way intuitive, and should be
   suffixed in any case.
2) For MultiStateT, it makes sense to transform an existing transformer,
   adding another state. The signature would be close to that of runStateT,
   only without the unwrapping part, i.e. `s -> t m a -> t' m b`, where `s`
   is the initial state, and `t` is `t'` with another state added.
3) Sometimes you might want to add/run a single state, or a bunch of them.
   For example, when running an arbitrary StateT, you would need to provide
   a HList of initial states, and would receive a HList of final states.

Our naming scheme will instead be:

1) `runStateT.*` unwraps a StateT. A suffix controls
   what exactly is returned by the function. There is a special version for
   when the list of states is Nil, `runStateTNil`.
2) `withStateT.*` adds one or more states to a subcomputation. A suffix
   controlls the exact return value.

~~~~
                 withStates
            /-------------------------------------------------------\
            |     withState                withState .. withState   v
StateT '[s, ..] m --------> StateT '[..] m --------> .. --------> StateT '[] m
            |                                                       |
            |                                                       |
            |   runStateT                            runStateTNil   |
            \--------------------> m .. <---------------------------/
~~~~

Specific functions are

~~~~
runMultiStateT = runStateTAS
runMultiStateTA  :: HList s -> MultiStateT s m a -> m a
runMultiStateTAS :: HList s -> MultiStateT s m a -> m (a, s)
runMultiStateTSA :: HList s -> MultiStateT s m a -> m (s, a)
runMultiStateTS  :: HList s -> MultiStateT s m a -> m s
runMultiStateT_  :: HList s -> MultiStateT s m a -> m ()

runMultiStateTNil  :: MultiStateT '[] m a -> m a
runMultiStateTNil_ :: MultiStateT '[] m a -> m ()

withMultiState = withStateAS
withMultiStateA  :: s -> MultiStateT (s ': ss) m a -> MultiStateT ss m a
withMultiStateAS :: s -> MultiStateT (s ': ss) m a -> MultiStateT ss m (a, s)
withMultiStateSA :: s -> MultiStateT (s ': ss) m a -> MultiStateT ss m (s, a)
withMultiStateS  :: s -> MultiStateT (s ': ss) m a -> MultiStateT ss m s
withMultiState_  :: s -> MultiStateT (s ': ss) m a -> MultiStateT ss m ()

withMultiStates = withStatesAS
withMultiStatesAS :: HList s1 -> MultiStateT (Append s1 s2) m a -> MultiStateT s2 m (a, HList s1)
withMultiStatesSA :: HList s1 -> MultiStateT (Append s1 s2) m a -> MultiStateT s2 m (HList s1, a)
withMultiStatesA  :: HList s1 -> MultiStateT (Append s1 s2) m a -> MultiStateT s2 m a
withMultiStatesS  :: HList s1 -> MultiStateT (Append s1 s2) m a -> MultiStateT s2 m (HList s1)
withMultiStates_  :: HList s1 -> MultiStateT (Append s1 s2) m a -> MultiStateT s2 m ()
~~~~

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

See changelog.md
