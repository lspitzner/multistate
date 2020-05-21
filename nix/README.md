
This nix setup expects the iohk haskell-nix overlay to be available/included
when importing `<nixpkgs>`. Also, you might need a specific commit if you
want to test against all supported ghcs (8.4 - 8.10, currently).

# Useful commands:

~~~~.sh
# enter a shell for a specific build-plan
# (cabal-solved with ghc-8.4 in this case)
nix-shell nix/all.nix -A '"cabal-8.4".shell'
# run all tests an show summary lines of test output
find -L $(nix-build ./nix/all.nix -A test-all -o nix-output-tests) -mindepth 1 | sort -n | xargs -I{} bash -c "echo {}; grep examples {}"
~~~~


# Files in this directory:

all.nix       - main entrypoint into this package's nix world
via-cabal.nix - how to build this via cabal-solved package-set
via-stack.nix - how to build via stackage-based package set
                (the name is a lie, we are not using stack, just stackage)

(plus some currently unused:)

materialized  - materializations of cabal-solved build-plans
plan.nix      - manual materialization of unsolved build-plan (used with
                stackage snapshot to build package set)
