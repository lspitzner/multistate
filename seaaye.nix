{ seaaye-spec = 1;
  package-name = "multistate";
  targets =
  {
    hackage-8-06 = {
      resolver = "hackage";
      index-state = "2021-07-01T00:00:00Z";
      ghc-ver = "ghc865";
    };
    hackage-8-08 = {
      resolver = "hackage";
      index-state = "2021-07-01T00:00:00Z";
      ghc-ver = "ghc884";
    };
    hackage-8-10 = {
      resolver = "hackage";
      index-state = "2021-07-01T00:00:00Z";
      ghc-ver = "ghc8107";
    };
    hackage-9-01 = {
      resolver = "hackage";
      index-state = "2021-07-01T00:00:00Z";
      ghc-ver = "ghc901";
    };
    stackage-8-06 = {
      resolver = "stackage";
      stackFile = "stack-8.6.yaml";
      ghc-ver = "ghc865";
    };
    stackage-8-08 = {
      resolver = "stackage";
      stackFile = "stack-8.8.yaml";
      ghc-ver = "ghc884";
    };
    stackage-8-10 = {
      resolver = "stackage";
      stackFile = "stack-8.10.yaml";
      ghc-ver = "ghc8107";
    };
    stackage-9-01 = {
      resolver = "stackage";
      stackFile = "stack-9.0.yaml";
      ghc-ver = "ghc901";
    };
  };
  module-flags = [
    # N.B.: There are haskell-nix module options. See the haskell-nix docs
    #       for details. Also, be careful about typos: In many cases you
    #       will not get errors but the typo'd flag will just not have any
    #       effect!
    # { packages.my-package.flags.my-package-examples-examples = true; }
  ];
  default-target = "hackage-8-06";
  do-check-hackage = "hackage.haskell.org";
  do-check-changelog = "changelog.md";
  # local-config-path = ./seaaye-local.nix;
}