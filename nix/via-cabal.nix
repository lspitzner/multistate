{ pkgs ? import <nixpkgs.nix>
, pkg-def-extras ? []
, ghc-ver
, index-state
, plan-sha256 ? null
, materialized ? null
}:
let
  multistate-plan = pkgs.haskell-nix.importAndFilterProject (pkgs.haskell-nix.callCabalProjectToNix {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./..; name = "multistate-" + ghc-ver; };
    inherit index-state plan-sha256 materialized;
    # ghc = pkgs.haskell-nix.compiler.${ghc-ver};
    compiler-nix-name = ghc-ver;
  });
in rec {
  inherit multistate-plan pkgs;

  hsPkgs = 
    let
    in let pkg-set = pkgs.haskell-nix.mkCabalProjectPkgSet
              { plan-pkgs = multistate-plan.pkgs;
                pkg-def-extras = pkg-def-extras;
                modules = [ 
                  { ghc.package = pkgs.haskell-nix.compiler.${ghc-ver}; }
                ];
              };
    in pkg-set.config.hsPkgs;

  multistate = hsPkgs.multistate;
  tests = { inherit (hsPkgs.multistate.checks) multistate-test cabal-check; };
  shell = hsPkgs.shellFor {
    # Include only the *local* packages of your project.
    packages = ps: with ps; [
      multistate
    ];

    # Builds a Hoogle documentation index of all dependencies,
    # and provides a "hoogle" command to search the index.
    withHoogle = false;

    # You might want some extra tools in the shell (optional).

    # Some common tools can be added with the `tools` argument
    # tools = { cabal = "3.2.0.0"; };
    # See overlays/tools.nix for more details

    # Some you may need to get some other way.
    buildInputs = with pkgs.haskellPackages;
      [ cabal-install ghcid bash pkgs.nix ];

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = true;
  };
}
