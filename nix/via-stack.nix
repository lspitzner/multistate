{ pkgs ? import <nixpkgs.nix>
, pkg-def-extras ? []
, resolver
}:
let
  # package-desc = import ./plan.nix;
  # package-desc = import (
  #   pkgs.haskell-nix.callCabalToNix2 {
  #     name = "multistate";
  #     cabal-file = ./../multistate.cabal;
  #   }
  # );
  # multistate-plan = {
  #   inherit resolver;
  #   extras = hackage:
  #     { multistate = package-desc; };
  # };
  multistate-plan = (pkgs.haskell-nix.importAndFilterProject (
    (pkgs.haskell-nix.callStackToNix {
      name = "multistate-plan";
      src = ./..;
      stackYaml = builtins.toFile "stack.yaml" ''
        resolver: ${resolver}
        packages:
          - '.'
        extra-deps: []
        extra-package-dbs: []
      '';
      ignorePackageYaml = true;
    })
  ));
  hsPkgs = (pkgs.haskell-nix.mkStackPkgSet {
    # src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; name = "multistate"; };
    stack-pkgs = multistate-plan.pkgs;
    pkg-def-extras = pkg-def-extras;
    modules = [
    ];
  }).config.hsPkgs;
in
  {
    inherit multistate-plan hsPkgs pkgs;
    multistate = hsPkgs.multistate;
    tests = hsPkgs.multistate.checks.multistate-test;
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
# pkgs.haskell-nix.stackProject {
#   src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; name = "multistate"; };
#   pkg-def-extras = pkg-def-extras;
#   modules = [
#     { doHaddock = false; }
#   ];
# }
