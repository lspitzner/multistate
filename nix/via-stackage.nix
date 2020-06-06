{ pkgs
, cleanedSource
, resolver
, pkg-def-extras ? []
}:
let
  # package-desc = import ./plan.nix;
  # multistate-plan = {
  #   inherit resolver;
  #   extras = hackage:
  #     { multistate = args: package-desc args // {
  #       src = pkgs.haskell-nix.cleanSourceHaskell {
  #         src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./..; name = "multistate"; };
  #         name = "multistate";
  #       };
  #     };
  #   };
  # };
  # this does not work at all, does not use local package (!)
  # multistate-plan = (pkgs.haskell-nix.importAndFilterProject (
  #   (pkgs.haskell-nix.callStackToNix {
  #     name = "multistate-plan";
  #     src = ./..;
  #     stackYamlFile = builtins.toFile "stack.yaml" ''
  #       resolver: ${resolver}
  #       packages:
  #         - '.'
  #       extra-deps: []
  #       extra-package-dbs: []
  #     '';
  #     ignorePackageYaml = true;
  #   })
  # ));
  # this uses an experimental addtion to haskell-nix; is probably not releavnt
  # any longer. Leaving it here for future reference, until haskell-nix has
  # stabilized.
  # multistate-plan = pkgs.haskell-nix.cabalFileToStackagePlan {
  #   name = "multistate";
  #   src = ./..;
  #   inherit resolver;
  # };
  multistate-pkg = import (
    pkgs.haskell-nix.callCabalToNix {
      name = "multistate";
      src = cleanedSource;
      cabal-file = "multistate.cabal";
    }
  );
  multistate-plan = {
    inherit resolver;
    extras = hackage: { multistate = multistate-pkg; };
  };
  hsPkgs = (pkgs.haskell-nix.mkStackPkgSet {
    stack-pkgs = multistate-plan;
    pkg-def-extras = pkg-def-extras;
    modules = [
    ];
  }).config.hsPkgs;
in {
  inherit multistate-plan hsPkgs pkgs;
  inherit (hsPkgs) multistate;
  inherit (hsPkgs.multistate) checks;
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
# pkgs.haskell-nix.stackProject {
#   src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; name = "multistate"; };
#   pkg-def-extras = pkg-def-extras;
#   modules = [
#     { doHaddock = false; }
#   ];
# }
