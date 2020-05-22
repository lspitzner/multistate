args@{pkgs ? import <nixpkgs.nix>}:
if !(builtins.hasAttr "haskell-nix" pkgs)
then
  builtins.throw "this package requires haskell.nix overlay!"
else
let
  versions = {
    "stack-8.0" = import ./via-stack.nix (args // { resolver = "lts-9.21"; });
    "stack-8.2" = import ./via-stack.nix (args // { resolver = "lts-11.22"; });
    "stack-8.4" = import ./via-stack.nix (args // { resolver = "lts-12.26"; });
    "stack-8.6" = import ./via-stack.nix (args // { resolver = "lts-14.27"; });
    "stack-8.8" = import ./via-stack.nix (args // { resolver = "lts-15.12"; });
    "cabal-8.4" = import ./via-cabal.nix (args // { 
      ghc-ver = "ghc844";
      index-state = "2020-05-01T00:00:00Z";
      # plan-sha256 = "0s6rfanb6zxhr5zbinp7h25ahwasciwj3ambsr6zdxm1l782b3ap";
      # materialized = ./materialized/cabal-8.4;
    });
    "cabal-8.6" = import ./via-cabal.nix (args // { 
      ghc-ver = "ghc865";
      index-state = "2020-05-01T00:00:00Z";
      # plan-sha256 = "01m95xirrh00dvdxrpsx8flhcwlwcvgr3diwlnkw7lj5f3i7rfrl";
      # materialized = ./materialized/cabal-8.6;
    });
    "cabal-8.8" = import ./via-cabal.nix (args // { 
      ghc-ver = "ghc883";
      index-state = "2020-05-01T00:00:00Z";
      # plan-sha256 = "14qs7ynlf7p2qvdk8sf498y87ss5vab3ylnbpc8sacqbpv2hv4pf";
      # materialized = ./materialized/cabal-8.8;
    });
  } // (if builtins.hasAttr "ghc8101" pkgs.haskell-nix.compiler
    then {
    "cabal-8.10" = import ./via-cabal.nix (args // { 
      ghc-ver = "ghc8101";
      index-state = "2020-05-01T00:00:00Z";
      # plan-sha256 = "1s8a6cb5qgf4ky5s750rzx6aa52slp1skazh8kbx0dbfjd6df7yw";
      # materialized = ./materialized/cabal-8.10;
    });
    } else builtins.trace "warn: ghc 8.10 is not avaiable, will not be tested!" {}
  );
  linkFarmFromDrvs = name: drvs:
    let mkEntryFromDrv = drv: { name = drv.name; path = drv; };
    in pkgs.linkFarm name (map mkEntryFromDrv drvs);
in
versions // {
  default = versions."stack-8.8";
  test-all = pkgs.linkFarm "multistate-test-all" [
    { name = "stack-8.04"; path = versions."stack-8.4".tests; }
    { name = "stack-8.06"; path = versions."stack-8.6".tests; }
    { name = "stack-8.08"; path = versions."stack-8.8".tests; }
    { name = "cabal-8.04"; path = versions."cabal-8.4".tests; }
    { name = "cabal-8.06"; path = versions."cabal-8.6".tests; }
    { name = "cabal-8.08"; path = versions."cabal-8.8".tests; }
    { name = "cabal-8.10"; path = versions."cabal-8.10".tests; }
  ];
}