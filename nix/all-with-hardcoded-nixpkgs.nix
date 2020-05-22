let
  haskellNix = import (
    builtins.fetchTarball
      https://github.com/lspitzner/haskell.nix/archive/4560bbb5f95e81e2b78efb7c4b4b55aa899e6f29.tar.gz
  ) { version = 2; };
  nixpkgsSrc = haskellNix.sources.nixpkgs-1909;
in
import ./all.nix {
  pkgs = import nixpkgsSrc haskellNix.nixpkgsArgs;
}