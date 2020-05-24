let
  haskellNix = import (
    builtins.fetchTarball
      https://github.com/lspitzner/haskell.nix/archive/3e32c4af7465c410ebe151f2c26ba2e97ba0d5fb.tar.gz
  ) { version = 2; };
  nixpkgsSrc = haskellNix.sources.nixpkgs-1909;
in
import ./all.nix {
  pkgs = import nixpkgsSrc haskellNix.nixpkgsArgs;
}