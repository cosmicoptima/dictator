with import <nixpkgs> {};
let
  mach-nix = import (builtins.fetchGit {
    url = "https://github.com/DavHau/mach-nix";
    ref = "refs/tags/3.4.0";
  }) {};
  newPython = mach-nix.mkPython {
    requirements = "sentence-transformers";
  };
in
  stdenv.mkDerivation {
    name = "dictator";
    buildInputs = [pkgs.bzip2 pkgs.ghc pkgs.zlib newPython];
  }
