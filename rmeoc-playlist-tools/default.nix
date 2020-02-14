{ pkgs ? import <nixpkgs> {}
, callCabal2nix ? pkgs.haskellPackages.callCabal2nix
}:
  callCabal2nix "rmeoc-playlist-tools" ./. { }
