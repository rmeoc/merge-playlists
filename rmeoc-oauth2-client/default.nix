{ pkgs ? import <nixpkgs> {}
, callCabal2nix ? pkgs.haskellPackages.callCabal2nix
}:
  callCabal2nix "rmeoc-oauth2-client" ./. { }
