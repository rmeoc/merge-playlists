{ pkgs ? import <nixpkgs> {}
, callCabal2nix ? pkgs.haskellPackages.callCabal2nix
, haskell ? pkgs.haskell
}:
  let
    staticDir = builtins.path {
      path = ./static;
      name = "rmeoc-merge-playlists-app-static";
    };
    configFile = pkgs.writeText "rmeoc-merge-playlists-app-config" ''
      static-dir: "${staticDir}"
    '';
    myWebApp = haskell.lib.overrideCabal
      (callCabal2nix "rmeoc-merge-playlists-app" ./. { })
      (drv: {
        doCheck = false;
        doHaddock = false;
      });
  in
    pkgs.writeShellScriptBin "rmeoc-merge-playlists-app-launcher" ''
      exec ${myWebApp}/bin/rmeoc-merge-playlists-app $@ ${configFile}
    ''
