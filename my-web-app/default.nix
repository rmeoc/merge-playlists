{ pkgs ? import <nixpkgs> {}
, callCabal2nix ? pkgs.haskellPackages.callCabal2nix
, haskell ? pkgs.haskell
}:
  let
    staticDir = builtins.path {
      path = ./static;
      name = "my-web-app-static";
    };
    configFile = pkgs.writeText "my-web-app-config" ''
      static-dir: "${staticDir}"
    '';
    myWebApp = haskell.lib.overrideCabal
      (callCabal2nix "my-web-app" ./. { })
      (drv: {
        doCheck = false;
        doHaddock = false;
      });
  in
    pkgs.writeShellScriptBin "my-web-app-launcher" ''
      exec ${myWebApp}/bin/my-web-app $@ ${configFile}
    ''
