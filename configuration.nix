{ config, pkgs, ... }:

let
  stateDir = "/var/rmeoc-merge-playlists-app";
  myWebAppConfigFile = pkgs.writeText "rmeoc-merge-playlists-app-config" ''
    client-session-key-path: "/run/keys/client-session"
    generated-dir: "${stateDir}"
    tls:
      certificate-file: "${./tls-certificate.pem}"
      key-file:         "/run/keys/tls-key"
    auth0:
      client-id:   ???
      domain:      ???
    spotify-client:
      client-id:   ???
  '';
in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  nixpkgs.overlays = [ (import ./overlays/overlay.nix) ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/xvda"; # or "nodev" for efi only

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = ???;
  networking.interfaces.eth0.useDHCP = ???;
  networking.interfaces.eth0.ipv4.addresses = [{
    address = ???;
    prefixLength = ???;
  }];
  networking.defaultGateway = { address = ???; interface = ???; };
  networking.nameservers = [???];
  networking.firewall.allowedTCPPorts = [3000];

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_11;
    authentication = pkgs.lib.mkOverride 10 ''
      local all postgres         ident
      local all all              md5
      host  all all      ::1/128 md5
    '';
    initialScript = pkgs.writeText "backend-initScript" ''
      CREATE USER "rmeoc-merge-playlists-app" WITH PASSWORD 'rmeoc-merge-playlists-app';
      CREATE DATABASE "rmeoc-merge-playlists-app" WITH OWNER "rmeoc-merge-playlists-app";
    '';
  };

  systemd.services.rmeoc-merge-playlists-app =
  { description = "rmeoc-merge-playlists-app";
    wants = [ "postgresql.service" "client-session-key.service" "auth0-client-secret-key.service" ];
    after = [ "postgresql.service" "client-session-key.service" "auth0-client-secret-key.service" ];
    wantedBy = [ "multi-user.target" ];
    script = ''
      export YESOD_AUTH0_CLIENT_SECRET=$(cat /run/keys/auth0-client-secret)
      export MERGE_PLAYLISTS_SPOTIFY_CLIENT_SECRET=$(cat /run/keys/spotify-client-secret)
      ${pkgs.haskellPackages.rmeoc-merge-playlists-app}/bin/rmeoc-merge-playlists-app-launcher ${myWebAppConfigFile}
    '';
    serviceConfig =
      { User = "mywebsrv";
        Group = "mywebsrv";
        WorkingDirectory = stateDir;
      };
  };

  users.users = {
    mywebsrv = {
      description = "mywebsrv";
      home = stateDir;
      createHome = true;
      useDefaultShell = true;
      group = "mywebsrv";
      extraGroups = [ "keys" ];
      isSystemUser = true;
    };
  };

  users.groups.mywebsrv = {};

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.09"; # Did you read the comment?
}

