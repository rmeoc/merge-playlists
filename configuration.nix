{ config, pkgs, ... }:

let
  stateDir = "/var/rmeoc-merge-playlists-app";
  myWebAppConfigFile = pkgs.writeText "rmeoc-merge-playlists-app-config" ''
    approot:        ???
    client-session-key-path: "/run/keys/client-session"
    generated-dir: "${stateDir}"
    database:
      user:     ""
      password: ""
      host:     ""
      port:     0
    auth0:
      client-id:   ???
      domain:      ???
    spotify-client:
      client-id:   ???
  '';
  socketName = "/tmp/rmeoc-merge-playlists-app.socket";
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
  networking.firewall.allowedTCPPorts = [443];

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_11;
    authentication = pkgs.lib.mkOverride 10 ''
      local all all peer
    '';
    ensureDatabases = [
      "rmeoc-merge-playlists-app"
    ];
    ensureUsers = [
      {
        name = "mywebsrv";
        ensurePermissions = {
          "DATABASE \"rmeoc-merge-playlists-app\"" = "ALL PRIVILEGES";
        };
      }
    ];
  };

  services.nginx = {
    enable = true;
    virtualHosts._ = {
      forceSSL = true;
      sslCertificate = "${./tls-certificate.pem}";
      sslCertificateKey = "/run/keys/tls-key";
      locations."/" = {
        extraConfig = ''
          scgi_pass unix:${socketName};
          scgi_param  QUERY_STRING       $query_string;
          scgi_param  REQUEST_METHOD     $request_method;
          scgi_param  CONTENT_TYPE       $content_type;
          scgi_param  CONTENT_LENGTH     $content_length;
          scgi_param  PATH_INFO          $fastcgi_script_name;
          scgi_param  SERVER_PROTOCOL    $server_protocol;
          scgi_param  GATEWAY_INTERFACE  CGI/1.1;
          scgi_param  SERVER_SOFTWARE    nginx/$nginx_version;
          scgi_param  REMOTE_ADDR        $remote_addr;
          scgi_param  SERVER_ADDR        $server_addr;
          scgi_param  SERVER_PORT        $server_port;
          scgi_param  SERVER_NAME        $server_name;
        '';
      };
    };
  };

  systemd.sockets.rmeoc-merge-playlists-app =
  { description = "rmeoc-merge-playlists-app";
    listenStreams = [ socketName ];
    socketConfig.Accept = false;
    socketConfig.SocketMode = 220;
    socketConfig.SocketUser = "nginx";
  };

  systemd.services.rmeoc-merge-playlists-app =
  { description = "rmeoc-merge-playlists-app";
    wants = [ "postgresql.service" "client-session-key.service" "auth0-client-secret-key.service" ];
    after = [ "postgresql.service" "client-session-key.service" "auth0-client-secret-key.service" ];
    script = ''
      export YESOD_AUTH0_CLIENT_SECRET=$(cat /run/keys/auth0-client-secret)
      export MERGE_PLAYLISTS_SPOTIFY_CLIENT_SECRET=$(cat /run/keys/spotify-client-secret)
      ${pkgs.haskellPackages.rmeoc-merge-playlists-app}/bin/rmeoc-merge-playlists-app-launcher ${myWebAppConfigFile}
    '';
    serviceConfig.User = "mywebsrv";
    serviceConfig.Group = "mywebsrv";
    serviceConfig.WorkingDirectory = stateDir;
    serviceConfig.StandardInput="socket";
    serviceConfig.StandardOutput="journal";
    serviceConfig.StandardError="journal";
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

