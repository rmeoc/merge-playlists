{ config, lib, pkgs, ... }:

let
  stateDir = "/var/rmeoc-merge-playlists-app";
  socketName = "/tmp/rmeoc-merge-playlists-app.socket";
in
{
  imports =
    [
    ];

  options = {

    rmeoc = {

      domain = lib.mkOption {
        type = lib.types.str;
        description = "Domain name";
      };

      mergePlaylistsApp = {

        appRoot = lib.mkOption {
          type = lib.types.str;
          description = "Controls the base of generated URLs.";
        };

        auth0 = {

          clientId = lib.mkOption {
            type = lib.types.str;
            description = "Client ID to use when authenticating with Auth0 server";
          };

          domain = lib.mkOption {
            type = lib.types.str;
            description = "Domain for accessing the Auth0 API";
          };
        };

        spotifyClient.clientId = lib.mkOption {
          type = lib.types.str;
          description = "Client ID to use when authenticating with Spotify server";
        };
      };
    };
  };

  config = {

    nixpkgs.overlays = [ (import ./overlays/overlay.nix) ];

    deployment.keys.client-session.user = "mywebsrv";
    deployment.keys.client-session.group = "mywebsrv";
    deployment.keys.client-session.permissions = "0400";
    
    deployment.keys.auth0-client-secret.user = "mywebsrv";
    deployment.keys.auth0-client-secret.group = "mywebsrv";
    deployment.keys.auth0-client-secret.permissions = "0400";

    deployment.keys.spotify-client-secret.user = "mywebsrv";
    deployment.keys.spotify-client-secret.group = "mywebsrv";
    deployment.keys.spotify-client-secret.permissions = "0400";
    
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
      virtualHosts.${config.rmeoc.domain} = {
        forceSSL = true;
        enableACME = true;
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

    systemd.sockets.rmeoc-merge-playlists-app = rec {
      description = "rmeoc-merge-playlists-app";
      wantedBy = [
        "nginx.service"
      ];
      before = wantedBy;
      listenStreams = [ socketName ];
      socketConfig.Accept = false;
      socketConfig.SocketMode = "0200";
      socketConfig.SocketUser = "nginx";
    };

    systemd.services.rmeoc-merge-playlists-app = rec {
      description = "rmeoc-merge-playlists-app";
      wants = [
        "postgresql.service"
        "client-session-key.service"
        "auth0-client-secret-key.service"
        "spotify-client-secret-key.service"
      ];
      after = wants;
      script = let
        configFile = pkgs.writeText "rmeoc-merge-playlists-app-config" ''
          approot:        "${config.rmeoc.mergePlaylistsApp.appRoot}"
          client-session-key-path: "/run/keys/client-session"
          generated-dir: "${stateDir}"
          database:
            user:     ""
            password: ""
            host:     ""
            port:     0
          auth0:
            client-id:   "${config.rmeoc.mergePlaylistsApp.auth0.clientId}"
            domain:      "${config.rmeoc.mergePlaylistsApp.auth0.domain}"
          spotify-client:
            client-id:   "${config.rmeoc.mergePlaylistsApp.spotifyClient.clientId}"
        '';
      in
        ''
          export YESOD_AUTH0_CLIENT_SECRET=$(cat /run/keys/auth0-client-secret)
          export MERGE_PLAYLISTS_SPOTIFY_CLIENT_SECRET=$(cat /run/keys/spotify-client-secret)
          ${pkgs.haskellPackages.rmeoc-merge-playlists-app}/bin/rmeoc-merge-playlists-app-launcher ${configFile}
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
  };
}

