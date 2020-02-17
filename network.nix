{
  webserver =
    { config, pkgs, ... }:
    { 
      imports = [ ./configuration.nix ];

      deployment.targetHost = ???;

      deployment.keys.client-session.keyFile = ./client-session.aes;
      deployment.keys.client-session.user = "mywebsrv";
      deployment.keys.client-session.group = "mywebsrv";
      deployment.keys.client-session.permissions = "0400";
      
      deployment.keys.auth0-client-secret.keyFile = ./auth0-client-secret.txt;
      deployment.keys.auth0-client-secret.user = "mywebsrv";
      deployment.keys.auth0-client-secret.group = "mywebsrv";
      deployment.keys.auth0-client-secret.permissions = "0400";

      deployment.keys.spotify-client-secret.keyFile = ./spotify-client-secret.txt;
      deployment.keys.spotify-client-secret.user = "mywebsrv";
      deployment.keys.spotify-client-secret.group = "mywebsrv";
      deployment.keys.spotify-client-secret.permissions = "0400";
      
      deployment.keys.tls-key.keyFile = ./tls-key.pem;
      deployment.keys.tls-key.user = "nginx";
      deployment.keys.tls-key.group = "nginx";
      deployment.keys.tls-key.permissions = "0400";
    };
}
