self: super: {
  haskellPackages = super.haskellPackages.override {
    overrides = hpSelf: hpSuper: {
      rmeoc-merge-playlists-app = hpSelf.callPackage ../rmeoc-merge-playlists-app {};
      rmeoc-oauth2-client = hpSelf.callPackage ../rmeoc-oauth2-client {};
      rmeoc-playlist-tools = hpSelf.callPackage ../rmeoc-playlist-tools {};
      rmeoc-spotify-client = hpSelf.callPackage ../rmeoc-spotify-client {};
      rmeoc-utils = hpSelf.callPackage ../rmeoc-utils {};
    };
  };
}
