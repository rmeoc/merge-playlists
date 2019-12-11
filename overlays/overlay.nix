self: super: {
  haskellPackages = super.haskellPackages.override {
    overrides = hpSelf: hpSuper: {
      rmeoc-merge-playlists-app = hpSelf.callPackage ../rmeoc-merge-playlists-app {};
    };
  };
}
