self: super: {
  haskellPackages = super.haskellPackages.override {
    overrides = hpSelf: hpSuper: {
      my-web-app = hpSelf.callPackage ../my-web-app {};
    };
  };
}
