{ config, lib, pkgs, ... }:

{
  imports = [ ];

  boot.initrd.availableKernelModules = [ "ata_piix" "ehci_pci" "sr_mod" "xen_blkfront" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = ???;
      fsType = ???;
    };

  swapDevices =
    [ { device = ???; }
    ];

  nix.maxJobs = lib.mkDefault 2;
}
