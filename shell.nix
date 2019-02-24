with import <nixpkgs> {};
(haskellPackages.callPackage ./shh.nix {}).env
