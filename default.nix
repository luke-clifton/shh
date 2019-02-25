with import <nixpkgs> {};
haskellPackages.callCabal2nix "shh" ./. {
  inherit vault;
}
