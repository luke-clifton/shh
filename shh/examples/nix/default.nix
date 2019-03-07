with import <nixpkgs> {};
haskellPackages.callCabal2nix "my-script" ./. {
  # By default arguments are pulled from Hackage instead of from
  # nixpkgs, so if you have a system dependency that clashes with
  # a hackage package, you may need to explicitly feed it in here
  # as well as in the build-depends field in the cabal file.

  # For example, if there was a package on Hackage called vim, then
  # we would need to specify the real vim here in order to get it instead
  # of whatever Haskell package is on hackage.

  inherit vim;
}
