{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, async, base, coreutils, deepseq, directory
      , filepath, hashable, mtl, perl, process, split, stdenv, tasty
      , tasty-hunit, tasty-quickcheck, template-haskell, temporary, unix
      , vim, hostname
      }:
      mkDerivation {
        pname = "shh";
        version = "0.2.0.2";
        src = ./shh;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          async base deepseq directory filepath mtl process split
          template-haskell unix hostname temporary
        ];
        executableHaskellDepends = [
          async base directory hashable split temporary
        ];
        executableToolDepends = [ coreutils vim ];
        testHaskellDepends = [ base tasty tasty-hunit tasty-quickcheck ];
        testToolDepends = [ perl vim ];
        description = "Simple shell scripting from Haskell";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
