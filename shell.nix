{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, async, base, bytestring, containers, deepseq
      , directory, doctest, filepath, mtl, process, split, stdenv
      , stringsearch, tasty, tasty-hunit, tasty-quickcheck
      , template-haskell, temporary, unix, utf8-string, hostname
      , markdown-unlit
      }:
      mkDerivation {
        pname = "shh";
        version = "0.6.0.0";
        src = ./shh;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          async base bytestring containers deepseq directory filepath mtl
          process split stringsearch template-haskell unix utf8-string hostname
        ];
        executableHaskellDepends = [
          async base bytestring deepseq directory temporary unix
        ];
        testHaskellDepends = [
          async base bytestring directory doctest tasty tasty-hunit
          tasty-quickcheck utf8-string markdown-unlit
        ];
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
