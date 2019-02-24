{ mkDerivation, async, base, coreutils, deepseq, directory
, filepath, mtl, perl, process, split, stdenv, tasty, tasty-hunit
, tasty-quickcheck, template-haskell, unix, vim
}:
mkDerivation {
  pname = "shh";
  version = "0.2.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async base deepseq directory filepath mtl process split
    template-haskell unix
  ];
  executableHaskellDepends = [ async base ];
  executableToolDepends = [ coreutils ];
  testHaskellDepends = [ base tasty tasty-hunit tasty-quickcheck ];
  testToolDepends = [ perl vim ];
  description = "Simple shell scripting from Haskell";
  license = stdenv.lib.licenses.bsd3;
}
