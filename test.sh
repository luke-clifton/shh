#! /usr/bin/env bash
set -o errexit
set -o nounset
cabal build "$1"
cabal test "$1"
cabal repl "$1" --build-depends=QuickCheck --build-depends=template-haskell --with-ghc=doctest --repl-options='-w -Wdefault'
