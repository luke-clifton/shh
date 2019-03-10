#! /usr/bin/env bash

set -e

hackageVersion() {
    curl -fsS -H 'Accept: application/json' "https://hackage.haskell.org/package/$1/preferred" \
        | jq -r '."normal-version"[0]'
}

localVersion() {
    grep '^version:' "$1/$1.cabal" | tr -d '[:space:]' | cut -d: -f2
}

needsUpload() {
    if [ "$(hackageVersion "$1")" = "$(localVersion "$1")" ]
    then
        return 1
    else
        return 0
    fi
}

upload() {
    if needsUpload "$1"
    then
        if sanity "$1"
        then
            toupload="$1-$(localVersion "$1")"
            echo "Uploading $toupload"
            read -rp 'continue? (type "yes") ' answer
            if ! [ "$answer" = "yes" ]
            then
                exit 1
            fi
            tmp=$(mktemp -d)
            cabal new-sdist --verbose=0 --builddir="$tmp" "$1"
            cabal new-haddock --haddock-for-hackage --builddir="$tmp" "$1"
            cabal upload --publish "$tmp/sdist/${toupload}.tar.gz"
            cabal upload --publish -d "$tmp/${toupload}-docs.tar.gz"
            git tag -a "$toupload" -m "Releasing $toupload"
            git push origin "$toupload"
        else
            echo "$1 has issues, not uploading"
        fi
    else
        echo "$1 was not uploaded (versions match)"
        if ! codeMatch "$1"
        then
            echo "... but there is a difference in the tarballs!"
            echo "... bump the version number to upload!"
        fi
    fi
}

codeMatch() {
    tmp=$(mktemp -d)
    cabal new-sdist --verbose=0 --builddir="$tmp" "$1"
    cmp -s -c "$tmp"/sdist/*.tar.gz \
        <(curl -fsS "https://hackage.haskell.org/package/$1/$1-$(hackageVersion "$1").tar.gz")
    a=$?
    rm -Rf "$tmp"
    return "$a"
}

sanity() {
    git diff --exit-code || return 1
    git diff --cached --exit-code || return 1
    cabal new-build "$1" || return 1
    cabal new-test "$1" || return 1
    if cabal new-haddock --haddock-for-hackage "$1" | grep -A 5 'Missing documentation'
    then
        return 1
    fi
}

upload shh
upload shh-extras
