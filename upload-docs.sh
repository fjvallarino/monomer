#!/bin/sh
set -e

dir=$(mktemp -d dist-docs.XXXXXX)
#trap 'rm -r "$dir"' EXIT
echo "$dir"

# assumes cabal 2.4 or later
stack exec --no-ghc-package-path -- cabal v2-haddock --builddir="$dir" --haddock-for-hackage --enable-doc

echo "Uploading $dir"

stack exec --no-ghc-package-path -- cabal upload -d --publish $dir/monomer*-docs.tar.gz
