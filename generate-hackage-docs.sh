#!/bin/sh
set -e

#dir=$(mktemp -d dist-docs.XXXXXX)
#trap 'rm -r "$dir"' EXIT
dir=hackage-docs

# echo "$dir"

# assumes cabal 2.4 or later
stack exec --no-ghc-package-path -- cabal v2-haddock --builddir="$dir" --haddock-for-hackage --enable-doc

# this unpacks to .tar, which is the file format expected by Hackage
cd $dir
gunzip monomer*-docs.tar.gz
cd ..

# echo "Uploading $dir"

# stack exec --no-ghc-package-path -- cabal v2-upload -d --publish $dir/monomer*-docs.tar.gz
