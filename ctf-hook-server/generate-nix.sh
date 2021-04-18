#!/usr/bin/env nix-shell
#! nix-shell -i bash
filename=generated.nix

set -xe

echo "Generating new $filename"
cabal2nix . > $filename
echo "Success"

git diff-files -U5 $filename
