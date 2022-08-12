#!/bin/sh

set -eu

unset OPAM_REPO_COMMIT
nix-shell default.nix -A resolve

sha=$(nix eval --impure --expr \
     '(import ./nix/opam-selection.nix {}).repos.opam-repository.fetch.rev' \
    | sed -e 's/"//g')

sed -i'' -e "s/\(OPAM_REPO_COMMIT=\).*/\1${sha}/" .envrc

