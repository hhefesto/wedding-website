#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

nix build "$repo_root#website" --out-link "$repo_root/site-build"

mkdir -p "$repo_root/site"
rsync -r --delete --no-perms --chmod=Du+rwx,Fu+rw \
  "$repo_root/site-build/" "$repo_root/site/"

echo "Local deployable site refreshed in: $repo_root/site"
