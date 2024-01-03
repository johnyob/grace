#!/usr/bin/env bash
set -euo pipefail

# Often we want to avoid formatting unstaged changes (since they likely won't compile, etc)
# So we stash them, run `make fmt`, then pop the stash if it was non-empty
stash_flag=false
if ! git diff --exit-code --quiet; then
  git stash save --keep-index
  stash_flag=true
fi

make fmt

# Check if there are any changes
if ! git diff --exit-code; then
  cat <<EOF
⚠️ Some files were not correctly formatted! ⚠️
You can use 'git diff' to see the changes made by 'make fmt'.

If your git index originally contained some unstaged files, they have 
been stashed. Run 'git stash pop' to retrieve them.
EOF
    exit 1
fi

# Pop the stash if we stashed earlier (if there were no formatting changes)
if $stash_flag; then
  git stash pop || true
fi