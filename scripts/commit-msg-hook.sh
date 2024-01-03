#!/usr/bin/env bash
set -euo pipefail

COMMIT_FILE=$1
COMMIT_MSG=$(cat "$COMMIT_FILE")

# Check if the commit message follows commit linting rules
if ! echo "$COMMIT_MSG" | npx commitlint; then
  echo "⚠️ Commit message does not follow commit linting rules ⚠️"
  exit 1
fi