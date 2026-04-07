#!/bin/sh
set -eu

EXPECTED=$(node -e "console.log(require('./package-lock.json').packages['node_modules/@playwright/test'].version)")
INSTALLED=$(npm list -g --json playwright | node -e "
  let d='';
  process.stdin.resume();
  process.stdin.on('data', c => d += c);
  process.stdin.on('end', () => console.log(JSON.parse(d).dependencies.playwright.version));
")

if [ "$EXPECTED" != "$INSTALLED" ]; then
  echo "Playwright version mismatch:"
  echo "  package-lock.json: $EXPECTED"
  echo "  devcontainer image: $INSTALLED"
  echo "Update .devcontainer/devcontainer.json playwright version to $EXPECTED"
  exit 1
fi

echo "Playwright version OK: $EXPECTED"
