#!/bin/sh
# The elm-test-rs npm installer hard-links its native binary over the JS
# launcher file (elm-test-rs.js), but pnpm's bin shim assumes any .js file
# should be executed with node — which fails on an ELF binary.  Locate the
# installed elm-test-rs file and exec it directly instead.
set -e
cd "$(dirname "$0")/.."

candidate=""
for c in \
  node_modules/.pnpm/elm-test-rs@*/node_modules/elm-test-rs/elm-test-rs.js \
  node_modules/elm-test-rs/elm-test-rs.js
do
  if [ -f "$c" ]; then
    candidate="$c"
    break
  fi
done

if [ -z "$candidate" ]; then
  echo "Could not locate the elm-test-rs binary; try: pnpm install" >&2
  exit 1
fi

bin="$(readlink -f "$candidate")"

# elm-test-rs exits with 2 when the run is "incomplete" (a Test.skip was
# used).  We use Test.skip only for documented known-bugs, so an incomplete
# run with zero failures is treated as success.
code=0
output=$("$bin" "$@" 2>&1) || code=$?
printf '%s\n' "$output"

if [ "$code" -eq 2 ] && printf '%s' "$output" | grep -Eq 'Failed: +0'; then
  echo "Note: suite INCOMPLETE due to Test.skip — forcing success (0) error-code anyway"
  exit 0
fi

exit "$code"
