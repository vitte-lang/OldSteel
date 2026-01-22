#!/bin/sh
set -eu

cd "$(dirname "$0")"

fpc steel.pas >/dev/null
./steel build steelconf_test --emit /tmp/steel.log

if rg -q "^[[:space:]]*profile \"release\"$" /tmp/steel.log \
  && rg -q "^\\[target\\.sets\\]$" /tmp/steel.log \
  && rg -q "^[[:space:]]*\\.set output bin/app$" /tmp/steel.log; then
  printf '%s\n' "ok: emit verified"
  exit 0
fi

printf '%s\n' "error: emit verification failed"
exit 1
