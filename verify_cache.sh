#!/bin/sh
set -eu

cd "$(dirname "$0")"

rm -f /tmp/steel_cache.log /tmp/steel_cache.out /tmp/steel_cache.out2
./steel run steelconf_test --all --log /tmp/steel_cache.log > /tmp/steel_cache.out 2>&1 || exit 1
./steel run steelconf_test --all --verbose --log /tmp/steel_cache.log > /tmp/steel_cache.out2 2>&1 || exit 1

if rg -q "^skip " /tmp/steel_cache.out2; then
  printf '%s\n' "ok: cache skip observed"
  exit 0
fi

echo "error: expected skip not found"
exit 1
