#!/bin/sh
set -eu

cd "$(dirname "$0")"

tmp="$(mktemp /tmp/steel_cycle.XXXXXX)"
cat > "$tmp" <<'EOF'
!muf 4

[workspace]
  .set name "cycle-test"
  .set root "."
..

[bake a]
  .needs "b"
  .exec "echo a"
..

[bake b]
  .needs "a"
  .exec "echo b"
..
EOF

if ./steel run "$tmp" --all --dry-run --log /tmp/steel_cycle.log >/tmp/steel_cycle.out 2>&1; then
  echo "error: expected cycle failure"
  rm -f "$tmp"
  exit 1
fi

if rg -q "dependency cycle detected" /tmp/steel_cycle.out; then
  printf '%s\n' "ok: cycle detected"
  rm -f "$tmp"
  exit 0
fi

echo "error: cycle message not found"
rm -f "$tmp"
exit 1
