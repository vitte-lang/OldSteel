#!/bin/sh
set -eu

cd "$(dirname "$0")"

fpc steel.pas >/dev/null

STEEL_OS=Windows_NT STEEL_ARCH=i386 ./steel build steelconf_test --emit /tmp/steel_smoke.log >/dev/null
rg -q "^\\[host\\]$" /tmp/steel_smoke.log
rg -q "^[[:space:]]*os \"windows\"$" /tmp/steel_smoke.log
rg -q "^[[:space:]]*arch \"x86\"$" /tmp/steel_smoke.log
rg -q "^profile \"release\"$" /tmp/steel_smoke.log
rg -q "^\\[target\\.sets\\]$" /tmp/steel_smoke.log
rg -q "^[[:space:]]*\\.set output bin/app$" /tmp/steel_smoke.log

STEEL_OS=Windows_NT STEEL_ARCH=i386 ./steel run steelconf_test --all --dry-run --log /tmp/steel_smoke_run.mff > /tmp/steel_smoke_run.out
rg -q "^dry-run: exec echo tool$" /tmp/steel_smoke_run.out
rg -q "^dry-run: exec echo build$" /tmp/steel_smoke_run.out
rg -q "^dry-run: exec echo lib$" /tmp/steel_smoke_run.out
rg -q "^dry-run: source .*steelconf_test$" /tmp/steel_smoke_run.out
rg -q "^dry-run: source .*steel.pas$" /tmp/steel_smoke_run.out
rg -q "^dry-run: source .*steel_types.pas$" /tmp/steel_smoke_run.out
rg -q "^dry-run: file .*target.*/artifacts/out3$" /tmp/steel_smoke_run.out
rg -q "^dry-run: file .*target.*/artifacts/out4$" /tmp/steel_smoke_run.out
rg -q "^dry-run: file .*target.*/artifacts/lib.out$" /tmp/steel_smoke_run.out

STEEL_OS=Windows_NT STEEL_ARCH=i386 ./steel run steelconf_test --all --no-cache --log /tmp/steel_smoke_runlog.log >/dev/null
rg -q "^\\[log meta\\]$" /tmp/steel_smoke_runlog.log
rg -q "^format \"steel-runlog-1\"$" /tmp/steel_smoke_runlog.log
rg -q "^\\[bake log \"lib\"\\]$" /tmp/steel_smoke_runlog.log
rg -q "^\\[bake log \"app\"\\]$" /tmp/steel_smoke_runlog.log
rg -q "^output \"artifacts/lib.out\"$" /tmp/steel_smoke_runlog.log
rg -q "^output \"artifacts/out3\"$" /tmp/steel_smoke_runlog.log
rg -q "^source \"steel_types.pas\"$" /tmp/steel_smoke_runlog.log
rg -q "^source \"steelconf_test\"$" /tmp/steel_smoke_runlog.log

sh "./verify_cycle.sh"
sh "./verify_cache.sh"
sh "./verify_presets.sh"

printf '%s\n' "ok: smoke verified"
