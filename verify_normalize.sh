#!/bin/sh
set -eu

cd "$(dirname "$0")"

fpc steel.pas >/dev/null

STEEL_OS=Windows_NT STEEL_ARCH=i386 ./steel build steelconf_test --emit /tmp/steel_norm.log >/dev/null
rg -q "^\\[host\\]$" /tmp/steel_norm.log
rg -q "^[[:space:]]*os \"windows\"$" /tmp/steel_norm.log
rg -q "^[[:space:]]*arch \"x86\"$" /tmp/steel_norm.log

STEEL_OS=SunOS STEEL_ARCH=sparcv9 ./steel build steelconf_test --emit /tmp/steel_norm.log >/dev/null
rg -q "^[[:space:]]*os \"solaris\"$" /tmp/steel_norm.log
rg -q "^[[:space:]]*arch \"sparc64\"$" /tmp/steel_norm.log

STEEL_OS=macosx STEEL_ARCH=amd64 ./steel build steelconf_test --emit /tmp/steel_norm.log >/dev/null
rg -q "^[[:space:]]*os \"macos\"$" /tmp/steel_norm.log
rg -q "^[[:space:]]*arch \"x86_64\"$" /tmp/steel_norm.log

STEEL_OS="Windows 95" STEEL_ARCH="Pentium II" ./steel build steelconf_test --emit /tmp/steel_norm.log >/dev/null
rg -q "^[[:space:]]*os \"windows\"$" /tmp/steel_norm.log
rg -q "^[[:space:]]*arch \"x86\"$" /tmp/steel_norm.log

STEEL_OS="Mac OS 9" STEEL_ARCH="PowerPC G3" ./steel build steelconf_test --emit /tmp/steel_norm.log >/dev/null
rg -q "^[[:space:]]*os \"macos-classic\"$" /tmp/steel_norm.log
rg -q "^[[:space:]]*arch \"ppc\"$" /tmp/steel_norm.log

STEEL_OS="Plan 9" STEEL_ARCH="DEC Alpha" ./steel build steelconf_test --emit /tmp/steel_norm.log >/dev/null
rg -q "^[[:space:]]*os \"plan9\"$" /tmp/steel_norm.log
rg -q "^[[:space:]]*arch \"alpha\"$" /tmp/steel_norm.log

printf '%s\n' "ok: normalization verified"
