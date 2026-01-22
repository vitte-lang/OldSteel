#!/bin/sh
set -eu

cd "$(dirname "$0")"

os="$(uname -s | tr '[:upper:]' '[:lower:]')"
arch="$(uname -m | tr '[:upper:]' '[:lower:]')"

preset=""
case "$arch" in
  x86_64|amd64) preset="x86-pentium4" ;;
  i386|i486|i586|i686) preset="x86-386" ;;
  armv4*|armv5*) preset="armv5" ;;
  armv6*) preset="armv6" ;;
  armv7*|armhf|armel) preset="armv7" ;;
  aarch64|arm64) preset="arm64" ;;
  ppc|powerpc) preset="ppc32" ;;
  ppc64|ppc64le) preset="ppc64" ;;
  sparc64) preset="sparc64" ;;
  sparc) preset="sparc-v8" ;;
  mips64) preset="mips64" ;;
  mips64el) preset="mips64el" ;;
  mipsel) preset="mipsel" ;;
  mips) preset="mips32" ;;
  riscv64) preset="riscv64" ;;
  riscv32) preset="riscv32" ;;
  s390x|s390) preset="s390" ;;
  sh4) preset="sh4" ;;
  *) preset="" ;;
esac

if [ "$os" != "linux" ]; then
  preset=""
fi

if [ -n "$preset" ]; then
  echo "auto: $os/$arch -> $preset"
  sh ./build_legacy.sh "$preset"
else
  echo "auto: $os/$arch -> native"
  fpc steel.pas
fi
