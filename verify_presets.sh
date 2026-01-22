#!/bin/sh
set -eu

cd "$(dirname "$0")"

PRESETS=$(awk '
  $1 ~ /^[a-z0-9][a-z0-9-]*\)$/ {
    gsub(")", "", $1);
    print $1
  }
' build_legacy.sh | sort -u)

missing=0
for preset in $PRESETS; do
  case "$preset" in
    dos-8086) profile="legacy-386" ;;
    i386) profile="legacy-386" ;;
    x86-386) profile="legacy-386" ;;
    x86-486) profile="legacy-386" ;;
    x86-pentium|x86-pentium-mmx|x86-pentium2|x86-pentium3|x86-pentium4|x86-pentium-m) profile="legacy-pentium" ;;
    x86-k6|x86-k6-2) profile="legacy-k6" ;;
    x86-k6-3) profile="legacy-k6-3" ;;
    x86-athlon|x86-athlon-xp) profile="legacy-athlon-xp" ;;
    x86-duron) profile="legacy-duron" ;;
    x86-sempron) profile="legacy-sempron" ;;
    x86-c3) profile="legacy-c3" ;;
    x86-winchip) profile="legacy-winchip" ;;
    ppc603|ppc7400|ppc970|ppc32) profile="legacy-ppc" ;;
    ppc64) profile="legacy-ppc64" ;;
    m68k-020) profile="legacy-m68k" ;;
    m68k-060) profile="legacy-m68k-060" ;;
    m68k-amiga) profile="legacy-amiga" ;;
    sparc-v8) profile="legacy-sparc" ;;
    sparc-v9|sparc64) profile="legacy-sparc64" ;;
    sparc-ultra) profile="legacy-sparc-ultra" ;;
    mips32) profile="legacy-mips" ;;
    mips64|mips64el) profile="legacy-mips64" ;;
    mipsel) profile="legacy-mipsel" ;;
    mipseb) profile="legacy-mipseb" ;;
    armv4) profile="legacy-armv4" ;;
    armv5) profile="legacy-armv5" ;;
    armv6) profile="legacy-armv6" ;;
    armv7) profile="legacy-armv7" ;;
    arm64) profile="legacy-arm64" ;;
    arm64-tahoe) profile="legacy-arm64" ;;
    riscv32) profile="legacy-riscv32" ;;
    riscv64) profile="legacy-riscv64" ;;
    arc) profile="legacy-arc" ;;
    os2-386) profile="legacy-os2" ;;
    beos-x86) profile="legacy-beos" ;;
    qnx-x86) profile="legacy-qnx" ;;
    plan9-386) profile="legacy-plan9" ;;
    aix-ppc) profile="legacy-aix" ;;
    irix-mips) profile="legacy-irix" ;;
    ultrix-mips) profile="legacy-ultrix" ;;
    alpha) profile="legacy-alpha" ;;
    hppa) profile="legacy-hppa" ;;
    vax) profile="legacy-vax" ;;
    m88k) profile="legacy-m88k" ;;
    s390) profile="legacy-s390" ;;
    sh4) profile="legacy-sh4" ;;
  esac

  if rg -Fq "[profile ${profile}]" steelconf_test; then
    printf 'ok: %-12s -> %s\n' "$preset" "$profile"
  else
    printf 'missing: %-12s -> %s\n' "$preset" "$profile"
    missing=$((missing+1))
  fi
done

if [ "$missing" -gt 0 ]; then
  printf '%s\n' "error: ${missing} presets missing profiles"
  exit 1
fi

printf '%s\n' "ok: all presets have profiles"
