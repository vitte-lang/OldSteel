#!/bin/sh
set -eu

usage() {
  cat <<'EOF'
Usage: build_legacy.sh <preset>

Presets:
  dos-8086     DOS 16-bit (go32v2 toolchain)
  i386         Linux i386 (alias)
  x86-386      Linux i386 (386-compatible)
  x86-486      Linux i486
  x86-pentium  Linux Pentium
  x86-pentium2 Linux Pentium II
  x86-pentium3 Linux Pentium III
  x86-pentium4 Linux Pentium 4
  x86-pentium-mmx Linux Pentium MMX
  x86-k6       Linux AMD K6
  x86-k6-2     Linux AMD K6-2
  x86-k6-3     Linux AMD K6-III
  x86-pentium-m Linux Pentium M
  x86-duron    Linux AMD Duron
  x86-sempron  Linux AMD Sempron
  x86-c3       Linux VIA C3
  x86-winchip  Linux IDT WinChip
  x86-athlon-xp Linux Athlon XP
  x86-athlon   Linux Athlon (K7)
  ppc603       PowerPC 603 (classic)
  ppc7400      PowerPC G4 (7400)
  ppc970       PowerPC G5 (970)
  m68k-020     Motorola 68020
  m68k-amiga   AmigaOS m68k (68020)
  sparc-v8     SPARC v8
  sparc-v9     SPARC v9
  mips32       MIPS32
  mips64       MIPS64
  sparc64      SPARC64 (alias for sparc-v9)
  mipsel       MIPSEL
  mipseb       MIPS big-endian
  mips64el     MIPS64EL
  ppc32        PowerPC 32-bit (alias)
  ppc64        PowerPC 64-bit
  armv5        ARMv5
  armv7        ARMv7
  armv4        ARMv4
  armv6        ARMv6
  m68k-060     Motorola 68060
  sparc-ultra  SPARC Ultra (v9)
  riscv32      RISC-V 32
  riscv64      RISC-V 64
  arc          ARC (if toolchain available)
  arm64-tahoe  ARM64 (Tahoe example)
  arm64        ARM64 (alias for arm64-tahoe)
  sparc-v8     SPARC v8 (alias)
  sparc-v9     SPARC v9 (alias)
  os2-386      OS/2 i386
  beos-x86     BeOS x86
  qnx-x86      QNX x86
  plan9-386    Plan 9 i386 (requires toolchain)
  aix-ppc      AIX PowerPC (requires toolchain)
  irix-mips    IRIX MIPS (requires toolchain)
  ultrix-mips  Ultrix MIPS (requires toolchain)
  m88k         Motorola 88000 (requires toolchain)
  s390         IBM s390 (requires toolchain)
  sh4          SuperH SH-4 (requires toolchain)
  alpha        DEC Alpha (requires toolchain)
  hppa         PA-RISC (requires toolchain)
  vax          VAX/VMS (requires toolchain)

Examples:
  sh build_legacy.sh x86-386
  sh build_legacy.sh ppc603
EOF
}

if [ $# -ne 1 ]; then
  usage
  exit 1
fi

PRESET="$1"
case "$PRESET" in
  dos-8086)
    fpc -Tgo32v2 -Pi386 -CX -Xs -O1 -Os steel.pas
    ;;
  i386)
    fpc -Tlinux -Pi386 -Cp386 -O1 -CfSOFT -Os steel.pas
    ;;
  x86-386)
    fpc -Tlinux -Pi386 -Cp386 -O1 -CfSOFT -Os steel.pas
    ;;
  x86-486)
    fpc -Tlinux -Pi386 -Cp486 -O1 -CfSOFT -Os steel.pas
    ;;
  x86-pentium)
    fpc -Tlinux -Pi386 -CpPENTIUM -O2 -XX -Os steel.pas
    ;;
  x86-pentium2)
    fpc -Tlinux -Pi386 -CpPENTIUM2 -O2 -XX -Os steel.pas
    ;;
  x86-pentium3)
    fpc -Tlinux -Pi386 -CpPENTIUM3 -O2 -XX -Os steel.pas
    ;;
  x86-pentium4)
    fpc -Tlinux -Pi386 -CpPENTIUM4 -O2 -XX -Os steel.pas
    ;;
  x86-pentium-mmx)
    fpc -Tlinux -Pi386 -CpPENTIUMMMX -O2 -XX -Os steel.pas
    ;;
  x86-k6)
    fpc -Tlinux -Pi386 -CpK6 -O2 -XX -Os steel.pas
    ;;
  x86-k6-2)
    fpc -Tlinux -Pi386 -CpK6_2 -O2 -XX -Os steel.pas
    ;;
  x86-k6-3)
    fpc -Tlinux -Pi386 -CpK6_3 -O2 -XX -Os steel.pas
    ;;
  x86-pentium-m)
    fpc -Tlinux -Pi386 -CpPENTIUMM -O2 -XX -Os steel.pas
    ;;
  x86-duron)
    fpc -Tlinux -Pi386 -CpATHLON -O2 -XX -Os steel.pas
    ;;
  x86-sempron)
    fpc -Tlinux -Pi386 -CpATHLON -O2 -XX -Os steel.pas
    ;;
  x86-c3)
    fpc -Tlinux -Pi386 -CpK6_2 -O2 -XX -Os steel.pas
    ;;
  x86-winchip)
    fpc -Tlinux -Pi386 -CpPENTIUM -O2 -XX -Os steel.pas
    ;;
  x86-athlon-xp)
    fpc -Tlinux -Pi386 -CpATHLONXP -O2 -XX -Os steel.pas
    ;;
  x86-athlon)
    fpc -Tlinux -Pi386 -CpATHLON -O2 -XX -Os steel.pas
    ;;
  ppc603)
    fpc -Tlinux -Ppowerpc -Cppc603 -O1 -Os steel.pas
    ;;
  ppc7400)
    fpc -Tlinux -Ppowerpc -Cppc7400 -O1 -Os steel.pas
    ;;
  ppc970)
    fpc -Tlinux -Ppowerpc -Cppc970 -O1 -Os steel.pas
    ;;
  m68k-020)
    fpc -Tmorphos -Pmc68020 -O1 -Os steel.pas
    ;;
  m68k-amiga)
    fpc -Tamiga -Pmc68020 -O1 -Os steel.pas
    ;;
  sparc-v8)
    fpc -Tlinux -Psparc -CpSPARCv8 -O1 -Os steel.pas
    ;;
  sparc-v9)
    fpc -Tlinux -Psparc64 -CpSPARCv9 -O1 -Os steel.pas
    ;;
  mips32)
    fpc -Tlinux -Pmips -O1 -Os steel.pas
    ;;
  mips64)
    fpc -Tlinux -Pmips64 -O1 -Os steel.pas
    ;;
  sparc64)
    fpc -Tlinux -Psparc64 -CpSPARCv9 -O1 -Os steel.pas
    ;;
  mipsel)
    fpc -Tlinux -Pmipsel -O1 -Os steel.pas
    ;;
  mipseb)
    fpc -Tlinux -Pmips -O1 -Os steel.pas
    ;;
  mips64el)
    fpc -Tlinux -Pmips64 -O1 -Os steel.pas
    ;;
  ppc32)
    fpc -Tlinux -Ppowerpc -O1 -Os steel.pas
    ;;
  ppc64)
    fpc -Tlinux -Ppowerpc64 -O1 -Os steel.pas
    ;;
  armv5)
    fpc -Tlinux -Parm -CpARMV5 -O1 -Os steel.pas
    ;;
  armv7)
    fpc -Tlinux -Parm -CpARMV7 -O1 -Os steel.pas
    ;;
  armv4)
    fpc -Tlinux -Parm -CpARMV4T -O1 -Os steel.pas
    ;;
  armv6)
    fpc -Tlinux -Parm -CpARMV6 -O1 -Os steel.pas
    ;;
  m68k-060)
    fpc -Tmorphos -Pmc68060 -O1 -Os steel.pas
    ;;
  sparc-ultra)
    fpc -Tlinux -Psparc64 -CpSPARCv9 -O1 -Os steel.pas
    ;;
  riscv32)
    fpc -Tlinux -Priscv32 -O1 -Os steel.pas
    ;;
  riscv64)
    fpc -Tlinux -Priscv64 -O1 -Os steel.pas
    ;;
  arc)
    fpc -Tlinux -Parc -O1 -Os steel.pas
    ;;
  arm64-tahoe)
    fpc -Tlinux -Paarch64 -O2 -Os steel.pas
    ;;
  arm64)
    fpc -Tlinux -Paarch64 -O2 -Os steel.pas
    ;;
  sparc-v8)
    fpc -Tlinux -Psparc -CpSPARCv8 -O1 -Os steel.pas
    ;;
  sparc-v9)
    fpc -Tlinux -Psparc64 -CpSPARCv9 -O1 -Os steel.pas
    ;;
  os2-386)
    fpc -Tos2 -Pi386 -Cp386 -O1 -Os steel.pas
    ;;
  beos-x86)
    fpc -Tbeos -Pi386 -Cp486 -O1 -Os steel.pas
    ;;
  qnx-x86)
    fpc -Tqnx -Pi386 -Cp386 -O1 -Os steel.pas
    ;;
  plan9-386)
    fpc -Tplan9 -Pi386 -Cp386 -O1 -Os steel.pas
    ;;
  aix-ppc)
    fpc -Taix -Ppowerpc -O1 -Os steel.pas
    ;;
  irix-mips)
    fpc -Tirix -Pmips -O1 -Os steel.pas
    ;;
  ultrix-mips)
    fpc -Tultrix -Pmips -O1 -Os steel.pas
    ;;
  m88k)
    fpc -Tlinux -Pm88k -O1 -Os steel.pas
    ;;
  s390)
    fpc -Tlinux -Ps390 -O1 -Os steel.pas
    ;;
  sh4)
    fpc -Tlinux -Psh4 -O1 -Os steel.pas
    ;;
  alpha)
    fpc -Tlinux -Palpha -O1 -Os steel.pas
    ;;
  hppa)
    fpc -Tlinux -Phppa -O1 -Os steel.pas
    ;;
  vax)
    fpc -Tvms -Pvax -O1 -Os steel.pas
    ;;
  *)
    echo "error: unknown preset: $PRESET"
    usage
    exit 1
    ;;
esac
