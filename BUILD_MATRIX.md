# Build Matrix (CPU x OS)

This document lists every legacy build preset and the exact FreePascal flags used.
Use these with `sh build_legacy.sh <preset>`.

| Preset | Target OS | CPU | Extra Flags | Notes |
| --- | --- | --- | --- | --- |
| dos-8086 | `-Tgo32v2` | `-Pi386` | `-CX -Xs -O1 -Os` | DOS 16-bit (GO32v2 toolchain) |
| i386 | `-Tlinux` | `-Pi386 -Cp386` | `-O1 -CfSOFT -Os` | i386 alias |
| x86-386 | `-Tlinux` | `-Pi386 -Cp386` | `-O1 -CfSOFT -Os` | i386 baseline |
| x86-486 | `-Tlinux` | `-Pi386 -Cp486` | `-O1 -CfSOFT -Os` | i486 baseline |
| x86-pentium | `-Tlinux` | `-Pi386 -CpPENTIUM` | `-O2 -XX -Os` | Pentium (P5) |
| x86-pentium-mmx | `-Tlinux` | `-Pi386 -CpPENTIUMMMX` | `-O2 -XX -Os` | Pentium MMX |
| x86-pentium2 | `-Tlinux` | `-Pi386 -CpPENTIUM2` | `-O2 -XX -Os` | Pentium II |
| x86-pentium3 | `-Tlinux` | `-Pi386 -CpPENTIUM3` | `-O2 -XX -Os` | Pentium III |
| x86-pentium4 | `-Tlinux` | `-Pi386 -CpPENTIUM4` | `-O2 -XX -Os` | Pentium 4 |
| x86-pentium-m | `-Tlinux` | `-Pi386 -CpPENTIUMM` | `-O2 -XX -Os` | Pentium M |
| x86-k6 | `-Tlinux` | `-Pi386 -CpK6` | `-O2 -XX -Os` | AMD K6 |
| x86-k6-2 | `-Tlinux` | `-Pi386 -CpK6_2` | `-O2 -XX -Os` | AMD K6-2 |
| x86-k6-3 | `-Tlinux` | `-Pi386 -CpK6_3` | `-O2 -XX -Os` | AMD K6-III |
| x86-athlon | `-Tlinux` | `-Pi386 -CpATHLON` | `-O2 -XX -Os` | AMD K7 |
| x86-athlon-xp | `-Tlinux` | `-Pi386 -CpATHLONXP` | `-O2 -XX -Os` | Athlon XP |
| x86-duron | `-Tlinux` | `-Pi386 -CpATHLON` | `-O2 -XX -Os` | Duron |
| x86-sempron | `-Tlinux` | `-Pi386 -CpATHLON` | `-O2 -XX -Os` | Sempron |
| x86-c3 | `-Tlinux` | `-Pi386 -CpK6_2` | `-O2 -XX -Os` | VIA C3 |
| x86-winchip | `-Tlinux` | `-Pi386 -CpPENTIUM` | `-O2 -XX -Os` | IDT WinChip |
| ppc603 | `-Tlinux` | `-Ppowerpc -Cppc603` | `-O1 -Os` | PPC 603 |
| ppc7400 | `-Tlinux` | `-Ppowerpc -Cppc7400` | `-O1 -Os` | PPC G4 |
| ppc970 | `-Tlinux` | `-Ppowerpc -Cppc970` | `-O1 -Os` | PPC G5 |
| ppc32 | `-Tlinux` | `-Ppowerpc` | `-O1 -Os` | PPC 32-bit |
| ppc64 | `-Tlinux` | `-Ppowerpc64` | `-O1 -Os` | PPC 64-bit |
| m68k-020 | `-Tmorphos` | `-Pmc68020` | `-O1 -Os` | 68020 |
| m68k-060 | `-Tmorphos` | `-Pmc68060` | `-O1 -Os` | 68060 |
| m68k-amiga | `-Tamiga` | `-Pmc68020` | `-O1 -Os` | AmigaOS |
| sparc-v8 | `-Tlinux` | `-Psparc -CpSPARCv8` | `-O1 -Os` | SPARC v8 |
| sparc-v9 | `-Tlinux` | `-Psparc64 -CpSPARCv9` | `-O1 -Os` | SPARC v9 |
| sparc64 | `-Tlinux` | `-Psparc64 -CpSPARCv9` | `-O1 -Os` | SPARC64 |
| sparc-ultra | `-Tlinux` | `-Psparc64 -CpSPARCv9` | `-O1 -Os` | UltraSPARC |
| mips32 | `-Tlinux` | `-Pmips` | `-O1 -Os` | MIPS32 |
| mips64 | `-Tlinux` | `-Pmips64` | `-O1 -Os` | MIPS64 |
| mipsel | `-Tlinux` | `-Pmipsel` | `-O1 -Os` | MIPSEL |
| mipseb | `-Tlinux` | `-Pmips` | `-O1 -Os` | MIPSEB |
| mips64el | `-Tlinux` | `-Pmips64` | `-O1 -Os` | MIPS64EL |
| armv4 | `-Tlinux` | `-Parm -CpARMV4T` | `-O1 -Os` | ARMv4 |
| armv5 | `-Tlinux` | `-Parm -CpARMV5` | `-O1 -Os` | ARMv5 |
| armv6 | `-Tlinux` | `-Parm -CpARMV6` | `-O1 -Os` | ARMv6 |
| armv7 | `-Tlinux` | `-Parm -CpARMV7` | `-O1 -Os` | ARMv7 |
| arm64-tahoe | `-Tlinux` | `-Paarch64` | `-O2 -Os` | ARM64 example |
| arm64 | `-Tlinux` | `-Paarch64` | `-O2 -Os` | ARM64 alias |
| riscv32 | `-Tlinux` | `-Priscv32` | `-O1 -Os` | RISC-V 32 |
| riscv64 | `-Tlinux` | `-Priscv64` | `-O1 -Os` | RISC-V 64 |
| arc | `-Tlinux` | `-Parc` | `-O1 -Os` | ARC |
| os2-386 | `-Tos2` | `-Pi386 -Cp386` | `-O1 -Os` | OS/2 |
| beos-x86 | `-Tbeos` | `-Pi386 -Cp486` | `-O1 -Os` | BeOS |
| qnx-x86 | `-Tqnx` | `-Pi386 -Cp386` | `-O1 -Os` | QNX |
| plan9-386 | `-Tplan9` | `-Pi386 -Cp386` | `-O1 -Os` | Plan 9 |
| aix-ppc | `-Taix` | `-Ppowerpc` | `-O1 -Os` | AIX |
| irix-mips | `-Tirix` | `-Pmips` | `-O1 -Os` | IRIX |
| ultrix-mips | `-Tultrix` | `-Pmips` | `-O1 -Os` | Ultrix |
| alpha | `-Tlinux` | `-Palpha` | `-O1 -Os` | Alpha |
| hppa | `-Tlinux` | `-Phppa` | `-O1 -Os` | PA-RISC |
| vax | `-Tvms` | `-Pvax` | `-O1 -Os` | VAX/VMS |
| m88k | `-Tlinux` | `-Pm88k` | `-O1 -Os` | m88k |
| s390 | `-Tlinux` | `-Ps390` | `-O1 -Os` | s390 |
| sh4 | `-Tlinux` | `-Psh4` | `-O1 -Os` | SH-4 |
