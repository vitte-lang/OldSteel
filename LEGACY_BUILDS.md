# Legacy CPU Build Presets

This cheat sheet maps older CPU families to reasonable FreePascal flags and
environment normalization. These presets prioritize compatibility and size.

## x86 (DOS, 386/486/Pentium)

- **8086/8088/80186/80286 (DOS)**  
  `-Tgo32v2 -Pi386 -CX -Xs -O1 -Os`
- **386**  
  `-Tlinux -Pi386 -Cp386 -O1 -CfSOFT -Os`
- **i386 (alias)**  
  `-Tlinux -Pi386 -Cp386 -O1 -CfSOFT -Os`
- **486**  
  `-Tlinux -Pi386 -Cp486 -O1 -CfSOFT -Os`
- **Pentium**  
  `-Tlinux -Pi386 -CpPENTIUM -O2 -XX -Os`
- **Pentium MMX**  
  `-Tlinux -Pi386 -CpPENTIUMMMX -O2 -XX -Os`
- **Pentium II/III**  
  `-Tlinux -Pi386 -CpPENTIUM2` / `-CpPENTIUM3`
- **Pentium 4**  
  `-Tlinux -Pi386 -CpPENTIUM4 -O2 -XX -Os`
- **Pentium MMX**  
  `-Tlinux -Pi386 -CpPENTIUMMMX -O2 -XX -Os`
- **AMD K6 / K6-2**  
  `-Tlinux -Pi386 -CpK6 -O2 -XX -Os` / `-CpK6_2`
- **AMD K6-III**  
  `-Tlinux -Pi386 -CpK6_3 -O2 -XX -Os`
- **Athlon (K7)**  
  `-Tlinux -Pi386 -CpATHLON -O2 -XX -Os`
- **Athlon XP**  
  `-Tlinux -Pi386 -CpATHLONXP -O2 -XX -Os`
- **Duron / Sempron**  
  `-Tlinux -Pi386 -CpATHLON -O2 -XX -Os`
- **Pentium M**  
  `-Tlinux -Pi386 -CpPENTIUMM -O2 -XX -Os`
- **Atom (early)**  
  use `-CpPENTIUMM` if `-CpATOM` is not available in your FPC build
- **VIA C3**  
  `-Tlinux -Pi386 -CpK6_2 -O2 -XX -Os`
- **IDT WinChip**  
  `-Tlinux -Pi386 -CpPENTIUM -O2 -XX -Os`

## PowerPC (classic)

- **PPC 603/604/G3**  
  `-Tlinux -Ppowerpc -Cppc603 -O1 -Os`
- **PPC G4/G5**  
  `-Tlinux -Ppowerpc -Cppc7400` / `-Cppc970`
- **PPC 32/64 aliases**  
  `-Tlinux -Ppowerpc -O1 -Os` / `-Ppowerpc64`

## m68k

- **68020/030/040**  
  `-Tmorphos -Pmc68020 -O1 -Os`
- **68060**  
  `-Tmorphos -Pmc68060 -O1 -Os`
- **AmigaOS m68k**  
  `-Tamiga -Pmc68020 -O1 -Os`

## SPARC

- **SPARC v8**  
  `-Tlinux -Psparc -CpSPARCv8 -O1 -Os`
- **SPARC v9**  
  `-Tlinux -Psparc64 -CpSPARCv9 -O1 -Os`
- **SPARC Ultra**  
  `-Tlinux -Psparc64 -CpSPARCv9 -O1 -Os`

## MIPS

- **MIPS32**  
  `-Tlinux -Pmips -O1 -Os`
- **MIPS64**  
  `-Tlinux -Pmips64 -O1 -Os`
- **MIPS R4000/R5000**  
  `-Tlinux -Pmips -O1 -Os`
- **MIPSEL/MIPSEB**  
  `-Tlinux -Pmipsel -O1 -Os` / `-Pmips -O1 -Os`

## Alpha / PA-RISC / VAX (if toolchain available)

- **Alpha**  
  `-Tlinux -Palpha -O1 -Os`
- **Alpha EV4/EV5/EV6**  
  `-Tlinux -Palpha -O1 -Os`
- **PA-RISC (HPPA)**  
  `-Tlinux -Phppa -O1 -Os`
- **PA-RISC 2.0**  
  `-Tlinux -Phppa -O1 -Os`
- **VAX**  
  `-Tvms -Pvax -O1 -Os`

## Other Legacy CPUs (if toolchain available)

- **m88k**  
  `-Tlinux -Pm88k -O1 -Os`
- **s390 / s390x**  
  `-Tlinux -Ps390 -O1 -Os`
- **SH4**  
  `-Tlinux -Psh4 -O1 -Os`
- **ARMv5/ARMv7**  
  `-Tlinux -Parm -CpARMV5 -O1 -Os` / `-CpARMV7`
- **ARMv4/ARMv6**  
  `-Tlinux -Parm -CpARMV4T -O1 -Os` / `-CpARMV6`
- **RISC-V 32/64**  
  `-Tlinux -Priscv32 -O1 -Os` / `-Priscv64 -O1 -Os`
- **ARC**  
  `-Tlinux -Parc -O1 -Os`
- **ARM64 (example/alias)**  
  `-Tlinux -Paarch64 -O2 -Os`

## Legacy OS Targets (if toolchain available)

- **OS/2**  
  `-Tos2 -Pi386 -Cp386 -O1 -Os`
- **BeOS**  
  `-Tbeos -Pi386 -Cp486 -O1 -Os`
- **QNX**  
  `-Tqnx -Pi386 -Cp386 -O1 -Os`
- **Plan 9**  
  `-Tplan9 -Pi386 -Cp386 -O1 -Os`
- **AIX**  
  `-Taix -Ppowerpc -O1 -Os`
- **IRIX**  
  `-Tirix -Pmips -O1 -Os`
- **Ultrix**  
  `-Tultrix -Pmips -O1 -Os`
- **Xenix / Minix**  
  target availability varies; use `-Pi386 -Cp386 -O1 -Os`

## Notes

- Prefer `-Os` for small binaries on memory-constrained systems.
- Avoid `-O3` or aggressive CPU-specific flags when targeting unknown hardware.
- Ensure the cross-toolchain for the target is installed and configured.
- For PPC64/ARM/MIPS variants, use the matching preset in `build_legacy.sh`.

## Preset to Profile Map

| Preset | Profile | Notes |
| --- | --- | --- |
| dos-8086 | legacy-386 | DOS 16-bit (GO32v2) |
| x86-386 | legacy-386 | i386 baseline |
| x86-486 | legacy-386 | i486 baseline |
| x86-pentium | legacy-pentium | P5 |
| x86-pentium-mmx | legacy-pentium | MMX |
| x86-pentium2 | legacy-pentium | P6 |
| x86-pentium3 | legacy-pentium | P6 |
| x86-pentium4 | legacy-pentium | NetBurst |
| x86-k6 | legacy-k6 | AMD K6 |
| x86-k6-2 | legacy-k6 | AMD K6-2 |
| x86-k6-3 | legacy-k6-3 | AMD K6-III |
| x86-athlon | legacy-athlon-xp | AMD K7 |
| x86-athlon-xp | legacy-athlon-xp | AMD K7 |
| x86-duron | legacy-duron | AMD Duron |
| x86-sempron | legacy-sempron | AMD Sempron |
| x86-c3 | legacy-c3 | VIA C3 |
| x86-winchip | legacy-winchip | IDT WinChip |
| x86-pentium-m | legacy-pentium-m | Pentium M |
| ppc603 | legacy-ppc | PPC 603 |
| ppc7400 | legacy-ppc | PPC G4 |
| ppc970 | legacy-ppc | PPC G5 |
| ppc32 | legacy-ppc | PPC32 |
| ppc64 | legacy-ppc64 | PPC64 |
| m68k-020 | legacy-m68k | 68020 |
| m68k-060 | legacy-m68k-060 | 68060 |
| m68k-amiga | legacy-amiga | AmigaOS |
| sparc-v8 | legacy-sparc | SPARC v8 |
| sparc-v9 | legacy-sparc64 | SPARC v9 |
| sparc64 | legacy-sparc64 | SPARC64 |
| sparc-ultra | legacy-sparc-ultra | UltraSPARC |
| mips32 | legacy-mips | MIPS32 |
| mips64 | legacy-mips64 | MIPS64 |
| mipsel | legacy-mipsel | MIPSEL |
| mipseb | legacy-mipseb | MIPSEB |
| mips64el | legacy-mips64 | MIPS64EL |
| armv4 | legacy-armv4 | ARMv4 |
| armv5 | legacy-armv5 | ARMv5 |
| armv6 | legacy-armv6 | ARMv6 |
| armv7 | legacy-armv7 | ARMv7 |
| riscv32 | legacy-riscv32 | RISC-V 32 |
| riscv64 | legacy-riscv64 | RISC-V 64 |
| arc | legacy-arc | ARC |
| os2-386 | legacy-os2 | OS/2 |
| beos-x86 | legacy-beos | BeOS |
| qnx-x86 | legacy-qnx | QNX |
| plan9-386 | legacy-plan9 | Plan 9 |
| aix-ppc | legacy-aix | AIX |
| irix-mips | legacy-irix | IRIX |
| ultrix-mips | legacy-ultrix | Ultrix |
| alpha | legacy-alpha | Alpha |
| hppa | legacy-hppa | PA-RISC |
| vax | legacy-vax | VAX/VMS |
| m88k | legacy-m88k | m88k |
| s390 | legacy-s390 | s390 |
| sh4 | legacy-sh4 | SH-4 |
