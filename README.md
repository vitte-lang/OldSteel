# Steel (FreePascal)

This is the FreePascal rewrite of Steel, built with legacy systems in mind. It parses
MUF v4 blocks, follows the CLI contract, and emits `steel.log` with a resolved
workspace/profile/target plus tool/bake/export listings and a raw `[blocks]` dump.

## Build

Use the single helper script for all common tasks:

```sh
sh steel_tool.sh build
```



## Tutorial

This walkthrough starts from zero and ends with a verified run log.

### 1) Build

```sh
sh steel_tool.sh build
```

### 2) Quick run (smoke)

Dry-run the sample config to verify parsing and resolution:

```sh
./steel run steelconf_test --all --dry-run --log /tmp/steel_run.mff
```

### 3) Full verification suite

```sh
sh steel_tool.sh test
```

### 4) Legacy builds (older CPUs)

Use a preset to target an older CPU or OS:

```sh
sh steel_tool.sh legacy x86-386
sh steel_tool.sh legacy ppc603
sh steel_tool.sh legacy m68k-060
```

### 4b) Auto build for current system

```sh
sh steel_tool.sh native
```

### 4a) All legacy build commands

Full list of available presets:

```sh
sh steel_tool.sh legacy dos-8086
sh steel_tool.sh legacy i386
sh steel_tool.sh legacy x86-386
sh steel_tool.sh legacy x86-486
sh steel_tool.sh legacy x86-pentium
sh steel_tool.sh legacy x86-pentium-mmx
sh steel_tool.sh legacy x86-pentium2
sh steel_tool.sh legacy x86-pentium3
sh steel_tool.sh legacy x86-pentium4
sh steel_tool.sh legacy x86-pentium-m
sh steel_tool.sh legacy x86-k6
sh steel_tool.sh legacy x86-k6-2
sh steel_tool.sh legacy x86-k6-3
sh steel_tool.sh legacy x86-athlon
sh steel_tool.sh legacy x86-athlon-xp
sh steel_tool.sh legacy x86-duron
sh steel_tool.sh legacy x86-sempron
sh steel_tool.sh legacy x86-c3
sh steel_tool.sh legacy x86-winchip
sh steel_tool.sh legacy ppc603
sh steel_tool.sh legacy ppc7400
sh steel_tool.sh legacy ppc970
sh steel_tool.sh legacy ppc32
sh steel_tool.sh legacy ppc64
sh steel_tool.sh legacy m68k-020
sh steel_tool.sh legacy m68k-060
sh steel_tool.sh legacy m68k-amiga
sh steel_tool.sh legacy sparc-v8
sh steel_tool.sh legacy sparc-v9
sh steel_tool.sh legacy sparc64
sh steel_tool.sh legacy sparc-ultra
sh steel_tool.sh legacy mips32
sh steel_tool.sh legacy mips64
sh steel_tool.sh legacy mipsel
sh steel_tool.sh legacy mipseb
sh steel_tool.sh legacy mips64el
sh steel_tool.sh legacy armv4
sh steel_tool.sh legacy armv5
sh steel_tool.sh legacy armv6
sh steel_tool.sh legacy armv7
sh steel_tool.sh legacy arm64-tahoe
sh steel_tool.sh legacy arm64
sh steel_tool.sh legacy riscv32
sh steel_tool.sh legacy riscv64
sh steel_tool.sh legacy arc
sh steel_tool.sh legacy os2-386
sh steel_tool.sh legacy beos-x86
sh steel_tool.sh legacy qnx-x86
sh steel_tool.sh legacy plan9-386
sh steel_tool.sh legacy aix-ppc
sh steel_tool.sh legacy irix-mips
sh steel_tool.sh legacy ultrix-mips
sh steel_tool.sh legacy alpha
sh steel_tool.sh legacy hppa
sh steel_tool.sh legacy vax
sh steel_tool.sh legacy m88k
sh steel_tool.sh legacy s390
sh steel_tool.sh legacy sh4
```

For a full OS/CPU matrix with flags, see `pascal/BUILD_MATRIX.md`.
For additional notes and mappings, see `pascal/LEGACY_BUILDS.md`.
## Run

These examples cover the most common CLI paths, from a default build to a full run:

```sh
./steel
./steel steelconf
./steel build steelconf --emit steel.log
./steel check
./steel print --profile release
./steel run --all --log target/run.mff
```

### 5) Run log output (Rust-like)

Generate the Rust-style run log (`steel-runlog-1`) for debugging and audits:

```sh
./steel run steelconf_test --all --log /tmp/steel_run.log
```

The run log uses `steel-runlog-1` format and includes per-bake sources, outputs,
command timings, and stdout/stderr when `MUFFIN_RUN_STDOUT=1` is set.

## Supported subset

Steel (Pascal) supports the following MUF v4 subset:

- `!muf 4` header
- Blocks: `[workspace]`, `[profile <name>]`, `[target <name>]`, `[default]`, `[tool]`, `[bake]`, `[run]`, `[export]`
- Directives: `.set`, `.exec`, `.make`, `.output`, `.takes`, `.emits`, `.ref` (unknown directives are preserved in `[blocks]`)
- Comments starting with `;;`
- Basic `${var}` expansion using workspace/profile/target/`-D` variables
- Overrides: `--profile`, `--target`, `--os`, `--arch`, `-D KEY=VALUE`
- Env defaults: `MUFFIN_PROFILE`, `MUFFIN_TARGET`, `MUFFIN_EMIT`

The emitter writes `mff 1` with `[host]`, `[workspace]`, `[target]`, `[paths]`, plus
`[tools]`, `[bakes]`, `[exports]` and a raw `[blocks]` dump.
