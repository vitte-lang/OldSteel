# Steel (FreePascal)

This is a FreePascal rewrite intended for older systems. It parses MUF v4 blocks,
implements the CLI contract, and emits `steel.log` with resolved workspace/profile/target
plus tool/bake/export listings and a raw `[blocks]` dump.

## Build

```sh
fpc steel.pas
```

## Run

```sh
./steel
./steel steelconf
./steel build steelconf --emit steel.log
./steel check
./steel print --profile release
./steel run --all --log target/run.mff
```

## Supported subset

- `!muf 4` header
- Blocks: `[workspace]`, `[profile <name>]`, `[target <name>]`, `[default]`, `[tool]`, `[bake]`, `[run]`, `[export]`
- Directives: `.set`, `.exec`, `.make`, `.output`, `.takes`, `.emits`, `.ref` (unknown directives are preserved in `[blocks]`)
- Comments starting with `;;`
- Basic `${var}` expansion using workspace/profile/`-D` variables
- Overrides: `--profile`, `--target`, `--os`, `--arch`, `-D KEY=VALUE`
- Env defaults: `MUFFIN_PROFILE`, `MUFFIN_TARGET`, `MUFFIN_EMIT`

The emitter writes `mff 1` with `[host]`, `[workspace]`, `[target]`, `[paths]`, plus
`[tools]`, `[bakes]`, `[exports]` and a raw `[blocks]` dump.
