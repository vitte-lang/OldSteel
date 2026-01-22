#!/bin/sh
set -eu

cd "$(dirname "$0")"

usage() {
  cat <<'EOF'
Usage: steel_tool.sh <command> [args]

Commands:
  build                 Compile steel.pas
  install               Build and install to a user-writable PATH dir
  native                Auto-build for current system
  legacy <preset>       Build with a legacy preset (see build_legacy.sh list)
  test                  Run the full verification suite

Examples:
  sh steel_tool.sh build
  sh steel_tool.sh native
  sh steel_tool.sh legacy x86-386
  sh steel_tool.sh install
  sh steel_tool.sh test
EOF
}

if [ $# -lt 1 ]; then
  usage
  exit 1
fi

cmd="$1"
shift

case "$cmd" in
  build)
    fpc steel.pas
    ;;
  install)
    fpc steel.pas
    dest=""
    os="$(uname -s | tr '[:upper:]' '[:lower:]')"
    distro=""
    if [ -f /etc/os-release ]; then
      distro="$(. /etc/os-release && echo "$ID")"
    fi
    if [ "$os" = "darwin" ]; then
      for p in /opt/homebrew/bin /usr/local/bin "$HOME/.local/bin"; do
        if [ -d "$p" ] && [ -w "$p" ]; then
          dest="$p"
          break
        fi
      done
    elif [ "$os" = "linux" ]; then
      case "$distro" in
        ubuntu|debian|linuxmint) ;;
        fedora|rhel|centos|rocky|almalinux) ;;
        arch|manjaro) ;;
        nixos) ;;
        *) ;;
      esac
      for p in /usr/local/bin "$HOME/.local/bin"; do
        if [ -d "$p" ] && [ -w "$p" ]; then
          dest="$p"
          break
        fi
      done
    elif [ "$os" = "freebsd" ] || [ "$os" = "openbsd" ] || [ "$os" = "netbsd" ] || [ "$os" = "dragonfly" ]; then
      for p in /usr/local/bin /usr/pkg/bin "$HOME/.local/bin"; do
        if [ -d "$p" ] && [ -w "$p" ]; then
          dest="$p"
          break
        fi
      done
    elif [ "$os" = "sunos" ] || [ "$os" = "solaris" ]; then
      for p in /usr/local/bin /opt/local/bin "$HOME/.local/bin"; do
        if [ -d "$p" ] && [ -w "$p" ]; then
          dest="$p"
          break
        fi
      done
    elif [ "$os" = "aix" ] || [ "$os" = "hpux" ] || [ "$os" = "irix" ]; then
      for p in /usr/local/bin "$HOME/.local/bin"; do
        if [ -d "$p" ] && [ -w "$p" ]; then
          dest="$p"
          break
        fi
      done
    elif [ "$os" = "cygwin" ] || [ "$os" = "mingw" ] || [ "$os" = "msys" ]; then
      for p in /usr/local/bin "$HOME/.local/bin"; do
        if [ -d "$p" ] && [ -w "$p" ]; then
          dest="$p"
          break
        fi
      done
    else
      for p in "$HOME/.local/bin" /usr/local/bin; do
        if [ -d "$p" ] && [ -w "$p" ]; then
          dest="$p"
          break
        fi
      done
    fi
    if [ -z "$dest" ]; then
      dest="$HOME/.local/bin"
      mkdir -p "$dest"
    fi
    cp ./steel "$dest/steel"
    chmod 755 "$dest/steel"
    echo "installed: $dest/steel"
    echo "try: steel --help"
    ;;
  native)
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
    ;;
  legacy)
    if [ $# -ne 1 ]; then
      echo "error: legacy requires a preset"
      usage
      exit 1
    fi
    sh ./build_legacy.sh "$1"
    ;;
  test)
    sh ./verify_smoke.sh
    ;;
  *)
    usage
    exit 1
    ;;
esac
