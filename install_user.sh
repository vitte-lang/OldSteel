#!/bin/sh
set -eu

cd "$(dirname "$0")"

fpc steel.pas

dest=""
for p in /usr/local/bin /opt/homebrew/bin "$HOME/.local/bin"; do
  if [ -d "$p" ] && [ -w "$p" ]; then
    dest="$p"
    break
  fi
done

if [ -z "$dest" ]; then
  dest="$HOME/.local/bin"
  mkdir -p "$dest"
fi

cp ./steel "$dest/steel"
chmod 755 "$dest/steel"

echo "installed: $dest/steel"
echo "try: steel --help"
