#!/usr/bin/env bash
# install-gippity.sh — build Gippity.app: a launcher that brings Emacs to the
# front on agent-repl's Chats, opening the most recent chat thread.
#
#   bin/install-gippity.sh [--icon ICNS] [DEST]
#
# DEST defaults to /Applications/Gippity.app and is REPLACED. The app only
# runs `open -a Emacs` on master's launch file (launch/open-chats.agent-repl-
# chats); macOS holds that open until Emacs takes it, so a launch while
# Emacs is starting is not lost, and threads.el's launch mode opens the
# thread. --icon gives the app ICNS as its icon.
set -euo pipefail

icon=""
if [[ "${1:-}" == "--icon" ]]; then
  icon="$2"
  shift 2
fi
dest="${1:-/Applications/Gippity.app}"
launch_file="$HOME/.config/doom/modules/app/agent-repl/launch/open-chats.agent-repl-chats"

if [[ -n "$icon" && ! -f "$icon" ]]; then
  echo "install-gippity: no icon at $icon" >&2
  exit 1
fi

work="$(mktemp -d "${TMPDIR:-/tmp}/gippity.XXXXXX")"
trap 'rm -rf "$work"' EXIT
script="$work/gippity.applescript"
cat > "$script" <<APPLESCRIPT
do shell script "open -a Emacs " & quoted form of "$launch_file"
APPLESCRIPT

rm -rf "$dest"
osacompile -o "$dest" "$script"
if [[ -n "$icon" ]]; then
  # The compiled asset catalog's icon outranks applet.icns, so it goes.
  rm -f "$dest/Contents/Resources/Assets.car"
  plist="$dest/Contents/Info.plist"
  if /usr/libexec/PlistBuddy -c 'Print :CFBundleIconName' "$plist" >/dev/null 2>&1; then
    /usr/libexec/PlistBuddy -c 'Delete :CFBundleIconName' "$plist"
  fi
  cp "$icon" "$dest/Contents/Resources/applet.icns"
  # Editing the bundle broke osacompile's signature; sign it again.
  codesign --force --sign - "$dest"
  touch "$dest"
fi
echo "install-gippity: built $dest, opening $launch_file"
