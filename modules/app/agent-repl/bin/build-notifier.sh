#!/usr/bin/env bash
# build-notifier.sh — build Zettel.app, agent-repl's macOS desktop notifier
# (notifier/), into a complete, ad-hoc-signed bundle at OUT (default
# ~/.cache/agent-repl/bin/Zettel.app), then, unless --no-install, install it
# at ~/Applications/Zettel.app, register it with LaunchServices, and restart
# its launchd agent (com.agentrepl.notifier) onto the new build.
#
# THE BUNDLE IS ASSEMBLED, NEVER COMMITTED: the executable is SwiftPM's
# release build, the Info.plist is notifier/Bundle/Info.plist, and the icon is
# rendered from source by the ZettelIcon target at every size iconutil packs.
#
# INSTALLING REPLACES THE WHOLE BUNDLE BY RENAME, so a click on a banner never
# finds a half-copied app: the new bundle is copied beside the old one, and
# the two are swapped by mv. ~/Applications is where Spotlight and Finder
# find it.
#
# Usage: bin/build-notifier.sh [--out BUNDLE] [--no-install]
#
# Test seams: AGENT_REPL_APPLICATIONS_DIR (default ~/Applications),
# AGENT_REPL_LAUNCHCTL (default launchctl), AGENT_REPL_LSREGISTER.

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd -P)"
NOTIFIER="$ROOT/notifier"
OUT="${HOME}/.cache/agent-repl/bin/Zettel.app"
INSTALL=1
while [ $# -gt 0 ]; do
    case "$1" in
        --out) OUT="$2"; shift 2 ;;
        --no-install) INSTALL=0; shift ;;
        *) echo "build-notifier.sh: unknown argument $1" >&2; exit 2 ;;
    esac
done
APPS="${AGENT_REPL_APPLICATIONS_DIR:-$HOME/Applications}"
LAUNCHCTL="${AGENT_REPL_LAUNCHCTL:-launchctl}"
LSREGISTER="${AGENT_REPL_LSREGISTER:-/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister}"
LABEL="com.agentrepl.notifier"

echo "[build-notifier] building the release products..."
( cd "$NOTIFIER" && swift build -c release --product Zettel && swift build -c release --product ZettelIcon )
BIN="$(cd "$NOTIFIER" && swift build -c release --show-bin-path)"

WORK="$(mktemp -d "${TMPDIR:-/tmp}/zettel-bundle.XXXXXX")"
trap 'rm -rf "$WORK"' EXIT
STAGE="$WORK/Zettel.app"
mkdir -p "$STAGE/Contents/MacOS" "$STAGE/Contents/Resources" "$WORK/AppIcon.iconset"
cp "$NOTIFIER/Bundle/Info.plist" "$STAGE/Contents/Info.plist"
cp "$BIN/Zettel" "$STAGE/Contents/MacOS/Zettel"
for px in 16 32 128 256 512; do
    "$BIN/ZettelIcon" "$px" "$WORK/AppIcon.iconset/icon_${px}x${px}.png"
    "$BIN/ZettelIcon" "$((px * 2))" "$WORK/AppIcon.iconset/icon_${px}x${px}@2x.png"
done
iconutil -c icns "$WORK/AppIcon.iconset" -o "$STAGE/Contents/Resources/AppIcon.icns"
codesign --force --sign - "$STAGE"

mkdir -p "$(dirname "$OUT")"
rm -rf "$OUT.new"
cp -R "$STAGE" "$OUT.new"
rm -rf "$OUT"
mv "$OUT.new" "$OUT"
echo "[build-notifier] built $OUT"

[ "$INSTALL" -eq 1 ] || exit 0
DEST="$APPS/Zettel.app"
mkdir -p "$APPS"
rm -rf "$DEST.new" "$DEST.old"
cp -R "$OUT" "$DEST.new"
[ ! -e "$DEST" ] || mv "$DEST" "$DEST.old"
mv "$DEST.new" "$DEST"
rm -rf "$DEST.old"
"$LSREGISTER" -f "$DEST"
echo "[build-notifier] installed $DEST"
uid="$(id -u)"
if "$LAUNCHCTL" print "gui/$uid/$LABEL" >/dev/null 2>&1; then
    "$LAUNCHCTL" kickstart -k "gui/$uid/$LABEL"
    echo "[build-notifier] restarted $LABEL onto the new build"
fi
