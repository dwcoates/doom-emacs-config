#!/usr/bin/env bash
# Builds agent-repl's Wi-Fi helper app (main.swift, standing.swift) into OUT (default
# ~/.cache/agent-repl/AgentReplWifi.app), signed ad hoc so macOS can hold its
# Location Services permission against a stable identity.
#
# THE APP CARRIES THE HASH OF ITS SOURCES (Contents/Resources/source-sha256,
# source_hash below), which the installer's app_build check compares with the
# checkout's: a rebuild re-signs the app and macOS may ask for the permission
# again, so the installer rebuilds only when the sources changed.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"

# source_hash DIR: the one hash of the helper's sources, the installer's
# (bin/requirements.sh, check_app_build) and the build's alike.
source_hash() {
  cat "$1/main.swift" "$1/standing.swift" "$1/Info.plist" | shasum -a 256 | cut -d' ' -f1
}
if [ "${1:-}" = --source-hash ]; then
  source_hash "$here"
  exit 0
fi
out="${1:-$HOME/.cache/agent-repl/AgentReplWifi.app}"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/AgentReplWifi.app/Contents/MacOS"
cp "$here/Info.plist" "$work/AgentReplWifi.app/Contents/Info.plist"
swiftc -O -swift-version 5 -o "$work/AgentReplWifi.app/Contents/MacOS/agent-repl-wifi" "$here/main.swift" "$here/standing.swift" \
  -framework CoreWLAN -framework CoreLocation
mkdir -p "$work/AgentReplWifi.app/Contents/Resources"
source_hash "$here" >"$work/AgentReplWifi.app/Contents/Resources/source-sha256"
codesign --force --sign - --identifier com.agentrepl.wifi "$work/AgentReplWifi.app"
mkdir -p "$(dirname "$out")"
rm -rf "$out.new"
mv "$work/AgentReplWifi.app" "$out.new"
rm -rf "$out"
mv "$out.new" "$out"
echo "$out"
