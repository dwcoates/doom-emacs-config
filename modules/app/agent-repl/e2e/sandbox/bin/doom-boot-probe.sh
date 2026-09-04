#!/usr/bin/env bash
# Boot the sandbox's Doom profile in a REAL tty frame and print the readiness
# stamp it writes.
#
# This is the image's own answer to the question the Emacs layer's `awaitDoom`
# asks: does INTERACTIVE Doom actually come up in this image? It exists
# because bookworm's Emacs 28.2 could not -- the pinned Doom's `early-init.el`
# aborts an interactive session below 29.1 -- and the source-built Emacs 30.2
# is the fix. It replicates exactly what `e2e/emacs_test.go` StartEmacs does
# (staged ~/.emacs.d under a scratch HOME, a pty from `script(1)`, TERM that
# can position a cursor) minus the daemon, so it can be run on its own:
#
#   e2e-sandbox.sh run --dir e2e/sandbox bin/doom-boot-probe.sh
#
# It exits non-zero, loudly, on a stamp that never lands or that reports a
# failed boot -- there is no "probably fine" path.
set -euo pipefail

log() { printf 'doom-boot-probe: %s\n' "$*" >&2; }
die() { log "$*"; exit 1; }

root=$(mktemp -d /tmp/doom-boot-probe.XXXXXX)
stamp=$root/doom-ready.json
socket=$root/server
settings=$root/e2e-settings.el
emacsdir=$root/.emacs.d
image_emacsdir=${IMAGE_EMACSDIR:-/sandbox/emacs.d}
# A small multiple of the observed healthy boot, not a round guess.
bound=${DOOM_BOOT_BOUND_SECONDS:-60}

[[ -d $image_emacsdir ]] || die "no Doom install at $image_emacsdir"
command -v script >/dev/null || die "script(1) is missing; a tty frame cannot be allocated"

# ~/.emacs.d, staged the way the Go layer stages it: sources symlinked (the
# image layer is read-only), `.local` copied so Doom's startup writes land in
# this scratch, `straight/` symlinked for size.
mkdir -p "$emacsdir"
for entry in "$image_emacsdir"/* "$image_emacsdir"/.[!.]*; do
  [[ -e $entry ]] || continue
  name=${entry##*/}
  if [[ $name != .local ]]; then
    ln -s "$entry" "$emacsdir/$name"
    continue
  fi
  mkdir -p "$emacsdir/.local"
  for l in "$entry"/*; do
    [[ -e $l ]] || continue
    lname=${l##*/}
    if [[ $lname == straight ]]; then ln -s "$l" "$emacsdir/.local/straight"; else cp -a "$l" "$emacsdir/.local/$lname"; fi
  done
done

# The settings file sandbox/doom/init.el loads before any module config.el.
# Cold start OFF: this probe is about Doom booting, not about the daemon.
printf '%s\n' \
  '(setq agent-repl-frontend-auto-start nil)' \
  '(provide (quote agent-repl-e2e-settings))' > "$settings"

env_args=(
  "HOME=$root"
  "EMACSDIR=$emacsdir"
  AGENT_REPL_E2E_EMACS=1
  "AGENT_REPL_E2E_SERVER=$socket"
  "AGENT_REPL_E2E_READY=$stamp"
  "AGENT_REPL_E2E_SETTINGS=$settings"
  "AGENT_REPL_STATE_DIR=$root/state"
  AGENT_REPL_FORBID_VENDOR_CALLS=1
  TERM=xterm-256color
)

ptylog=$root/pty.log
log "booting emacs -nw on a pty under $root"
script -q -c "env ${env_args[*]} emacs -nw" /dev/null > "$ptylog" 2>&1 &
emacs_pid=$!

deadline=$((SECONDS + bound))
while (( SECONDS < deadline )); do
  [[ -s $stamp ]] && break
  kill -0 "$emacs_pid" 2>/dev/null || break
  sleep 0.5
done

if [[ ! -s $stamp ]]; then
  kill "$emacs_pid" 2>/dev/null || true
  log "NO readiness stamp after ${bound}s; pty output follows:"
  cat "$ptylog" >&2
  exit 1
fi

cat "$stamp"; echo
ok=$(emacs -Q --batch --eval "(progn (require 'json) (princ (if (eq t (cdr (assq 'ok (json-read-file \"$stamp\")))) \"t\" \"nil\")))")

kill "$emacs_pid" 2>/dev/null || true
wait "$emacs_pid" 2>/dev/null || true

[[ $ok == t ]] || { log "the stamp reports a FAILED boot; pty output follows:"; cat "$ptylog" >&2; exit 1; }
log "interactive Doom booted and published its readiness stamp"
