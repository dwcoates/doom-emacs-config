# Elisp — instructions deliberately NOT carried into docs/overhaul/elisp.md

Final-audit triage, 2026-08-29. The big removals ARE noted in elisp.md's
removals slates (rename, codex, explain-config, readiness/SLO, debug
keymap, hide-project-dirs, completion, webview key affordances, sidebar
nav, output-feed nav, the local roster snapshot, the link banner, the
pre-close round-trip, the installer) — those are informed removals, not
silent drops. Below are the sub-details that got NO doc text at all;
absence is silence, not prohibition.

- The open ladder's specific rungs, the 12s stall threshold, and the Try
  list wording: the ladder is blessed generically; its details are the
  implementer's.
- The ready-fade's ~2s dwell constant and reset conditions: the local
  presentation axis is blessed generically; constants unstated.
- The notification coalescing window, the 60s clickable window, the 10s
  hung-notifier kill, and backend selection: implementation detail
  beneath "post an OS desktop notification".
- The one-shot prompt-history survival across aborts and the C-RET
  append-text variant: one-shots ride the wire now; minibuffer UX is the
  implementer's.
- The webview pool's staggering cadence and pool size: pre-creation is
  blessed generically; numbers unstated.
