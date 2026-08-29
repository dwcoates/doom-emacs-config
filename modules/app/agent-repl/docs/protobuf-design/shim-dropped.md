# Shim — instructions deliberately NOT carried into docs/overhaul/shim.md

Final-audit triage, 2026-08-29. Absence is silence, not prohibition,
unless marked FORBIDDEN.

- The vendor replay-gating gotcha (replayed tool results skipped; task
  notifications uuid-deduped BEFORE the replay guard, or detached work
  never settles): deliberately NOT written down — store-side write_id
  dedup absorbs the duplicate class; the asymmetry is left for the
  implementer to rediscover if it bites.
- The shim-side prompt-cache hit-rate warning (<80% whole-tree scope):
  dropped; token judgment is the daemon's.
- --claude-bin (driving the user's system claude for version parity):
  FORBIDDEN by the bundled-only ruling (the pinned SDK binary is the one
  driven) — this one IS a prohibition, stated in shim.md.
- (Prohibition audit, later 2026-08-29) STRIPPED to silence: the
  lock-acquisition loud-failure/refuse-to-start sentences, and the
  nothing-parses-the-login-TUI rule (the raw-bytes design implies it).
