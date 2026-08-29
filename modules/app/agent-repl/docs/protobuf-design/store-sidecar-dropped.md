# Store + sidecar — instructions deliberately NOT carried into
# docs/overhaul/store.md / sidecar.md

Final-audit triage, 2026-08-29. Absence is silence, not prohibition.

- The store's SQLite pragma set (WAL, busy_timeout, synchronous NORMAL,
  txlock immediate) AND the commit-then-publish ordering mutex
  (publication order matches commit order; added after two production
  session kills): deliberately NOT prescribed — the one-transaction
  WriteBatch rule is the whole written contract. NOTE: this is the
  highest-teeth silent drop in this file; the incident class it guarded
  is real.
- Slow-query observability (per-statement-family thresholded warns, env
  knob): dropped; the general logging discipline covers pathology work.
- Spool owner-resolution rules 2–4 (exact-path evidence outranks
  task-id; filename similarity is never evidence; conflicting claims
  refuse attribution): not written; only the /tmp symlink normalization
  (rule 1) was carried.
- The bounded-read loss statements (oversized-carry byte drop on resync;
  the 64KiB unparsed-evidence truncation): the bounds and their
  loss-is-stated policy were left unwritten.
- The transcript LINE taxonomy (known-metadata → vendor_specific; novel
  → unknown; system/attachment subtype routing): not written; the
  residue arms + total-ingestion mandate are the whole written guidance.
- Tail rotation/truncation reset behavior (dev:inode change or shrink →
  cursor reset to 0, re-read, dedup absorbs; truncation is a known
  loss): not written.
- The fsnotify latency path with periodic-scan backstop: only the
  multi-root requirement was carried; the notification mechanism is the
  implementer's.
- The watch-stream slow-consumer DISCONNECT policy details: only the
  raise-the-bound guidance was carried; the disconnect-vs-block policy
  is the lead's.
