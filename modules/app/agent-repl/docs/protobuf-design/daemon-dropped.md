# Daemon — instructions deliberately NOT carried into docs/overhaul/daemon.md

Final-audit triage, 2026-08-29. Each item below existed in the old daemon
(or was proposed by an auditor) and was ruled NOT to become a prescription.
Unless a line says FORBIDDEN, absence is silence, not prohibition — the
implementing lead is free to reinvent the mechanism if it proves needed.

- Boot-sweep verdict SURFACING: boot probes' verdicts land in logs only;
  no pushed-state surfacing requirement was carried.
- The three runtime-free CLI subcommands (create-workspace,
  list-workspaces, list-transcripts): dropped; the file ingress and rpcs
  are the entry points.
- USER-FACING REWIND / undo (transcript truncation under a new vendor
  uuid, the lineage trio, three durable columns): dropped entirely; the
  only rewind is the shim-internal keep-alive discard.
- The agent-authored WIDGET surface (cee-web-widget reader, capabilities
  endpoint, chess rendering): dropped; /show-chess-game degrades to text.
- The turn_interruption replay tombstone (store-coordinate-keyed): no
  structural replay-resurrection answer was prescribed; left to the wave.
- The layer-2 wire-version handshake: dropped; protobuf compatibility
  semantics + single-deploy reality cover it.
- The durable receipts trio: standing terminal-failure card with
  idempotent re-fence; pending-resumption receipts for force-killed
  turns; the conversation-identity replay cursor with its
  never-downgrade backfill ladder. All dropped — force-kill surfaces as
  a workspace error and the user re-prompts.
- The boot-observability bundle: GET /healthz out-of-band readiness,
  boot-phase timing marks, listener-bind-before-dependency-boot,
  binary-staleness mtime reporting. None prescribed (DaemonHealth is the
  health surface).
- Post-merge SESSION fate (today: hibernate the merged workspace's
  session) and the merge-terminal exactly-once re-say store: not
  prescribed beyond "resumed or loudly failed".
- Workspace-key canonicalization spelling: not stated; "idempotent by
  dir" plus id-not-path is the whole prescription.
- The additive-DDL schema doctrine and dual-stamp semantics: not carried
  (the fresh-database ruling mooted migration semantics).
- Per-command ack-latency instrumentation (env-thresholded warns): not
  carried.
- The boot sweep of stale merge worktrees: not carried (the residue
  class dies with cherry-pick; no janitor line was added for the new
  landing's residue either).
- Restart-EPOCH exclusion from wall-clock failure bounds: not carried
  (freeness-gating removes most spurious cases).
- The unmodeled-tool warning's LEGIBILITY requirement (an abbreviated
  account of the call's arguments): dropped — name + count only; the raw
  call lives in the store.
- The footer Status×SubStatus coverage review (a pre-freeze gate by
  earlier ruling): WAIVED outright.
- (Prohibition audit, later 2026-08-29) The WSM retention clause (live
  never pruned, capped terminals) was STRIPPED back to silence.
