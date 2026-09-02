# E2E remediation plan: real sidecar, no hand-written store events

User ruling 2026-09-02: the e2e suite (daemon/e2e) must not write sidecar
events into the store, and must not write vendor JSONL either. The FAKE SDK
is the ONLY writer of vendor files; a REAL shim-sidecar process turns them
into store rows. Where the fake lacks a shape a test needs, the fake grows a
named scenario or option (grounded by a golden, or explicitly marked
ungrounded in the shim's manifest). Remediation is done by a sonnet-medium agent (orchestrated
by the project lead), after the five-way merge lands and before the coverage
hardening step. Nobody edits by hand.

## Facts the plan rests on

- The fake SDK (shim `src/fake/vendor-files.ts`) already writes the real
  vendor tree: `$CLAUDE_CONFIG_DIR/projects/<cwd-slug>/<session>.jsonl` and
  `.../<session>/subagents/agent-<id>.jsonl`, plus bash spools. The sidecar's
  integration harness already spawns the sidecar with `--store-socket
  --config-roots --spool-root --log --poll-interval --rescan-interval` and
  stale-grace flags (shim-sidecar/integration/helpers_test.go).
- daemon/e2e today: 30 files call `sidecar*Event(...)` /
  `ingestTranscriptAsSidecar(...)` and write `protocolv1.Event`s straight to
  the store (heaviest: skillbody 12, mergewindow 11, clearcompact 9,
  machinery 7, slashdurability 6, hibernationharness 5).

## Steps

1. Harness: build the sidecar once per run (beside buildShim/buildShimStore)
   and spawn ONE sidecar per test store with `--config-roots=<the test's
   CLAUDE_CONFIG_DIR>`, `--spool-root=<the test's spool root>`, tight poll and
   rescan intervals (the sidecar harness's values), stderr captured. Skip
   loudly if the sidecar source is absent, like the store. Stop it with the
   store at cleanup; a sidecar exit before cleanup fails the test.
2. Replace every hand-written event with one of two moves, chosen per call:
   a. The fact is produced by a fake-SDK scenario (turns, subagents, bash,
      /clear, /compact, skill bodies): drive the scenario through the shim
      and let the sidecar ingest what the SDK wrote. Preferred.
   b. The fact needs a transcript shape no scenario yields (an ingest-order
      corner, a boundary redelivered, a rewound cursor, a specific compact
      summary, a skill body mid-turn): EXTEND THE FAKE SDK with a named
      scenario or scenario option that writes it, with a shim unit test and
      a grounding note. Tests never touch the config dir or spool root.
3. Waits: assertions poll the daemon's rendered frames (as now) bounded by
   the harness default; no sleeps. The sidecar's poll interval is the only
   added latency; keep it at the sidecar harness's value.
4. Delete `sidecar*Event`, `ingestTranscriptAsSidecar` and the store-side
   event constructors from e2e once no caller remains. Grep gate: an e2e test
   that dials the store's write verbs or writes under the config dir or
   spool root fails the run.
4b. Fold in the two known fake-writer defects: the fan-wide-cancel agent-spool
   `EXIT=` terminator (agent spools carry no terminator) and the
   context-budget-warning attachment spelling (needs a grounding capture).
5. Record in daemon/e2e's package doc: five real processes minus the
   frontends; the sidecar is real; the daemon is hosted in-process (its
   black-box coverage lives in daemon/integration).

## Order and ownership

- After the five-way merge into overhaul/integration (the e2e package must
  compile against the landed protos first).
- Step 0 (first): one sonnet-medium agent inventories every hand-written
  event shape in the 30 files and maps each to an existing fake scenario or
  a proposed new one; the project lead reviews the map.
- Step 1 and the fake-SDK additions of 2b: sonnet-medium agents in a shim
  worktree (the fake lives in the shim tree). Then a fanout of sonnet-medium
  agents, one per heavy file group, for the e2e rewrites in worktrees off
  overhaul/integration; project lead merges and resolves. Step 4 last.
- Then the scenario→e2e audit is rerun against daemon/e2e alone, and the
  coverage hardening step begins.
