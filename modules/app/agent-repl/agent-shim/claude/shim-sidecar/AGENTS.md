# agent-shim/claude/shim-sidecar/

The Claude FILE-PLANE READER (Go, singleton, launchd-managed). It observes what
the vendor's agent binary writes to disk, tails it with cursored,
truncation-aware reads, converts each record into `conversation.v1` vocabulary,
and writes it to the store as `store.v1.StoreEntry` batches with the reader
position riding the same transaction.

It is a COPIER. It has no view of process liveness, no session semantics, owns
no database, and never contacts the daemon. The only thing it concludes on its
own is that it STOPPED SEEING a detached run.

Dual-plane relationship with the shim: `StoreEntry.plane` names the producer.
The SHIM (stream plane) watches the SDK live — first to know, authoritative for
session and turn LIFECYCLE, and the only source for anything not yet on disk.
The SIDECAR (file plane) reads what the vendor itself recorded — authoritative
for conversation CONTENT. Both write through the same envelope, into one
upsert-key space, over one write path.

The sidecar is 100% specific to Claude's file formats BY DESIGN; its entire job
is converting that vendor reality into the vendor-agnostic contract.

## The store surface: two verbs, over Connect

`internal/storeclient` is a `storev1connect.ShimStoreClient` over an
`http.Transport` whose `DialContext` opens the store's unix socket. Plain
Connect protocol, binary codec, HTTP/1.1 — both verbs are unary, so no h2c
upgrade is involved.

- `WriteBatch` and `GetSidecarCursors` are the WHOLE surface. The read side
  (OpenAgentSession / WatchAgentSession / ReadAgentPage / GetWorkflow /
  GetLiveWork) is the SHIM's; the sidecar never calls it. In particular the
  sidecar never calls `GetLiveWork`, so it holds no authoritative open-task
  snapshot and re-derives everything from files and cursors.
- THERE IS NO CONNECTION TO HOLD AND NO HEALTH VERB TO ASK. The
  length-prefixed Any-over-UDS framing, its Subscribe/Ack dial protocol,
  `ConnectionHeartbeat`, `Health`, `ErrNoHealthProbe`, `ErrNotConnected`, the
  15s beat timer and the beat-driven teardown are DELETED. Streams and the
  transport own liveness: LIVENESS IS WHETHER THE LAST RPC WORKED. Do not
  reintroduce a probe, and do not add a health rpc to the contract.
- THE RESPONSE IS ALWAYS READ. A failure arm is a `storeclient.RefusalError`
  (the store was reachable and said no); a transport error is a plain error; a
  response carrying NEITHER arm is raised, never read as an empty success.
- `WriteBatch` success means DURABLE — records and cursor advance in one
  transaction — and a replayed batch fully absorbed by `write_id` is the SAME
  success arm. Failure means NOTHING was committed, so the sidecar does not
  advance, holds NO retry buffer and spills NOTHING: its sources are durable
  files it re-reads from the last committed cursor.
- Producer string: `shim-claude-sidecar` (`storeclient.Producer`).
- `AGENT_REPL_STORE_SOCKET`, when set, is the DEFAULT of `--store-socket`; an
  explicit flag beats it. Default when unset:
  `~/.cache/agent-repl/sock/store.sock` (`XDG_CACHE_HOME` honored).

## The cursor-first production cycle, and the store-unreachable invariant

`cycle.go`. EVERY PRODUCTION CYCLE BEGINS WITH A SUCCESSFUL
`GetSidecarCursors`. Any store error — refused or unreachable, cursor read or
batch write — SUSPENDS ALL PRODUCTION until a full
recover-cursors-then-rescan succeeds. While production is suspended the
sidecar reads nothing, loudly.

It is structural rather than a retry around a special case:

- `cursors` is nil unless a cycle recovered it, and only `beginCycle` sets it;
- `rescan` is the ONLY place a tailer is ever built, and it asserts that;
- suspension DROPS every tailer with the cursors, so a resumed cycle rebuilds
  each one from the position THIS cycle's store handed it — never from a
  remembered one;
- after construction a tailer advances only through `Commit`, which the poll
  loop calls only on a durable `WriteBatch` success;
- every periodic action runs through `producing`, so nothing polls, sweeps or
  rescans while production is suspended.

Boot ordering is therefore irrelevant by construction: a store that starts late
simply means the first cycle has not begun yet, which is the same state a store
that dies mid-run produces, handled by the same code. There is no boot path to
get wrong.

- NEVER BUILD A TAILER FROM A POSITION THE STORE DID NOT HAND US. The
  predecessor of this design recovered cursors once at boot and, on failure,
  re-read every watched file from offset 0 — a fallback masking a down
  dependency, which re-ingested whole conversations in production.
- "COLD" means exactly one thing: a REACHED store that genuinely holds no
  cursor for a file. That is the backfill path and it reads from offset 0
  honestly.
- Recovery is a jittered ladder (250ms doubling to a 10s ceiling, held
  forever, no attempt budget), and the schedule is a DEADLINE THE LOOP
  COMPARES AGAINST, never a timer someone must re-arm: a lost re-arm silences
  recovery with no state left saying one was owed, and that is exactly what a
  production store bounce once produced.
- One WARNING opens a suspension, one normal record closes it
  (`production suspended` / `production resumed`), and each retry is verbose.

## Restart correctness: the boot rewind

Per file per BOOT, exactly once, `tail.RewindToTurnStart` moves the RESTORED
cursor back to the first record of the in-progress turn — ONE bounded backward
scan (`tail.DefaultRewindWindow`) ending at the committed offset, targeting the
last user-prompt record within it.

- WHY: the cursor is exactly-once for BYTES, but the converter's joins are
  in-memory. A tool result whose call was converted before the restart has no
  open call to settle. Re-reading the in-progress turn re-warms those joins.
- WHY IT IS FREE: every record mints a DETERMINISTIC `write_id` from its source
  coordinates, so the re-emitted records are absorbed as the same success arm
  rather than duplicated.
- The store's cursor row is NOT rewritten; only the in-memory position moves,
  and the next successful batch advances the durable cursor normally.
- Spools are never rewound: they carry no turns and their deltas already carry
  offsets. When the window holds no turn start the store's cursor stands —
  reading from an arbitrary older position would be worse than not rewinding.

## The hold

A record can be UNSETTLED at the end of a batch: its meaning depends on the
line after it (today: the compaction boundary and its following summary, written
~1ms apart, so a ~1s poll lands between them). A handler may HOLD the trailing
frame; the cursor then advances SHORT of what was read, to the held frame's
offset, so a restart re-reads it too.

- BOUNDED TO ONE REDELIVERY. The second delivery sets `Context.HoldForced` and
  the handler MUST convert on whatever evidence it has; a hold that survives
  the forced delivery is refused (WARNING) and the cursor advances past the
  frame. A record held forever is a record never stored.
- AN OUT-OF-BATCH `HeldOffset` REJECTS THE BATCH (`tail.ErrHoldOutOfBatch`,
  ERROR record) as a producer defect. Obeying it would rewind over already
  converted records or park the cursor ahead of the frame it claims to hold,
  and each of those loses or duplicates records.
- DEFERRING IS NOT SKIPPING, and the bound is what keeps that true.

## Discovery

`internal/discover`. BOTH account config roots are discovery roots
(`--config-roots`, default `~/.claude,~/.claude-chesscom`): the second
account's transcripts are invisible otherwise. A periodic `Scan` is the
completeness backstop; fsnotify (`Watcher`) supplies latency.

Four kinds of file, all written by the vendor's agent binary:

1. session transcripts — `<config root>/projects/<cwd-slug>/<vendor session>.jsonl`
2. subagent transcripts — `.../<vendor session>/subagents/agent-<id>.jsonl`,
   with `agent-<id>.meta.json` REQUIRED
3. workflow journals and their per-agent transcripts — under
   `.../subagents/workflows/wf_<id>/` (`journal.jsonl` and `agent-<id>.jsonl`
   + meta)
4. task spools — `<spool root>/[claude-<uid>/]<cwd-slug>/<vendor session>/tasks/<task>.output`

BOTH SPOOL-ROOT SPELLINGS ARE ACCEPTED: `--spool-root /tmp` resolves
`claude-<uid>` itself, and a root pointed straight at the uid dir works too,
because neither spelling may make a spool invisible.

- NOTHING IS EVER DECODED FROM `<cwd-slug>`. The vendor builds it by replacing
  every byte of the absolute cwd outside `[A-Za-z0-9]` with `-` (underscores
  included, case preserved), so `/private/var/folders/_m/x` becomes
  `-private-var-folders--m-x`. THAT MAPPING IS LOSSY AND NOT INVERTIBLE: two
  different directories can render to one slug. The slug is matched
  POSITIONALLY and its content is never read; every identity comes from what is
  INSIDE it — the session uuid file names, `subagents/`, `wf_*`, `agent-<id>`,
  and the `tasks/` basenames. Never add a slug-to-path decoder, and never
  compare slugs across roots as though they were paths.
- The `<vendor session>` directory segment of a spool path is not read either:
  it is the harness's RUNTIME session id (see below).

- A SPOOL PATH IS A LOCATION, NEVER AN IDENTITY. The spool layout embeds a
  session-shaped segment; it is the harness's RUNTIME session id, which
  disagrees with the transcript's whenever a session was resumed. Trusting it
  once filed one task under two ids. A spool `Target` therefore carries NO
  `SessionID`.
- A SPOOL'S KIND IS ITS TASK-ID PREFIX: `b*` shell output, `a*` an agent
  transcript, `w*` a workflow journal. ANY OTHER PREFIX IS A LOUD
  TOTAL-INGESTION VIOLATION (ERROR record) and is still discovered as
  `tail.KindResidueSpool` so its bytes land whole as residue. Dropping the
  file from discovery, which this package used to do, is the one outcome the
  mandate forbids.
- A TRANSCRIPT WITHOUT ITS META IS HELD, NEVER DROPPED: `agent-<id>.meta.json`
  is the ONLY source of the agent's type, spawn depth, model and worktree, so
  the transcript is discovered, warned about ONCE, re-checked every rescan, and
  not tailed until the meta appears.
- WORKFLOW IS KICKED this wave: journals and workflow per-agent transcripts are
  discovered and cursor-tailed, but they convert to residue only.
- EVERY DISCOVERED PATH AND EVERY ROOT IS SYMLINK-RESOLVED
  (`discover.Normalize`). macOS's `/tmp` -> `/private/tmp` otherwise makes one
  file read as two the moment a spool path is compared against an owner's
  output path. Normalization walks up to the deepest existing ancestor, so a
  not-yet-created spool normalizes too; it never fails and never drops a path.

## Owner resolution and the held spool

`owner.go`, `held.go`. A spool's owner is looked up by task id against the
spawning call the converter read out of a tool result, and by the exact output
path the vendor named — nothing else. Filename similarity is deliberately not
evidence and is never consulted. A task two different calls claim resolves to
NOTHING (ERROR record): guessing between two claims is how one run's output
lands in another run's card.

- An unclaimed spool is HELD: discovered, re-checked every rescan, not tailed.
- AN AGED UNOWNED SPOOL IS NEVER DROPPED. Past `UnownedSpoolWindow` its bytes
  are INGESTED as unparsed residue naming the spool as their source (one
  WARNING), and IT KEEPS BEING TAILED so nothing appended later is lost either.

## The LOST policy

`internal/stale`. LOST IS ITS OWN WORD: it means "we stopped seeing it", never
"we know it failed". The arm IS how we concluded it, and the sidecar states it
loudly rather than dropping the run or spelling it as a completion.

- `file_vanished` — the file disappeared while the run was open, and a grace
  window absorbed the ordinary rename/replace race first.
- `went_silent` — the file is still there and has not grown for longer than its
  kind's silence window.
- `swept_up` — at boot, the file has not been touched since before the machine
  booted. Nothing survives a reboot.

It RE-DERIVES FROM FILES AND CURSORS because there is nothing else: the store
holds no open-task snapshot for the sidecar. A run is keyed by its resolved
path; a terminal READ FROM THE FILE ITSELF (a spool's `EXIT=` marker) settles
the run so it can never afterwards be concluded LOST. A transcript is never
armed: it is an agent's own record and its silence concludes nothing.

The package MINTS NO RECORDS. A sweep returns OBSERVATIONS; spelling one as the
run's terminal is conversion and happens behind the seam.

- CONTRACT GAP: there is no `DetachedLost` message on the wire this wave, so
  HOW we stopped seeing a run survives only in the log record. Do not invent an
  arm that means something else.

## The reader/converter seam

`seam.go` is the WHOLE boundary. The reader (this package, `internal/tail`,
`internal/discover`, `internal/storeclient`, `internal/stale`) decides which
files exist, where it has read to, who owns a spool, and when it stopped seeing
a run. The converter (`internal/convert`, `internal/handler`) decides what a
record MEANS.

- `tail.Handler { Handle(frames []tail.Frame, ctx *tail.Context) []*storev1.StoreEntry }`,
  `tail.Frame`, `tail.Context` and the `handler.New*` constructors are the seam.
  The reader may ADD `tail.Context` fields; it never removes or renames one.
- `tail.Context` carries, beyond the counters and the hold protocol: `FileID`,
  `MainAgentID`, `AgentID`, `SpawnBackgrounded`, `MetaPath`, `ConfigRoots`,
  `SessionID`, `Path`, `Kind`, `TaskID`, `SpoolDir`, `RunID`.
- Two OPTIONAL interfaces, adopted by adding a method. Both take plain function
  and string arguments so neither package imports the other:
  - `SetTaskObserver(func(taskID, toolUseID, agentID, outputPath string))` —
    the converter reports each spawn it reads off a tool result; the reader
    turns it into a spool's owner. ONE CALL PER OBSERVATION, never a map two
    packages share.
  - `LostTerminal(taskID, runActivityID, ownerAgentID, reason string) []*storev1.StoreEntry`
    — the converter spells the reader's LOST conclusion as the run's terminal.
- A converter that has adopted NEITHER is not a silent degradation: the reader
  states exactly what it could not hand over, at ERROR level, every time it had
  something to hand.

## Identity and keys

- MAIN AGENT identity: `AgentId.value` is the transcript FILE's session uuid
  (the `<vendor session>.jsonl` basename), NEVER the per-record `sessionId`
  field, which diverges from it in roughly a fifth of records. Transcript
  divergence never rides the wire.
- SUBAGENT identity: the vendor `agentId` of sidechain records, which is also
  the `agent-<id>` file name. An agent is NOT its spawning call.
- `top_level`: main-agent frames name the main agent; sidechain frames name the
  owning session's main agent UNLESS the spawn was backgrounded, in which case
  the subagent itself. UNSET only when genuinely unresolvable — residue that
  names no agent.
- `write_id` is DETERMINISTIC: hex sha256 of
  `"shim-claude-sidecar|" + path + "|" + offset + "|" + discriminator`, where
  the discriminator distinguishes multiple entries minted from one record.
  RANDOMNESS IS FORBIDDEN — replay absorption rests on this.
- `upsert_key` maps the unit's identity and must equal the shim's for the same
  unit. Residue uses `residue:<path>:<offset>`.
- `StorePageLine.page_agent_id` names the frame's own agent: a subagent's
  constituents form ITS OWN book, while the spawn unit is a line in the
  parent's.
- Instants are never re-stamped at emit time: every `started_at`/`settled_at`
  comes from the file record's timestamp, so a re-read after restart yields an
  identical frame and an identical `write_id`.

## Total-ingestion mandate

Every JSON object in a file the sidecar reads MUST end up in the store as a
protobuf shape. No sampling, no skipping, no "not visually interesting"
filtering — curation is a downstream concern, never an ingestion concern.

- The mandate binds INGESTION only. A consumer is free never to read a stored
  record; irrelevance to the user is never a reason to skip parsing one or to
  leave it out of the database.
- Residue is the loud half of this, not a fallback: `unserved_item.unparsed`
  carries source, offset, parse error and the bytes verbatim;
  `unserved_item.unknown` carries a discriminator we parsed but do not model.
  A RECOGNIZABLE MODELED KIND REACHING RESIDUE IS A PRODUCER DEFECT.
- `residue.go` is the reader's own envelope-level last resort, reached only
  AFTER the failure to classify has already been stated at error or warning
  level. It reads nothing out of the bytes and models nothing about them, which
  is why it can never be mistaken for a converter.
- The EXEMPT SET is different: known built-ins deliberately not carried are
  DROPPED entirely — never `AgentUnmodeled`, never residue.
- A shape the schema cannot express is a SCHEMA GAP to surface loudly, never a
  record to drop.

## Conversion rules

<!-- Owned by the conversion work; append below this line. -->

## Logging

`internal/logging` is the ONLY diagnostic API. Direct output through `fmt`,
`log`, `slog` or an ad hoc logger is forbidden, except the documented
pre-logger bootstrap failure and the sink-emergency path.

- Every logical branch of production code logs: verbose for the ordinary path,
  `warn` for degraded-but-handled, `error` for failures. Every error is logged
  EXACTLY ONCE by its owning layer.
- IDENTIFIERS GO IN DEDICATED CONTEXT KEYS, never only in the message text.
  The correlation vocabulary is `producer`, `agent_id`, `vendor_session_id`,
  `book_agent_id`, `write_id`, `upsert_key`, `position`, `write_seq`,
  `watch_token_hash`, `rpc`, `file_id`, `path`, `offset`, `task_id`,
  `activity_id`, `turn_id`, plus `component` and `store_socket`. Top-level
  `request_id` stays.
- Numeric keys carry PRESENCE (`logging.Off`, `logging.Seq`), so an unset
  offset is absent rather than a zero that reads as the start of the file.
- RETIRED KEYS ARE GONE AND STAY GONE: `claude_session_id`,
  `agent_repl_session_id`, `seq`, `from_seq`, `replay_*_seq`. They named the
  retired (session_id, seq) addressing; the spine is agent-keyed now.
- Hot per-record and per-batch success diagnostics use `LogVerbose` (gated by
  `AGENT_REPL_LOG_VERBOSE`); lifecycle, invariant violations, refusals and
  failures are normal-verbosity records.
- R10: SIDECAR SELF-DIAGNOSTICS HAVE NO WIRE HOME. They are structured logs
  only. The diagnostic outbox that wrote them to the store is deleted; do not
  aim them at an approximate arm.
- Lifecycle records persist in
  `~/.cache/agent-repl/log/shim-claude-sidecar.log` (`--log`).

## Standing policies

- NO BACKWARDS COMPATIBILITY, EVER: no migration code, no preserving stored
  shapes, no compatibility shims for old wire formats. THE STORE IS NUKED,
  NEVER MIGRATED.
- Existing tests are not a source of truth; the protos and the planning docs
  are. Delete tests asserting dead behavior and rebuild coverage from the
  contract.
- THE VALIDATION INVARIANT: an unset non-optional field is ILLEGAL and raises
  loudly. An unset oneof is an error. PRESENCE, NEVER SENTINELS.
- CONSTANT COST: no variable-size state, no lineage walks. Each observation
  costs a constant number of single indexed lookups. A single parent-id lookup
  is fine; a variable number is not.
- NEVER swallow, default away, or fall back. Never remove existing
  error-handling coverage; adapt it.
- Implementers never change protobufs. A needed proto change is a CONCERN
  raised up the chain, never an edit.

## Vendor carry-over (viral)

Any future vendor-equivalent sidecar (a codex sidecar, say) MUST inherit this
AGENTS.md's mandates into its own AGENTS.md — including the total-ingestion
mandate, the store-unreachable invariant, the LOST policy's honesty, AND this
carry-over clause itself, so the directive propagates to every subsequent
vendor equivalent in turn.

## Verification

From this directory:

```bash
go build ./... && go vet ./... && go test -race ./...
```

- Table-driven tests, AAA (Arrange/Act/Assert), ONE edge case per test
  function, one `_test.go` per source file.
- NEVER `time.Sleep` for synchronization. The cycle is driven in tests by
  calling its steps against an injected clock (`sidecar.now`, `sidecar.jitter`,
  `sidecar.bootTimeMs`), never by waiting on a real one.
- The fake store is an in-process `storev1connect` handler on a SHORT unix
  socket path: macOS caps `sun_path` near 104 bytes, so never build a socket
  path under `t.TempDir()`. Use
  `filepath.Join(os.TempDir(), "ar-"+<8 random hex>+".sock")` and remove it in
  cleanup.
- Every test process exports `AGENT_REPL_FORBID_VENDOR_CALLS=1` in `TestMain`.
- A fixture whose mtime matters (the LOST policy seeds a run's activity clock
  from it) must be stamped on the harness clock with `os.Chtimes`.
- `make coverage` reports per-function and aggregate statement coverage for this
  module, and `modules/app/agent-repl/bin/test-all.sh` runs every tracked suite
  across the module. Both are available for a review pass; neither is a gate.
