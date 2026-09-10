# Store + sidecar: existing-feature coverage against the overhaul docs

Scope: every behavior-bearing feature found in `agent-shim/shim-store/` and
`agent-shim/claude/shim-sidecar/`, checked against `docs/overhaul/store.md` and
`docs/overhaul/sidecar.md`. A FINDING is a real existing feature with no
accounting in either doc (no port, no ruled removal, no deliberate ignore).
Implicit coverage counts as accounted. Paths are repo-relative to
`modules/app/agent-repl/`.

## Findings (most significant first)

1. **The store-link state machine — reads gated on an established link, with
   per-connection (not per-process) recovery, a never-losable redial ladder,
   and outage reporting — has no successor in the Connect world.**
   - Evidence: `agent-shim/claude/shim-sidecar/link.go:37-113` (linkDown/linkUp,
     `whenUp`, `requireLinkUp` hard assert), `link.go:119-203` (`dialDue`,
     single-flight `dial`, `establish` = connect → recover cursors → reset
     tracker → seed owners → only then `rescan`), `link.go:208-258`
     (`linkLost` drops `cursors`, `reportOutageClosed`), `main.go:199-221,
     259-296` (dial ticker, `whenUp` on every periodic action).
   - Doc that should account: `sidecar.md` — its whole restart story is "On
     startup: `GetSidecarCursors`" (§Cursor recovery) plus removing the beat
     timer. Connect has no long-lived producer connection, so "the link is up"
     stops being observable, yet the invariant the machine enforces (never build
     a tailer from a position the store did not hand us; produce NOTHING while
     the store is unreachable) is load-bearing and caused a real incident when
     absent (`link.go:9-16`). Nothing in the doc says what replaces it.
   - Confidence: high.

2. **The deferred-frame ("hold") protocol between tailer and handler — a
   handler may refuse to convert an unsettled trailing frame and the tailer
   rewinds its cursor advance to that frame's offset.**
   - Evidence: `internal/tail/context.go:26-61` (`Redelivers`, `HeldOffset`,
     `HeldDeliveries`), `internal/tail/tailer.go:150-200` (`applyHold`, rewind,
     out-of-batch hold refused loudly), `internal/handler/transcript.go:96-160`
     (`holdCount`, `maxHoldDeliveries = 1`).
   - Doc that should account: `sidecar.md`. The doc's cursor section states
     offset/carry only; it never mentions that a batch may deliberately advance
     the cursor short of what it read, nor the bound on how long a record may be
     held. Under the new model the same problem persists (a record whose meaning
     depends on the next line), so this is a mechanism that must be ported or
     ruled out.
   - Confidence: high.

3. **Deliberate cancellation of detached work is read from disk today; the
   doc's exempt set drops the only record that carries it.**
   - Evidence: `internal/convert/detached.go:113-128` (`taskStop`: a
     `toolUseResult` with `command`+`taskType`+`taskId` is the CANCELLED
     terminal), `internal/handler/shell.go:24-34` (the parallel argument for
     the EXIT marker: without evidence the task waits for a LOST sweep).
   - Doc that should account: `sidecar.md` §Constant-cost conversion — its
     EXEMPT SET names `TaskStop/TaskOutput/TaskGet/TaskList` as "DROPPED
     entirely — never AgentUnmodeled, never residue". Dropping the TaskStop
     result deletes the cancellation terminal, leaving stopped background work
     to be resolved as LOST (which the code explicitly calls the wrong verdict).
     The doc never reconciles the exemption with the terminal it carries.
   - Confidence: high.

4. **The held-spool lifecycle: an unattributed spool is never tailed, and an
   aged spool absent from the open-task set is retired TERMINAL — a permanent,
   by-design ingestion drop — with bounded diagnostics and a readiness verdict.**
   - Evidence: `held.go:108-234` (`HeldLifecycle.Observe`, `terminallyAbsent`,
     "its bytes never reach the store" at `held.go:171`), `held.go:236-340`
     (`Snapshot`, `Readiness`), `main.go:163-171` (`UnownedSpoolWindow`,
     `ActiveUnresolvedSpoolThreshold`), `main.go:383-416` (`resolveTargetOwner`).
   - Doc that should account: `sidecar.md`. It covers only the *seeding* gap
     ("spool-owner seeding re-derive from the STORE's live-work reads") and
     insists unconvertible material is never dropped. A whole class of spool
     bytes being permanently and silently excluded from ingestion, plus a
     readiness signal derived from it, is unaccounted in either direction.
   - Confidence: high.

5. **Watch-stream backpressure: bounded per-subscriber buffering with
   hard slow-consumer disconnect.**
   - Evidence: `agent-shim/shim-store/internal/server/fanout.go:11-15`
     (`defaultSubBuffer = 1024`), `fanout.go:136-164` (`publish` drops a
     subscriber whose buffer is full rather than blocking the publisher),
     `internal/server/server.go:492-500` (`subscriptionTerminalSlowConsumer`),
     `server.go:574-611` (single-owner terminal record, warn level).
   - Doc that should account: `store.md` §Open/watch bifurcation, which
     describes `WatchAgentSession` as "a STANDING stream ... Pure tail; creates
     nothing, ends nothing". It never says what happens to a consumer that
     cannot keep up, nor that the store may end a watch. Since a dropped watcher
     recovers by re-opening (`known_through` catch-up), the policy is portable —
     but it is not stated.
   - Confidence: high.

6. **`/tmp` spools carry three kinds, not one: `a*` agent transcripts and `w*`
   workflow journals as well as `b*` shell output.**
   - Evidence: `internal/discover/discover.go:186-199` (kind by `a`/`b`/`w`
     task-id prefix; an unclassifiable prefix is logged as a total-ingestion
     violation), `internal/tail/context.go:6-13` (Kind comments naming the
     `a*.output` / `w*.output` spools).
   - Doc that should account: `sidecar.md` §Discovery scope item 4, which
     describes `tasks/*.output` spools purely as shell output ("the bash update
     arm is structurally detach-only") and gives agent/workflow material only
     its config-root paths. Two of the three spool kinds have no home in the new
     discovery scope.
   - Confidence: high.

7. **The store's SQLite durability/concurrency contract and its
   assign-then-announce ordering lock.**
   - Evidence: `internal/db/db.go:83-110` (WAL, `busy_timeout(5000)`,
     `synchronous(NORMAL)`, `foreign_keys(ON)`, `_txlock=immediate` with the
     BUSY_SNAPSHOT rationale), `internal/server/server.go:76-105` (`ingestMu`:
     mutual exclusion across commit-then-publish, added after two observed
     production session kills), `server.go:440-463`.
   - Doc that should account: `store.md`. It specifies "one transaction"
     semantics for WriteBatch but nothing about how concurrent producers are
     serialized, nor that publication order must match commit order on the watch
     stream. Under Connect with two planes writing concurrently the ordering
     hazard is unchanged.
   - Confidence: high for the ordering lock, medium for the pragma set.

8. **The store's opt-in, local-only pprof surface.**
   - Evidence: `internal/pprofsurface/pprofsurface.go:1-138` (unix-socket or
     loopback-only bind, non-socket path refused, 0600, private mux),
     `main.go:44` (`-pprof` / `AGENT_REPL_STORE_PPROF_ADDR`), `main.go:139-167,
     218-229` (opened before the database so a wedged migration is profilable).
   - Doc that should account: `store.md`. Its dead-code list removes the UDS
     front end and the health verb but says nothing about the operator surfaces
     that hang off the same process lifecycle; a Connect port silently loses
     this unless it is carried.
   - Confidence: medium-high.

9. **Slow-query observability: a threshold-driven warn record per statement
   family, with an env knob that refuses malformed values.**
   - Evidence: `internal/db/slowquery.go:12-92` (`AGENT_REPL_STORE_SLOW_QUERY_MS`,
     `DefaultSlowQuery = 250ms`, `observeQuery`), statement families at
     `slowquery.go:37-44` (already noting two families died with their queries),
     call sites `internal/db/ingest.go:71-73`, `internal/db/query.go:23,37,72`.
   - Doc that should account: `store.md`. The four-table schema replaces every
     statement family named here, and the doc's constant-cost principle ("every
     observation costs a constant number of single indexed lookups") is exactly
     what this instrumentation proves at runtime — yet neither the mechanism nor
     its retirement is mentioned.
   - Confidence: medium-high.

10. **Discovery breadth and latency: multiple config roots, and an fsnotify
    watcher as the latency path with the periodic scan as backstop.**
    - Evidence: `main.go:44` (`--config-roots ~/.claude,~/.claude-chesscom`),
      `internal/discover/discover.go:82-95` (per-root glob union + spool glob),
      `internal/discover/watcher.go:11-72` (recursive dir watches, `AddDir`,
      watch-add failure degrades to the scan).
    - Doc that should account: `sidecar.md` §Discovery scope, which enumerates
      four FILE KINDS but names no root set and no notification mechanism.
      Multi-root support is what makes a second Claude config dir visible at
      all. (Note: `NewWatcher` is currently referenced only from tests —
      `internal/discover/discover_test.go:211` — so the latency path is built
      but not wired into `Run`; the periodic tickers in `main.go:260-263` are
      the live behavior. Both facts are unaccounted.)
    - Confidence: medium-high for the root set, medium for the watcher.

11. **`write_id` is DETERMINISTIC — a digest of (producer, path, offset,
    discriminator) — which is what makes replay idempotent with no durable
    producer state.**
    - Evidence: `internal/convert/entry.go:62-84` (`writeID`,
      `syntheticWriteID` for inferred records), `internal/storeclient/client.go`
      Write doc comment (the replay guarantee rests on it), `diagnostics.go:88-97`
      (the diagnostic's own stable digest).
    - Doc that should account: `sidecar.md` §Writing to the store says only
      "mint `write_id` once per write"; `store.md` says "minted once per write,
      never regenerated". A random-per-write id satisfies both sentences and
      breaks the restart-replay absorption both docs then rely on ("`write_id`
      absorption is the recovery for the duplicate case"). The derivation rule
      is the missing half.
    - Confidence: medium-high.

12. **Spool-owner resolution's conflict detection and path normalization.**
    - Evidence: `owner.go:64-92` (`normalizeOwnerOutputPath` resolves the macOS
      `/tmp` → `/private/tmp` symlink through non-existent suffixes),
      `owner.go:97-134` (exact-output-path evidence beats task-only; filename
      similarity is never evidence), `owner.go:139-173` (`ownerConflicts` /
      `ownerPathConflicts` — two sessions claiming one task id disables
      resolution rather than picking one).
    - Doc that should account: `sidecar.md` §Owner resolution. It states that
      every frame must name its agent and that the main agent id is ours, but
      never how a per-task spool file is bound to an owner, nor what happens
      when two claims conflict. Under the new model the owner is an AgentId
      rather than a session id, so the rule must be restated, not inherited.
    - Confidence: medium.

13. **The store's structured-log correlation vocabulary is keyed to the retired
    addressing (`claude_session_id`, `replay_from_seq`/`first`/`last`,
    `delivered`, subscription terminal owner/reason).**
    - Evidence: `internal/logging/logging.go:17-48` (Fields),
      `logging.go:52-62` (record JSON: `claude_session_id`),
      `logging.go:163-171` (replay counters), `internal/server/server.go:596-606`
      (terminal record fields).
    - Doc that should account: `store.md`. It abolishes `seq` and session
      addressing on the wire and scopes by our main-agent id, but says nothing
      about what the store's log records correlate on afterwards — and these
      records are consumed by the debugging tooling, so the key is a contract of
      its own.
    - Confidence: medium.

14. **Bounded-evidence and bounded-read constants that shape what actually
    reaches the store.**
    - Evidence: `internal/tail/tailer.go:11-14` (`defaultMaxRead = 4MiB` per
      poll), `internal/tail/codec.go:14-18` + `codec.go:75-86`
      (`defaultMaxCarry = 16MiB`, oversize carry emits a parse-error frame and
      resyncs, DROPPING the oversized bytes), `internal/convert/entry.go:15-18`
      + `entry.go:170-186` (`maxUnparsedRaw = 64KiB` truncates the raw evidence
      an `unparsed` record carries).
    - Doc that should account: `sidecar.md`, which says `carry` is "bounded"
      but never that exceeding the bound discards bytes, and says `unparsed`
      "carries source, offset, parse_error, raw bytes" with no truncation. Both
      are quiet losses inside arms the doc describes as whole and investigable.
    - Confidence: medium.

15. **Transcript-level classification of non-conversation line types
    (`vendor_specific` vs `unknown`) and the `system`/`attachment` subtype
    routing.**
    - Evidence: `internal/convert/convert.go:199-217` (`knownMetadataLines`:
      mode, permission-mode, queue-operation, last-prompt, ai-title, pr-link,
      file-history-snapshot/delta, frame-link, attribution-snapshot),
      `convert.go:165-196` (Line's routing; empty `type` → unknown),
      `convert.go:462-477` (`system` subtypes: compact_boundary, api_error, rest
      vendor_specific), `convert.go:507-530` (`attachment` types).
    - Doc that should account: `sidecar.md`. Its EXEMPT SET is a list of TOOLS;
      its residue arms are described generically. The top-level transcript line
      taxonomy — which decides whether a brand-new vendor line asks for a
      converter or for a model — has no counterpart, and it is precisely the
      distinction the doc's "a recognizable modeled kind arriving there is a
      producer defect" rule depends on.
    - Confidence: medium.

16. **Context lifecycle records: `/clear` detection through the expanded
    command envelope, and compaction-boundary + summary coalescing in file
    order.**
    - Evidence: `internal/convert/convert.go:652-730` (`isClearCommand`,
      `unwrapCommandEnvelope`: the literal "/clear" never appears on disk),
      `convert.go:569-651` (`contextCleared`, `contextCompacted` coalescing the
      boundary with the following summary line, "FILE ORDER, NEVER TIMESTAMP
      ORDER"), `IsCompactBoundary` at `convert.go:614-623`.
    - Doc that should account: `sidecar.md`'s conversion and gotchas sections.
      `ContextCut`/compaction is named nowhere; the ~30 activity kinds listed do
      not include a context-lifecycle family, so a real and user-visible class
      of transcript record has no target shape.
    - Confidence: medium.

17. **Tail rotation/truncation handling: a changed `dev:inode` or a shrunken
    file resets the cursor to 0 and re-reads, leaning on store dedup.**
    - Evidence: `internal/tail/tailer.go:104-125` (rotation reset; truncation
      reset with "bytes past the committed offset are unrecoverable"),
      `tailer.go:126-137` (a cursor with no new bytes is still surfaced so a
      reset commits).
    - Doc that should account: `sidecar.md`, which justifies `file_id` as
      "rename-proof" but never states the reset-and-re-read behavior, nor that
      a truncation is a known data loss. Adjacent to accounted material, so the
      port could plausibly infer it — but the loud-loss statement is a policy,
      not an inference.
    - Confidence: medium-low.

18. **The health-probe JSON/exit-code contract that agent-shim-doctor parses.**
    - Evidence: `internal/healthcheck/healthcheck.go:21-64` (exit codes 0/2/10–17,
      failure-class strings, the `Result` JSON), `main.go:41-48, 56-89`
      (`-health-check` mode: exactly one JSON object on stdout).
    - Doc that should account: `store.md` §Blockers, which says "agent-shim-
      doctor's probe re-derives from the Connect endpoints" — that covers the
      probe's mechanism but not whether doctor's machine-readable contract
      (exit-code vocabulary and failure classes) survives, which is what doctor
      actually switches on.
    - Confidence: medium-low (partially accounted).

19. **Vendor `api_error` system records are read and carried today.**
    - Evidence: `internal/convert/convert.go:478-494` (`apiErrorFailure`,
      currently unported but explicitly kept as a read).
    - Doc that should account: `sidecar.md`'s conversation.v1 section. It names
      `AgentFailure` terminals as "the only record of how a turn ended"; a
      vendor API error recorded mid-turn in the transcript is not obviously one
      of those, and the mapping is not stated.
    - Confidence: low-medium (arguably implicit in the AgentFailure family).

## Checked and accounted for (no finding)

Store:
- `(session_id, seq)` + `top_level_message_id` schema, the `entry`/`unconverted`
  tables, `MaxSeq`, and every seq-derived read — explicitly killed by
  `store.md`'s dead-code list and the four-table architecture.
- `ErrRecordPersistenceUnreconciled` refusal stub (`internal/db/ingest.go:43-56`)
  — named for removal.
- UDS + wire-Any framing, connection role classification by first frame,
  `Subscribe`/`EntryDelivery`/`ConnectionHeartbeat`/`HealthCheck` — deleted with
  the Connect port (`store.md` dead-code list; `server.go:13-35`).
- The producer preamble / idle-producer keepalive (`server.go:356-364`) —
  `store.md` blockers: "Idle-producer liveness: ConnectionHeartbeat died
  deliberately … confirm the sidecar needs no substitute."
- Cursor table and `GetSidecarCursors` semantics including
  unset-file_id-means-all and the reached failure arm
  (`internal/db/query.go:31-84`, `server.go:297-354`) — `store.md` §Cursor
  semantics.
- Replay/dedup by `write_id`, cursor-in-the-same-transaction
  (`ingest.go:58-115`) — `store.md` §WriteBatch semantics.
- Plane extraction and the refusal of an entry that names no plane
  (`ingest.go:129-141`) — `store.md` §StoreEntry `plane`, plus the standing
  validation conventions.
- Migration framework, `schema_meta` versioning, refusal to read a newer/foreign
  schema (`db.go:196-293`) — killed by NUKED-NEVER-MIGRATED.
- Serve/Close race and the one-owner subscription terminal machinery
  (`server.go:180-265, 522-611`) — `store.md` blockers name the race; the doc
  explicitly keeps terminal ownership as transport lifecycle.
- No health verb (`healthcheck.go` refusal, `Probe`) — ruled by design.
- Bootstrap-vs-canonical logging split, deferred exit trace, signal shutdown,
  XDG cache paths (`main.go:107-124, 178-296`) — process plumbing the port
  carries unchanged; nothing in it depends on the retired contract.

Sidecar:
- `convert.UnportedEntry` / `__unported_conversion` and the ~34 deleted convert
  tests — `sidecar.md` dead-code list.
- Any-over-UDS transport, `Client.Health`/`Heartbeat` `ErrNoHealthProbe`, and
  the beat timer around them (`internal/storeclient/client.go:50-58, 219-260`;
  `main.go:262, 306-314`) — `sidecar.md` dead-code list and the LOAD-BEARING
  blocker ("remove probing, not find a probe").
- Unread `WriteBatchResponse` and the unreachable rejection branch
  (`client.go:194-215`) — `sidecar.md` blockers, "take it".
- No session attribution on `StoreEntry` (`emit`/`flushDiagnostics` losing their
  fanout, `main.go:508-613`; `convert/entry.go:19-45`) — `sidecar.md` blockers.
- Diagnostic outbox with no typed home (`diagnostics.go:17-130`) —
  `sidecar.md` blockers ("No bookkeeping arm").
- `OpenTaskState` loss: `seedOwners` (`owner.go:215-238`) and
  `stale.Restore` (`internal/stale/stale.go:119-157`) — `sidecar.md` blockers
  (re-derive from live-work reads per Owed A/H).
- LOST as its own verdict with the three inferences — vanished-file,
  silence-timeout, boot-sweep (`stale.go:1-16, 227-277`; `main.go:319-332`,
  `bootTimeMillis` at `main.go:668-676`) — `sidecar.md` §LOST/staleness maps
  them onto `DetachedLost { file_vanished | went_silent | swept_up }`.
- The `EXIT=<code>` terminal and its strict last-line matching
  (`internal/handler/shell.go:88-176`) — `sidecar.md` §Discovery scope
  ("the EXIT marker to the terminal").
- Spool bytes as offset-carrying deltas (`handler/shell.go:65-70`,
  `convert/detached.go:130-142`) — `sidecar.md` (`AgentBashUpdate` deltas).
- Workflow journal's two record shapes and run-id-from-path
  (`internal/convert/journal.go:23-40`, `handler/journal.go:28-40`,
  `discover.go:143-152`) — `sidecar.md` §Discovery scope, journal paragraph.
- Per-agent workflow transcripts + `meta.json` — `sidecar.md` Owed E (the
  discoverer's existing `MetaPath` for subagent sidechains, `discover.go:132-142`,
  is the same mechanism generalized).
- Skill-body resolution (`convert.go:532-568`) — deliberately REPLACED: the doc
  retires name-map correlation for `sourceToolUseID` on the isMeta user record.
- Tool call/return correlation by `tool_use_id` (`convert.go:336-393`) —
  `sidecar.md` §Constant-cost conversion.
- Total-ingestion mandate and the residue arms (`convert.go:151-196`,
  `entry.go:99-186`) — `sidecar.md` residue paragraph.
- Container id derived purely from the task id
  (`convert/entry.go:46-60`) — `sidecar.md` (typed `DetachedWorkId`, upsert per
  THING, nothing recovered across a restart).
- Spool path is a location, never an identity; the runtime-vs-transcript session
  id divergence (`discover.go:9-21`, `internal/tail/tailer.go` file_id) —
  `sidecar.md` §Gotchas (the ~22% transcript/runtime divergence) and "vendor
  identity never crosses the contract".
- Cursor ride-along with the batch and the no-spill policy
  (`main.go:488-506`, `client.go:186-215`) — `sidecar.md` §Cursor recovery /
  §Writing to the store.
- Bootstrap logging, exit trace, signal shutdown, `~` expansion, XDG cache paths
  (`main.go:55-147, 635-687`) — process plumbing, contract-independent.
