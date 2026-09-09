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
- THE AGENT REGISTER IS SOMETHING THE SIDECAR WRITES, NOT SOMETHING IT READS.
  A book comes into existence when a page-line write (or a spawn frame) first
  names its agent; until then the store has NEVER HEARD OF that agent and
  `OpenAgentSession` REFUSES it as `unknown_agent`. HEARD-OF BUT UNWRITTEN IS
  NOT THE SAME STATE: an agent already in the register serves an EMPTY page,
  never a refusal. Production is unaffected — the sidecar never opens a book —
  but the integration harness reads books back, and it must not confuse the two:
  a POLLING read (`awaitBookLines`, `awaitBookUnits`, `awaitBookLine`, via
  `bookLinesIfKnown`) treats `unknown_agent` as "the first batch has not
  committed yet" and keeps waiting, while a ONE-SHOT read (`openBook`,
  `bookLines`, `watchBook`) fails the subject on it, because naming an
  unregistered agent there means asserting against a book nothing ever wrote.
  NEVER seed a book by any path but a real write.
- `AGENT_REPL_STORE_SOCKET`, when set, is the DEFAULT of `--store-socket`; an
  explicit flag beats it. Default when unset:
  `~/.cache/agent-repl/sock/store.sock` (`XDG_CACHE_HOME` honored).

## Flags

The launchd plists reference these; every one has an env var standing in for
it, and AN EXPLICIT FLAG ALWAYS BEATS THE ENV.

| flag | env | default |
| --- | --- | --- |
| `--store-socket` | `AGENT_REPL_STORE_SOCKET` | `~/.cache/agent-repl/sock/store.sock` |
| `--config-roots` | — | `~/.claude,~/.claude-chesscom` |
| `--spool-root` | — | `/tmp` |
| `--state-dir` | `AGENT_REPL_STATE_DIR` | `~/.claude-emacs` |
| `--log` | — | `~/.cache/agent-repl/log/shim-claude-sidecar.log` |
| `--poll-interval` | — | `1s` |
| `--rescan-interval` | — | `30s` |
| `--stale-grace` | `AGENT_REPL_STALE_GRACE` | `30s` |
| `--stale-shell-silence` | `AGENT_REPL_STALE_SHELL_SILENCE` | `30m` |
| `--stale-agent-silence` | `AGENT_REPL_STALE_AGENT_SILENCE` | `60m` |
| `--stale-workflow-silence` | `AGENT_REPL_STALE_WORKFLOW_SILENCE` | `60m` |
| `--unowned-spool-window` | `AGENT_REPL_UNOWNED_SPOOL_WINDOW` | `60s` |
| `--recover-backoff-min` | `AGENT_REPL_RECOVER_BACKOFF_MIN` | `250ms` |
| `--recover-backoff-max` | `AGENT_REPL_RECOVER_BACKOFF_MAX` | `10s` |

The seven windows take GO DURATION SYNTAX (`250ms`, `1m30s`). UNSET KEEPS THE
PACKAGE DEFAULT — zero is the only meaning "unset" has, and `internal/stale`,
`held.go` and `cycle.go` fill their own defaults, so there is one place that
knows each. A MALFORMED OR NEGATIVE VALUE IS A BOOTSTRAP ERROR: the process
states it once on stderr and exits non-zero rather than starting on a window
the operator did not choose. They exist so the integration suite can exercise a
policy that is otherwise measured in half-hours; nothing in production passes
them.

The last two are the STORE-RECOVERY LADDER's floor and ceiling (`cycle.go`,
`recoverBackoffMin` / `recoverBackoffMax`). They were the only windows in this
process with no override, which is why the outage subjects — which run a REAL
sidecar, so the injected clock the cycle's unit tests advance does not reach it
— used to wait out real rungs of production's ladder. A CEILING BELOW THE
EFFECTIVE FLOOR IS REFUSED, not clamped: it describes a ladder that cannot
climb, and the check is against the effective pair, so a floor above the
DEFAULT ceiling is refused as well.

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

- THE CYCLE'S CURSOR SNAPSHOT IS NOT THE WHOLE ANSWER. It is taken once, when
  the cycle begins, so a file that appears afterwards — a spool whose hold
  expired, a transcript the vendor RENAMED mid-cycle — is absent from it.
  ABSENT FROM A SNAPSHOT IS NOT "THE STORE HOLDS NO CURSOR": a miss asks
  `GetSidecarCursors{file_id}` for that one identity before a tailer is built,
  the answer is remembered for the rest of the cycle, and only a REACHED store's
  empty answer means offset zero. A store that cannot answer leaves the file
  unwatched and abandons the rest of the pass, because production is suspended
  from that moment.
- THE CURSOR IS FOUND BY `file_id`, NEVER BY PATH. The store keys its cursor row
  by the file's dev:inode precisely so a rename cannot lose it, and
  `CursorState.path` is where the file was last SEEN — a thing to display.
  Keying the recovered-cursor index by path made every renamed file read as one
  nobody had ever read.
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

### The refusal KIND decides what happens (ruling R-S2)

`WriteBatchFailure` carries a `kind` oneof, and it is not decoration: it says
whether a retry can help. The two arms drive OPPOSITE reactions, so the kind
travels on the typed error (`storeclient.RefusalError.Kind`, and
`storeclient.InvalidRequest(err)`) rather than being re-derived from `detail`,
which the proto documents as never switched on.

- `storage_failure` — the transaction failed in the DATABASE and a retry may
  succeed. This is the ordinary outage: production suspends and the
  recover-cursors-then-rescan cycle above runs until it comes back.
- `invalid_request{field}` — the batch violated validation and the store can
  NEVER accept these bytes. That is a PRODUCER DEFECT, so:
  - production is NOT suspended (the store is reachable and answering, and
    stopping the whole file plane over a defect in one file would be a lie
    about the dependency);
  - ONE ERROR record is written, `operation: "producer-defect"`, carrying the
    store's `field`, the `write_ids` of the whole refused batch (a batch is
    refused whole, so naming one record would misreport it), the `path`,
    `file_id` and `offset`;
  - THAT FILE's tailer is PARKED for the life of the process. Nothing more is
    read from it, because re-reading the same durable bytes re-mints the same
    rejected batch forever — a tight identical replay loop that makes no
    progress and drowns the log. Its cursor stays exactly where the store has
    it, so a fixed sidecar resumes from the same byte;
  - every OTHER file keeps being read;
  - parking is PROCESS-scoped, not cycle-scoped: an outage in between does not
    make the defect go away, so a suspension that drops every tailer must not
    quietly un-park the file.
- A failure carrying NEITHER arm is illegal on this contract. It is stated at
  ERROR as a contract violation and then treated as `storage_failure`, so the
  sidecar keeps recovering rather than parking a file on a verdict the store
  never actually gave.
- Inferred records (the LOST sweep, a cancelled terminal) name no file
  position, so there is no tailer to park; an `invalid_request` for one is
  stated as the same producer defect and simply not restated.

## Restart correctness: the boot rewind

Per file per BOOT, exactly once — PER FILE MEANING PER `file_id`, so a rename
does not buy the same file a second scan — `tail.RewindToTurnStart` moves the
RESTORED cursor back to the first record of the in-progress turn — ONE bounded backward
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
sidecar's only discovery path (`RescanInterval`) — there is no fsnotify
watcher; a prior one existed with no caller and was deleted.

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
  discovered and cursor-tailed, but they convert to DECLARED residue only —
  `workflow_journal/<type>` for the journal, `workflow/agent_transcript` for a
  per-agent transcript. Never `unknown`: the two files share a discovery kind
  and hold DIFFERENT record shapes, and the disposition follows the file.
- EVERY DISCOVERED PATH AND EVERY ROOT IS SYMLINK-RESOLVED
  (`discover.Normalize`). macOS's `/tmp` -> `/private/tmp` otherwise makes one
  file read as two the moment a spool path is compared against an owner's
  output path. Normalization walks up to the deepest existing ancestor, so a
  not-yet-created spool normalizes too; it never fails and never drops a path.

### Spool routing by task-id prefix (ruling R-S4)

The prefix is HOW a spool's conversion is selected, and each of the four cases
is a different thing:

- `b*` — a detached SHELL spool. Raw bytes, `KindShellSpool`, deltas keyed
  `bash:<run>` under the spawning call's tool_use_id.
- `a*` — a backgrounded SUBAGENT's transcript, delivered through the task
  spool. JSONL, `KindAgentTranscript`, and its BOOK is the SPAWNING CALL's
  tool_use_id, resolved from the owner index. The spool's path names a task and
  nothing else, and the agent-transcript attribution has no filename fallback
  on purpose (naming a book by `agent-<id>` would give one agent two books, one
  per plane, that no consumer could reconcile) — so without the owner map's
  answer the records name no book at all. A spool whose spawn is unresolved is
  HELD rather than tailed, so reaching book resolution without one is a reader
  defect and is stated as one.
- `w*` — a WORKFLOW spool. Workflow is KICKED this wave, so it is discovered
  and cursor-tailed like any other file and its bytes land as DECLARED residue:
  `vendor_specific{kind: "spool/workflow"}`, keyed
  `residue:file:<path>:<offset>`. It is read RAW, because no conversion would
  use its record structure. It is deliberately NOT `unparsed`: the day workflow
  ingestion lands, every one of these rows is findable by that kind, which a
  row saying "no conversion could be selected" would never be.

  RESIDUE-ONLY IS THE WHOLE OUTCOME WHILE WORKFLOW IS KICKED, and which residue
  ARM a given w* spool lands on is not settled by this wave. Owner resolution is
  what selects between them: a w* spool with no observed spawn is held and then
  demoted, so its bytes land as `unparsed` residue rather than as the declared
  kind above — and nothing attributes one today, because a workflow launch result
  is keyed by its `run_id` while the spool is named by the harness's `task_id`,
  and no mechanism reconciles the two. THAT IS A KICKED FEATURE'S CONSEQUENCE,
  NOT A DEFECT TO PATCH AROUND: both arms are unservable residue holding the same
  bytes, so nothing is lost, and the day workflow ingestion lands is the day the
  attribution is designed. Do not add a run_id-to-task_id mapping to make the
  declared kind reachable sooner. The integration suite asserts what is actually
  guaranteed — the bytes land, and nothing workflow-shaped is ever converted or
  paged — and deliberately does not assert the arm.
- anything else — a TOTAL-INGESTION VIOLATION. Logged at ERROR and ingested
  whole as `unparsed` residue, because a file dropped from discovery is the one
  thing total ingestion forbids.

## Owner resolution and the held spool

`owner.go`, `held.go`. A spool's owner is looked up by task id against the
spawning call the converter read out of a tool result, and by the exact output
path the vendor named — nothing else. Filename similarity is deliberately not
evidence and is never consulted. A task two different calls claim resolves to
NOTHING (ERROR record): guessing between two claims is how one run's output
lands in another run's card.

- An unclaimed spool is HELD: discovered, re-checked every rescan, not tailed.
- AN AGED UNOWNED SPOOL IS NEVER DROPPED. Past the hold window
  (`UnownedSpoolWindow`, replaceable with `--unowned-spool-window`) its bytes
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
- `swept_up` — the file has not been touched since before the machine booted.
  Nothing survives a reboot. CHECKED AT BOOT AND ON EVERY SWEEP: a run
  discovered after the boot pass (a spool whose hold expired, say) is exactly as
  dead as one that was open during it, and it ranks ahead of `went_silent`
  because it says HOW we know rather than merely that the file is quiet.

ACTIVITY MEANS THE FILE GREW, and the clock is the file's MTIME rather than the
instant we read it. Our own read is not evidence of life: a spool full of
pre-reboot bytes is not alive because we got round to reading it, and stamping
the read time onto it would make `swept_up` unreachable for exactly the runs it
exists to conclude.

A VANISHED FILE KEEPS ITS TAILER until its terminal has been stated. The
converter that spells the terminal is reached through the watcher entry, so
dropping the tailer the moment the file disappears turns every `file_vanished`
conclusion into "no terminal for the LOST run". It is dropped in `lostEntries`,
after the statement. A run that merely went quiet keeps its tailer for good: the
file is still there and anything appended later must still land.

It RE-DERIVES FROM FILES AND CURSORS because there is nothing else: the store
holds no open-task snapshot for the sidecar. A run is keyed by its resolved
path; a terminal READ FROM THE FILE ITSELF (a spool's `EXIT=` marker) settles
the run so it can never afterwards be concluded LOST. A transcript is never
armed: it is an agent's own record and its silence concludes nothing.

The package MINTS NO RECORDS. A sweep returns OBSERVATIONS; spelling one as the
run's terminal is conversion and happens behind the seam.

- THE ARM IS ON THE WIRE (landing 3): `DetachedLost {file_vanished |
  went_silent | swept_up}` rides `AgentBashInterrupted.cause.lost` and
  `AgentSubagentFailure.cause.lost`, so HOW we stopped seeing a run is a
  STATEMENT rather than a log-only fact. An unrecognized reason PANICS rather
  than resolving to an arm: the three arms are the reader's whole vocabulary, so
  a fourth means this package and the policy have drifted, and picking one would
  have the wire assert something nobody observed. `by_user` and `timed_out` name
  DECISIONS and are never used for a LOST run.

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
  `SessionID`, `Path`, `Kind`, `TaskID`, `SpoolDir`, `RunID`, `RunActivityID`.
- FOUR OPTIONAL interfaces, adopted by adding a method. All take plain function
  and string arguments so neither package imports the other:
  - `SetTaskObserver(func(taskID, toolUseID, agentID, outputPath string, backgrounded bool))` —
    the converter reports each spawn it reads off a tool result; the reader
    turns it into a spool's owner. ONE CALL PER OBSERVATION, never a map two
    packages share.
  - `SetTaskStopObserver(func(taskID string))` — the converter reports each
    TaskStop result it reads; the reader mints the cancelled terminal through
    the spool's OWN handler, because the terminal owes the output the spool
    holds and the transcript's converter never reads those bytes.
  - `LostTerminal(taskID, runActivityID, ownerAgentID, reason string) []*storev1.StoreEntry`
    — the converter spells the reader's LOST conclusion as the run's terminal.
    It REFUSES without a run activity id and never falls back to the task id: a
    terminal on a row no reader can join to the call is worse than none.
  - `CancelTerminal(taskID, run, ownerAgentID string, settledAtMs int64) []*storev1.StoreEntry`
    — the same shape for a person's stop, carrying the bytes read so far.
  - `SetTerminalObserver(func(path, run string))` — the converter reports that
    it READ a run's own terminal off the file, and the reader untracks the run
    so the staleness sweep can never restate a finished run as LOST. The report
    is honored only once the batch carrying the terminal is DURABLE.
- ONLY A TRANSCRIPT CARRIES A LAUNCH OR A STOP (both are tool results), so a
  transcript converter adopting neither observer is an ERROR — it is the only
  source the reader has — while a spool or journal converter's silence is
  ordinary and recorded at verbose.
- A converter that should have adopted one and did not is never a silent
  degradation: the reader states exactly what it could not hand over.

## Identity and keys

- MAIN AGENT identity: `AgentId.value` is the transcript FILE's session uuid
  (the `<vendor session>.jsonl` basename), NEVER the per-record `sessionId`
  field, which diverges from it in roughly a fifth of records. Transcript
  divergence never rides the wire.
- SUBAGENT identity (the CROSS-PLANE MINTING RULE, binding on every producer):
  `AgentId.value` is the `tool_use_id` of the call that SPAWNED the agent, read
  from the companion `agent-<id>.meta.json`'s `toolUseId`. The `agent-<id>` of
  the file name is a LOCATOR (kept as `Target.VendorAgentID`) and NEVER an
  identity, and neither is the per-record `agentId` — reading either as one
  gives an agent a file-plane book the stream plane never writes to, which no
  consumer can reconcile. The bytes coincide with the spawn unit's activity id;
  the spaces stay distinct. THERE IS NO FALLBACK: a meta file that is missing,
  unparsable, or names no `toolUseId` HOLDS its transcript, and a sidechain that
  reaches the handler with no identity converts NOTHING.
- The meta file's parse is exactly four camelCase fields — `agentType`,
  `description`, `toolUseId`, `spawnDepth` — and NO model: the model is not
  stated there at all and comes only from the transcript's own assistant lines
  (`message.model`).
- `top_level`: main-agent frames name the main agent; sidechain frames name the
  owning session's main agent UNLESS the spawn was backgrounded, in which case
  the subagent itself. UNSET only when genuinely unresolvable — residue that
  names no agent.
- RESIDUE keys (ruled): `residue:<vendor record uuid>` where the record has a
  uuid — THE STREAM PLANE KEYS THE SAME RECORD IDENTICALLY, so both planes'
  writes collapse onto one row instead of standing beside each other as two
  copies of one unconvertible line — and `residue:file:<normalized path>:<byte
  offset>` where it has none (an unparsed line, a spool's raw bytes), which is a
  deliberately separate space so no path can collide with a uuid. A KEEP-ALIVE
  is not residue: it is a well-formed fact with no book and keeps the key of the
  unit it would have been.
- `write_id` is DETERMINISTIC: hex sha256 of
  `"shim-claude-sidecar|" + file_id + "|" + offset + "|" + discriminator`, where
  `file_id` is the file's `dev:inode` identity and the discriminator
  distinguishes multiple entries minted from one record. RANDOMNESS IS
  FORBIDDEN — replay absorption rests on this. IT DIGESTS THE FILE ID, NEVER THE
  PATH (ruling R-S1): the cursor is keyed by `dev:inode`, so a RENAMED file is
  resumed from its cursor — and a path-derived write identity would mint fresh
  ids for every record replayed after the rename, storing the whole re-read turn
  a second time. The cursor's identity and the write's identity must be one
  identity.
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
  `request_id` stays (the sidecar serves no inbound rpc, so it emits none).
- A REFUSAL RECORD CARRIES BOTH `refusal_kind` AND `refusal_site`. They answer
  different questions: the KIND is the store's oneof arm and says whether a
  retry can help; the SITE is which call was refused and is what joins the
  sidecar's record to the store's own record of the same refusal.
- THE OUTAGE LADDER'S LEVELS DESCEND: the FIRST refused recovery attempt of an
  outage is an `error`, every attempt after it is a `warn`, and both carry
  `attempt` and `backoff_ms`. None of them is verbose — an outage visible only
  with verbose emission on is an outage nobody sees. Recovery closes the window
  with exactly one `info` record.
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

THE TWO SUITES, AND WHY THE INVOCATION MATTERS. `./...` is the union of the
unit packages and `integration/`, run once each — it is not a third suite. The
integration package carries no build tag on purpose (a tag would take it out of
`go vet ./...` and out of `make coverage`, which runs `go test -coverpkg=./...
./...`), so naming it separately is naming it AGAIN:

```bash
go test ./internal/... .            # the unit suites alone (~0.3s)
go test ./integration/              # the integration suite alone (~12s)
go test ./...                       # both, once each — NOT unit + a rerun
```

Run the union, or run the halves; never both in one pass.

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
- A fixture whose mtime matters (the LOST policy's activity clock IS the file's
  mtime) must be stamped on the harness clock with `os.Chtimes`.
- The integration suite exercises the LOST arms through the window flags above
  (`integration/lost_policy_test.go`), with every wait a bounded receive on a
  real signal — a cursor advance, a store read, a log line — never a sleep.
- `make coverage` reports per-function and aggregate statement coverage for this
  module, and `modules/app/agent-repl/bin/test-all.sh` runs every tracked suite
  across the module. Both are available for a review pass; neither is a gate.

### Test wait bounds

Every harness timeout is a small multiple (~3x) of the healthy max observed on
a clean `go test -count=1 -json ./integration/` run, never a round number picked
by feel. Re-derive them the same way after a change materially alters a suite's
real timing (a heavier scenario, a new outage ladder) rather than nudging a
number that started failing.

A BOUND IS A FAILURE BOUND, NOT A COST. If a green run PAYS a bound, it is not
a bound — it is a sleep with a justification attached, and the fix is a signal
to end on, not a smaller number. Two of the rows below were exactly that and
are now signals.

Measured on `go test ./integration/ -count=1 -json`, green, at the package's own
parallelism: suite wall **11.8s**, slowest whole subject **2.82s**, slowest
subtest **0.87s**.

| Bound | Where | Old | New | Basis |
| --- | --- | --- | --- | --- |
| `waitBudget` | `integration/helpers_test.go` | 50s | **10s** | ~3x the slowest whole subject (2.82s, `TestMockKeepAliveTurnsNeverReachAPage`; 0.92s alone). A subject's own wall time bounds every wait inside it. The old 50s cited a ~16.6s max for `TestMockScenarios/!subagent`, which measures **0.33s** — the number was ~50x its own premise. Costs nothing on green. |
| `snapshotBudget` | `integration/helpers_test.go` | 1s | 1s, **no longer paid** | Unchanged as a number and no longer reached: `watchBashRun` ends on the row count its caller read off the wire, and cancels its context BEFORE closing the stream (closing first waits for the server, which is how the whole budget used to be paid on the way out of a healthy call). Three subjects paid 1s each on every green run; they now pay ~0. |
| `standDownGrace` | `integration/mock_helpers_test.go` | 3s | 3s | Unchanged. The mocked vendor writes every file synchronously and exits promptly on SIGTERM; "did not leave within" has never appeared in a green run. Costs nothing on green. |
| `growthSilence` | `integration/lost_policy_test.go` | (was `shortSilence`, 150ms) | **750ms** | ~5x the worst observed iteration (~150ms under this package's parallelism; ~10ms quiet) of the one subject that must KEEP a file alive across a silence window. INHERENT: the policy under test IS a silence window. Every other short-window subject keeps `shortSilence` (150ms). |
| `RecoverBackoffMin`/`Max` (suite) | `integration/helpers_test.go` | (no override existed) | **5ms / 20ms** | The same ladder shape — first rung, doubling, ceiling held forever — at a scale the suite observes rather than sits through. Production's 250ms/10s is untouched; see `--recover-backoff-min` / `--recover-backoff-max`. |
| `UnownedSpoolWindow` (suite) | `integration/helpers_test.go` | 200ms | 200ms | Unchanged, and now used by the spool-ownership subject too, which rode its own 2s. Its "held is not tailed" assertion is an ORDERING on the write stream, so no window length can make it race. |

Per-site exception, deliberately NOT tightened by this pass:

- `pollTick` (`integration/helpers_test.go`, 20ms) is a poll cadence, not a
  failure bound — tightening it would only add CPU and log churn, not
  correctness margin.
- `rpcTimeout` (`cycle.go`, 30s), `dialTimeout`
  (`internal/storeclient/client.go`, 5s), `UnownedSpoolWindow` (`held.go`,
  60s default), `DefaultPollInterval`/`DefaultRescanInterval` (`main.go`),
  `DefaultGrace` (`internal/stale/stale.go`, 30s) and `recoverBackoffMin` /
  `recoverBackoffMax` (`cycle.go`, 250ms / 10s) are production runtime
  defaults, not test-only harness bounds, and are out of scope: they govern
  the cycle's real behavior, and tests exercise them through the injected
  clock (`sidecar.now`/`sidecar.jitter`/`sidecar.bootTimeMs`) or explicit
  per-test overrides (`--poll-interval`, `--rescan-interval`,
  `--unowned-spool-window`, `--recover-backoff-min`, `--recover-backoff-max`),
  never by waiting them out.

### Parallelism, and the interlock that replaced the long timers

Every subject in `integration/` declares `t.Parallel()`. They were always safe
to: each owns its own `t.TempDir()` trees, its own randomly-named sockets, and
its own sidecar and store processes, and the only package-level state is
`sync.Once`-guarded build output written before `m.Run`.

- **The mocked-vendor drives are bounded structurally**, not by `-parallel`.
  One scenario is FOUR real processes, and the table has 133 rows.
  `takeMockDriveSlot` (in `generateMock`, so no call site can escape it) caps
  concurrent drives at `GOMAXPROCS/2`, floored at 2 and capped at 8. `-parallel`
  only decides how much of the rest of the package overlaps with them.
- **A subject that must act between two things the sidecar does back to back
  stops the sidecar, it does not out-run it.** `writeGate`
  (`integration/helpers_test.go`, on both the fake store and the recording
  proxy) withholds the ANSWER to one chosen batch. A batch is written
  synchronously inside the cycle and the cursor advances only on a durable
  success, so a store that has not answered is a cycle that has not moved on.
  That is what the hold subjects use instead of a 500ms (or 5s) poll interval.
  Release the gate inside the subject: a process frozen in a withheld write does
  not see SIGTERM until `rpcTimeout` expires, so `sidecarProc.Kill` exists for
  the subject that must end one there.
- **Never wait on a count where you mean a set.** `awaitBookLines(…, 4)` is
  satisfied by any four lines; `awaitBookUnits(…, ids…)` waits for the units the
  assertions actually read. The same rule cost four captured-transcript subjects
  their determinism the moment they ran concurrently.

## Conversion rules

Owned by `internal/convert` and `internal/handler`. The reader (this package's
root, `internal/tail`, `internal/discover`, `internal/storeclient`) decides which
files exist, where it has read to, who owns a spool and when it stopped seeing a
run; the conversion decides what a record MEANS. Everything crossing between them
crosses `tail.Handler`, `tail.Context`, and the two optional methods in
`internal/handler/seam.go`.

### The four outcomes a record can have

A page line, a detached run's frame, an unserved item, or — for the exempt set
alone — a drop. There is no fifth, and nothing on disk is ever silently lost.

- A **page line** names its book (`StorePageLine.page_agent_id`). A subagent's
  constituents form ITS OWN book; the SPAWN that created it is a line in the
  parent's.
- A **run frame** (`StoreAgentBash`) wraps the spawning call's unit id and is
  structurally unpaginatable.
- An **unserved item** is a keep-alive turn's item (no book), `vendor_specific`
  (understood, deliberately not carried — the follow-up is a CONVERTER),
  `unknown` (parsed, not modeled — the follow-up is a MODEL), or `unparsed`
  (unreadable — a FAILURE, carrying source, offset, parse_error and bounded raw).
- A **drop** is the exempt set only. Never residue, never `AgentUnmodeled`.

A recognizable modeled kind reaching `unknown` is a PRODUCER DEFECT. The
golden-corpus test asserts the `unknown` set is EMPTY and the `vendor_specific`
set is exactly the declared list, so a mapping that degrades into residue fails
the suite rather than quietly shrinking what the feed can show.

### Identity (R9)

- MAIN AGENT: `AgentId.value` == the transcript FILE's session uuid (the
  `<session>.jsonl` basename), RESOLVED THROUGH THE SHIM'S IDENTITY RECORDS.
  NEVER the per-record `sessionId`, which diverges from the runtime's answer in
  ~22% of records; that divergence never rides the wire.
- A ROTATION DOES NOT MOVE THE BOOK. A `/clear` (and a `forkSession`) mints a
  new vendor session id and a new transcript file, and NOTHING IN EITHER FILE
  LINKS THEM. The shim writes the link the files lack, under `--state-dir`:
  `shim/<workspace-key>/agent-id.json` names the conversation's
  `original_vendor_session_id`, and `shim/<workspace-key>/vendor-id/<id>.json`
  names the original a rotated id belongs to. `internal/identity` reads them;
  a rotated id books under its original, an id no record names books under
  itself (R9's resume rule). THE WORKSPACE KEY IS ENUMERATED, NEVER DERIVED:
  the reader holds only the vendor's lossy, non-invertible cwd slug, so it
  globs `<state>/shim/*` and takes the key from the records.
- THE BOOK IS RE-RESOLVED BY EVERY POLL AND EVERY RESCAN (`cycle.go`'s
  `rekeyRotations`, called from `pollAll` and `rescan`), because discovery order
  is not causal order: a link that lands after the transcript was first seen
  moves the watched file's book, and UN-PARKS it when the refusal that parked it
  was that very book move — the one refusal that stops being true. Nothing is
  duplicated, because `write_id` and `upsert_key` are digested from a file
  position that did not move.
- IT SHARES THE READ'S CLOCK, NOT DISCOVERY'S, and that is the whole point. A
  record read under a book the link file has already superseded is committed,
  advances the cursor, and — for a file the store never parked — is never read
  again, so a resolution on the slower rescan tick would leave one conversation
  split across two books for good. Tie it to the read and no byte is converted
  under an identity the disk has already contradicted.
- SUBAGENT: `AgentId.value` == the `toolUseId` of the companion
  `agent-<id>.meta.json` — the `tool_use_id` of the call that SPAWNED the agent
  (the cross-plane minting rule; see "Identity and keys"). The vendor `agentId`
  of sidechain records, which the `agent-<id>` file name repeats, is a LOCATOR
  and NEVER an identity. There is no fallback: a meta that is missing,
  unparsable, or names no `toolUseId` HOLDS its transcript.
- `top_level`: the main agent for main-agent and sync-subagent frames; the
  subagent ITSELF when the spawn was backgrounded (its stream outlives the turn).
  UNSET only when genuinely unresolvable — residue naming no agent.
- Vendor identity (uuids, message ids, task ids) never crosses the contract.
- Every identity is READ FROM `tail.Context` (`MainAgentID`, `AgentID`,
  `SpawnBackgrounded`, `FileID`), defensively — empty means the reader has not
  supplied it — with path-derived fallbacks. Re-deriving what the reader already
  resolved is how the two halves of the seam come to disagree about whose book a
  record lands in.

### Keys and the write identity

- `write_id` = hex sha256 of `"shim-claude-sidecar|" + file_id + "|" + offset +
  "|" + discriminator`, where `file_id` is the tailed file's `dev:inode`
  identity — the SAME identity the store's cursor row is keyed by (ruling R-S1).
  DETERMINISTIC: randomness is forbidden, because replay idempotence at the
  store rests entirely on the same bytes minting the same id. The discriminator
  separates the several entries one record mints (a block index, `settle:<id>`,
  `terminal`, `diag`). A RECORD WITH NO FILE POSITION gets a RUN-SCOPED
  identity instead: `sha256("shim-claude-sidecar|run:<run>|" + discriminator)`,
  carrying no offset at all. Exactly one record is like that — a terminal
  concluded from the ABSENCE of a file (a run swept up at boot, a spool that was
  never readable) — and inventing offset 0 for it would claim a byte nobody saw.
  Without it every inferred terminal in a process digests ONE id and the store,
  whose absorption is write_id equality, swallows the second run's terminal as a
  replay of the first, leaving that run open in every reader downstream. It is
  set through `Attribution.WriteScope`, and a file id ALWAYS wins over it. A
  frame carrying NEITHER is a READER defect and is raised as one; digesting an empty string would collapse every
  file onto one identity space keyed only by offset. The residue `write_id`
  minted in `residue.go` follows the same recipe with the `residue`
  discriminator, while its `upsert_key` still names the path.
- `upsert_key`, all of it in `internal/convert/keys.go` because the shim must
  mint the IDENTICAL key for the same unit:
  - `activity:<AgentActivityId>` — the vendor `tool_use_id` for a tool call;
    `<message.id>:<block ordinal>` for a text or thinking block.
  - `question:<tool_use_id of the AskUserQuestion call>` — its own identity space.
  - `terminal:<AgentId>:<record uuid>`, the bash row keys below,
    `session:context_cut:<uuid>`, `session:api_error:<uuid>`.
  - Residue with no unit identity is keyed `residue:<write_id>`, so a re-read
    supersedes its own row instead of appending a second copy of the same bytes.
- BLOCK ORDINALS RUN ACROSS THE LINES SHARING ONE `message.id` and reset only
  when it changes, so they equal the SDK's `content_block_start.index` the shim
  sees. Every block consumes an ordinal — including a `tool_use` block with its
  own id and an exempt block producing no unit — or the positions drift from the
  API message and the two planes stop agreeing.

### Frames, upserts and instants

- A frame is an UPSERT OF ITS WHOLE UNIT. A tool_result re-emits the unit's
  settled state under the same activity id, never as a child; nothing in this
  contract has a tool call as a parent.
- An assistant message becomes SEVERAL units (thinking, each text block, each
  tool_use), never one row.
- EXACTLY ONE UNIT PER API RESPONSE carries `usage` and `effort`: the unit for
  block 0 of the response. Every other unit leaves both UNSET, or a consumer
  summing units over-counts the bill by the number of blocks.
- INSTANTS COME FROM THE FILE, never a clock here. Every `started_at` /
  `settled_at` is the record's own timestamp, so a re-read after a restart mints
  byte-identical frames under byte-identical write ids.
- PRESENCE, NEVER SENTINELS: an unreported figure stays UNSET. An absent effort
  is not "low"; an absent retry hint is not "retry now"; an absent sandbox report
  is not "sandboxed"; an absent subagent total is not zero.

### Joins — one indexed lookup each, never a lineage walk

- Tool RETURN → its call by `tool_use_id`. One remembered entry per OPEN call
  (name, input, start instant), deleted on settle, so the map is bounded by
  in-flight calls rather than transcript length.
- SKILL document → its call by `sourceToolUseID` on the isMeta user record.
  Direct and structural; the old skill-name-versus-whatever-arrives-next
  correlation is explicitly retired.
- IDE diagnostics → the last write/edit unit by ADJACENCY. One remembered value,
  sanctioned at the schema because the vendor's record carries no call id.
- Spawned agent → its spawn via `AgentSubagentStart.created_agent_id`.
- An ORPHAN tool_result (its call is behind the cursor) lands as
  `vendor_specific{kind:"orphan_tool_result"}` with a WARNING. It is a genuinely
  lost settle after a restart, so it is loud, not verbose.

### Deliberate departures worth knowing

- THE SPAWN UNIT IS ANNOUNCED AT ITS RESULT, not at its call.
  `created_agent_id` is the join key the flat model rests on and is not optional,
  but the vendor names the created agent only in the launch's answer. Announcing
  with it unset breaks presence; inventing one breaks identity. The settled frame
  carries the ORIGINAL call instant so a drawn clock measures the spawn.
- A SKILL and a MONITOR settle later by design (`settlesLater`): a skill's own
  return is a bare acknowledgement and the DOCUMENT settles it; arming a monitor
  does not end it. This is kept distinct from a settle the converter FAILED to
  perform, or a handled record would be reported as a mapping gap in the very
  query built to find real ones.
- A TASK ACT is instantaneous at this tier: what the tracker did and where it
  left the task both come from the result, so the call announces nothing.

### The exempt set, and its one carve-out

Dropped entirely: `TaskStop`, `TaskOutput`, `TaskGet`, `TaskList`, `ToolSearch`,
`NotebookEdit`, `REPL`, `ListMcpResources`, `ReadMcpResource`, `SendFeedback`.
A drop is not residue: filing a known built-in as `unknown` would pollute the
query that finds real modelling gaps.

CARVE-OUT: the `TaskStop` CALL stays dropped, but its RESULT is CONSUMED as the
owning task's CANCELLED terminal before the drop. Deliberately-stopped work must
resolve cancelled, never LOST.

WHERE each is minted differs, and the reason is which producer holds the
evidence:

- AN AGENT TASK settles IN THE CONVERTER — the spawn unit is a line in this
  stream's own book — as `AgentSubagent.failure.stopped_by_user`, keyed by the
  CALL that spawned it (`activity:<tool_use_id>`), never by the vendor task id.
- A SHELL TASK does NOT: its terminal owes the output the run produced, and
  those bytes are in a spool this converter never reads. The converter reports
  `TaskStopped(taskID)` and the READER mints
  `AgentBash.success.interrupted.by_user` through the spool's own handler,
  keyed `bash:<run>:terminal`, carrying the bytes read so far.

Both branches REFUSE rather than guess when no launch on this stream opened the
task: the record is stored whole as `vendor_specific` (`task_stop/unlaunched`),
because keying a terminal on a vendor task id would settle a row no reader can
join to a call.

### Withholding classes (`vendor_specific`)

CLI bookkeeping and machinery (`mode`, `permission-mode`, `queue-operation`,
`last-prompt`, `ai-title`, `pr-link`, `frame-link`, `file-history-*`,
`attribution-snapshot`, `system/local_command`, harness-injected user records
(`user/meta` — the system reminder and the `<local-command-caveat>` a slash
command's envelope is preceded by), and the informational /
turn_duration / stop_hook_summary / away_summary / scheduled_task_fire /
model-refusal / agents_killed system lines); context-cut exclusions and the other
attachment machinery as `attachment/<type>`; the synthetic
`"No response requested."` assistant record; unmodeled content blocks as
`content_block/<type>`; workflow journal records (`workflow_journal/<type>`),
workflow spools (`spool/workflow`) and a workflow run's PER-AGENT TRANSCRIPTS
(`workflow/agent_transcript`) — workflow is KICKED this wave, so all three are
discovered and tailed so nothing is lost, and converted to nothing yet. A
per-agent transcript holds ORDINARY TRANSCRIPT RECORDS rather than the journal's
two shapes, so running it through the journal converter filed every record as
`unknown` — "we do not model this", which is false and which buries the real
modelling gaps that query exists to find.

R15: a FILE-PLANE USER PROMPT is `vendor_specific{kind:"user_prompt"}`, never a
page line. `AgentPrompt` carries a `TurnId` and a `PromptOrigin`, both
daemon-minted; a file reader holds neither, so no history page can regrow a fake
prompt bubble from this producer. The shim's `AgentPrompt` is the one served form,
and a subagent's commission rides `AgentSubagentStart.prompt`.

### Keep-alive

A user prompt whose first text block BEGINS with
`<!--agent-repl:keepalive-->` marks the turn keep-alive until the next
non-keepalive prompt (one remembered bool per file). Every record converted while
the bit is set lands on `unserved_item.keepalive` — structurally unable to appear
in any page, so no read filters them out and no activity routes onward.

### Context lifecycle, and the one legitimate hold

- `/clear` is detected by UNWRAPPING the expanded command envelope; the literal
  never appears on disk. The envelope must be the ONLY content — an argument or
  surrounding prose means the prompt quoted a command rather than invoking one.
- Compaction COALESCES the `system/compact_boundary` record with the FOLLOWING
  summary line in FILE ORDER. Never timestamp order: the harness composes the
  summary before writing the boundary, so the summary's timestamp is EARLIER, and
  a timestamp-ordered assembly pairs every boundary with the wrong summary in a
  session that compacted twice.
- Both land as `AgentUpdate.context_cut` — a page line of the MAIN agent's book,
  keyed `session:context_cut:<record uuid>`. A clear carries no token delta,
  because the vendor's reset record states none.
- THE HOLD is the only one: a trailing boundary is deferred (cursor parked before
  it) when the reader redelivers, bounded to ONE redelivery, and converted
  without its summary on the forced delivery — loudly.

### API errors

`system/api_error` → `AgentUpdate.api_error` = `ApiRequestFailed{message, kind}`,
a page line keyed `session:api_error:<uuid>`. EVIDENCE, NEVER A TERMINAL: the
turn's end is the frame-level failure arm and nothing else. The kind is the
VENDOR'S taxonomy, read from its `type`, falling back to its numeric status; an
unmodeled type is carried by name, and a transport failure (which has no vendor
type at all) is named `connection/<code>` rather than guessing a modeled kind.

### Detached shell spools

THE RUN IS THE SPAWNING CALL'S `tool_use_id`, never the vendor task id — the one
identity a detached command is announced under on BOTH planes, and equal to the
`DetachedWorkId` a consumer addresses the run by. The reader resolves it from the
launch it observed and hands it over as `tail.Context.RunActivityID`; a spool
that reaches the handler without one is a reader defect (an unclaimed spool is
HELD, not tailed), refused loudly with its bytes still landing as residue.

ONE ROW PER WRITE, never one row superseded:

- `bash:<run>:start` — a start row, if a producer ever mints one. The sidecar
  does not: a spool exists only after the STREAM plane announced the launch.
- `bash:<run>:<from_offset>` — one per delta. The offset IS the delta's
  identity, so the same bytes re-read after a restart supersede their own row
  instead of appending a second copy of the run's output.
- `bash:<run>:terminal` — the single terminal, however often it is restated.

A single `bash:<run>` key would leave the run holding only its most recent
delta, every earlier chunk erased by the next; store.v1 `WatchBashRun` replays a
run's rows in write order, which is only possible if each write is a row.

`from_offset` is a GAP DETECTOR, not addressing: it must equal what the consumer
has already accumulated, and a mismatch means the consumer REFUSES the frame
rather than concatenating across a hole.

`EXIT=<code>` → `AgentBash.success.completed` with `termination.exited`. The
matching is strict (last line of the batch, newline-terminated, line-start, at
most three digits) because `EXIT=` is common as ordinary output — 23 of the 44
real spools carrying it have it only mid-line. The raw codec carries nothing, so
whether a batch BEGINS a line is answered from whether the previous batch ended
on a newline — which the handler alone knows, and without which a marker
arriving on its own poll (the ordinary case) was never detected at all.

A TERMINAL CARRIES THE RUN'S OUTPUT, not the batch's: the handler holds what the
run has said, bounded, and states `partial{bytes_omitted}` past the bound rather
than claiming `whole` over a prefix a consumer cannot detect.

LOST (`file_vanished` | `went_silent` | `swept_up`) →
`AgentBash.success.interrupted` with `cause.lost` naming the arm (landing 3).
`by_user` and `timed_out` name DECISIONS and neither is what happened. A stop the
vendor recorded IS a decision and is the one place `by_user` is set — minted by
the SPOOL's handler, not by the transcript's converter, because the terminal owes
the bytes only the spool holds. A stop for a spool not yet claimed is held as one
pending value per task and applied on claim; it is retired only on a durable
write, and untracks the run at the same moment.

The LOST terminal shares the exited terminal's write identity, so a late LOST
verdict is absorbed rather than appended beside an observed exit.

A non-zero shell exit is COMPLETED, not a failure arm; empty search results are
SUCCESS with an empty answer.

#### A terminal states what it OBSERVED, and `not_observed` when it observed nothing

`AgentBashOutput` has three arms, and they are three different facts:

- `text{stdout: "", whole{}}` — the COMMAND printed nothing. A positive claim
  about the command.
- `text{..., partial{bytes_omitted: n}}` — the CARRIER cut it.
- `not_observed` — THE PRODUCER DOES NOT KNOW. Nothing on disk said what the
  run printed.

A terminal minted for a run whose spool this handler NEVER READ — swept up at
boot, or a file that was never readable — states `not_observed`. It may not say
`text{stdout: ""}`, which would put words in the command's mouth, nor
`partial{bytes_omitted: 0}`, which claims nothing was cut. The converse matters
as much: a run we DID read that printed nothing states empty `text{whole}`,
because that is something we know and downgrading it would throw it away.

Such a terminal is MINTED, never refused. A refused terminal is a run left open
forever in every reader downstream, which is strictly worse than one that
honestly says it saw nothing — and refusing was what made the `swept_up`
conclusion, whose whole premise is a spool nobody read, unstatable.

### Logging

Every logical branch logs. CORRELATION KEYS RIDE DEDICATED `logging.Context`
FIELDS AND NEVER MESSAGE TEXT — a record whose identifiers are interpolated into
a sentence cannot be filtered or joined by the integration loop that reads these
logs. One base helper per package sets producer, path, file id, offset, agent,
vendor session and task (`Attribution.ctxFor/ctxWarn/ctxError` in `convert`,
`handleCtx/handleWarn/handleErr` in `handler`); a call site adds only what is
specific to its branch (`ActivityID`, `UpsertKey`, `WriteID`, `BookAgentID`).
Per-record success paths are VERBOSE; refusals, invariant violations, withheld
classes reaching a warning threshold, and every failure are normal verbosity.

### Suites

```
go test ./internal/convert/... ./internal/handler/...
```

`internal/handler/golden_test.go` is the contract test: it drives every
file-plane corpus fixture under `testdata/corpus/` plus the real transcript under
`projects/` and asserts zero `unparsed`, zero `unknown`, the declared
`vendor_specific` set, all four envelope duties on every entry, write-id
uniqueness, and byte-identical output across two runs. `testdata/corpus/stream/`
is excluded deliberately: those are SDK probes, the shim's input, and converting
them here would test a path production never takes. A shape gap found later
becomes a fixture there FIRST, then a fix.
