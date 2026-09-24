# agent-shim/claude/shim-sidecar/

The Claude FILE-PLANE READER (Go, singleton, launchd-managed). It observes what
the vendor's agent binary writes to disk, tails it with cursored,
truncation-aware reads, converts each record into `conversation.v1` vocabulary,
and writes it to the store as `store.v1.StoreEntry` batches with the reader
position riding the same transaction.

It is a COPIER. It has no view of process liveness, no session semantics, and
owns no database. Its daemon calls are the read-only `WatchWorkspaceRoster`
lookup needed to obtain a daemon-minted ref and `ClientLog` for file-scoped
diagnostics; the daemon persists those records into workspace `sidecar.log`.
The only thing it concludes on its own is that it STOPPED SEEING a detached run.

Dual-plane relationship with the shim: `StoreEntry.plane` names the producer.
The SHIM (stream plane) watches the SDK live — first to know, authoritative for
session and turn LIFECYCLE, and the only source for anything not yet on disk.
The SIDECAR (file plane) reads what the vendor itself recorded — authoritative
for conversation CONTENT, EXCEPT where the transcript's rendering of a unit is
lossy against what the stream plane already holds; that unit is stream-owned and
the file plane writes no part of it (`internal/convert/streamowned.go`). Both write through the same envelope, into one
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

## Build reporting

The sidecar has no connection to the daemon at all, so it reports the build
it is running through a FILE: `reportBuild` (`buildreport.go`) writes
`<run dir>/shim-claude-sidecar.build.json` — this process's pid and the
content hash of its own executable — as soon as the canonical logger exists
and before the sidecar starts its discovery loop. The run dir is
`agentrepl/logging/buildreport`'s `ResolveDir` (`$AGENT_REPL_LOCK_DIR`, else
`~/.cache/agent-repl/run`), the same directory the kernel locks already live
under.

The daemon's deploy reads this file and compares it against the build it just
made to decide whether the launchd-managed sidecar is stale and needs a
restart. A failure at any step (resolving the process's own build, resolving
the run dir, or writing the file) is logged once at `error` through the
canonical logger and swallowed: the sidecar keeps booting regardless, because
a service that cannot report its own build still has every reason to keep
tailing transcripts with the one it has. The daemon simply reads the missing
or stale report as "not running the fresh build".

**Every harness that boots a real sidecar (or the real store it talks to)
must set `AGENT_REPL_LOCK_DIR` to a private directory.** Without it, a real
process spawned by a test resolves the run dir to the developer's actual
`~/.cache/agent-repl/run` and overwrites their real build-report file.

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
  - A REPEAT OF THE SAME DEFECT FOR THE SAME FILE IS RESTATED ONLY ON POWERS
    OF TWO, and every record carries `repeat_count`. A park normally states
    the defect once and that is the end of it — but a park is not permanent:
    the un-park below re-reads the file, and if the identity that decides its
    book oscillates the same refusal returns on every poll. That is a record
    per second for a condition that never changes, which is the exact drowning
    the park exists to prevent arriving through the un-park door. The ladder
    keeps such a defect visible (1, 2, 4, 8 ...) without letting it become the
    log's entire content, and the count says "seen ten thousand times" without
    ten thousand records. A defect naming a DIFFERENT `field` is a different
    bug and is always stated, starting its own tally;
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
- Spools are never rewound: they carry no turns, and a resumed spool's tail
  reseeds its window from the file's prefix (see "Detached shell spools"). When the window holds no turn start the store's cursor stands —
  reading from an arbitrary older position would be worse than not rewinding.

### It is for files that can carry a turn in flight, not for the corpus

The joins the rewind re-warms exist only for a turn this reader was half-way
through. A transcript that stopped growing hours ago, whose restored cursor
already sits at its end, holds none — and rewinding it anyway is how a restart
came to re-read the owner's entire history. Realtest 9, sweep rt-run37: a deploy
restarted the sidecar at 23:44:21, catch-up did not end until 23:46:45, the first
rescan rewound 1353 transcripts of which 1132 had last grown over 108 hours
earlier, and each produced a `residue-drop-summary` for two records it stored
none of.

- A TRANSCRIPT IS REWOUND IFF IT COULD STILL BE MID-TURN: its mtime is within
  the LOST tracker's own `AgentSilence` window, OR its restored cursor is behind
  the file's current end. The second arm is what makes the first safe — durable
  bytes this reader never converted ARE the half-converted turn, however old the
  file is — and the window is REUSED, not duplicated: "an agent has stopped
  working on this" is a bound this process already owns (`Tracker.Windows()`).
- A FILE WHOSE mtime AND SIZE CANNOT BE READ IS REWOUND, and the refusal to read
  them is stated. Nothing there can prove the file cold.
- DISCOVERY IS NOT NARROWED BY ANY OF THIS. Every file is still enumerated,
  classified and watched; a file at rest is watched FROM ITS CURSOR, and the
  ordinary poll reads it the instant it grows. Narrowing discovery "only serves
  to obfuscate inefficiency" (owner's standing rule) and nothing here does.
- THE SHAPE OF THE WALK IS STATED ONCE. Both per-file decisions are verbose, so
  one INFO `boot-rewind-summary` at the catch-up edge carries the two counts —
  rewound, and at-rest — beside the other summaries the walk owes.

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
account's transcripts are invisible otherwise.

### Two discovery paths, and neither one is fsnotify

There is no fsnotify watcher and no per-file kqueue: this process holds NO
descriptor open for a file it is not reading, because thousands of held
descriptors is what the file-table exhaustion of 2026-09-13 was made of (38
ENFILE meta reads in one afternoon). Discovery polls.

- `Scan` (`RescanInterval`, 30s) is the FULL enumeration and the backstop. It
  globs every shape under every root, refreshes the holds and the meta
  re-checks, and is the authority: whatever the probe misses is found here.
- `ScanChanged` (`PollInterval`, 1s, `change.go`) is the ACCELERATOR. It stats
  the directories the globs enumerate and re-enumerates (one `ReadDir`) only
  those whose mtime MOVED, so a NEW file is watched within one poll instead of
  one rescan.

WHY THE PROBE EXISTS. Discovery ran only on the rescan tick, so a transcript the
vendor wrote one instant after a scan waited out the whole 30s before a byte of
it was read. Realtest 9, sweep rt-run36: a fresh workspace's first turn concluded
at 23:22:44, the vendor wrote `projects/<slug>/e37f7527-….jsonl` at 23:22:44, and
`tail-pickup` came at 23:23:14. The daemon, the shim and the webapp had already
drawn the prompt, the turn end and the final-answer mark — only the ANSWER TEXT,
which nothing but this process reads, was thirty seconds late.

- IT IS NOT A NARROWER SCAN. Nothing was removed from what discovery looks at:
  narrowing it "only serves to obfuscate inefficiency" (owner's standing rule),
  and a shorter blanket rescan would spend the whole glob sixty times a minute.
  The probe asks the DIRECTORY, not the files — a new file moves the mtime of the
  directory it lands in, so one stat answers for everything under it at once.
- THE COST IS MEASURED, not asserted. On the owner's machine (2 config roots, 102
  project directories, 2928 targets, a spool root of /private/tmp with 23539
  children) the candidate set is 189 directories, an idle probe costs 0.22–0.32ms,
  and the full `Scan` costs 820ms. So the probe is 189 stats/second against the
  scan's 27ms/s amortized — two orders of magnitude cheaper per second than the
  enumeration it front-runs.
- THE SPOOL ROOT IS DELIBERATELY NOT A CANDIDATE. In production it is
  /private/tmp, whose mtime moves whenever anything on the box writes a temp
  file; probing it would re-crawl 23539 entries on most ticks, which is the
  blanket rescan this design exists to avoid, once a second. Every directory
  BELOW a spool tree the scan has already seen is a candidate, so a new
  `<task>.output` in a known `tasks/` directory is found on the next poll; a
  brand-new spool TREE waits for the next `Scan`, exactly as it did before.
- THE PROBE IS ALLOWED TO MISS, and `Scan` is why that is safe: a write inside
  the microseconds between a scan's glob and its stat, a filesystem whose mtime
  granularity swallowed a write, a directory nothing discoverable has ever been
  put in. Each of those costs what it cost before the probe existed — one rescan
  interval — and never more.
- A VANISHED DIRECTORY IS AN ORDINARY END. Session directories, workflow
  directories and task spools are deleted constantly; a candidate that is gone
  is dropped from the set and states NOTHING. A directory that is real and
  UNREADABLE is a different fact — every file under it now waits for the full
  rescan — and is stated once per directory per condition at `warn`
  (`discover-change`), repeats verbose, exactly like the `discover-meta` holds.
- ITS RECORDS: one `discover-change` INFO when a directory change actually led to
  a newly WATCHED file (naming the directory and the count, written by the cycle,
  which is what knows), and one `discover-change` DEBUG per tick otherwise
  (candidates stat'd / changed / re-enumerated). A 1Hz probe contributes nothing
  to a normal-verbosity log.
- BOTH PATHS BUILD TAILERS THROUGH ONE FUNCTION. `cycle.go`'s
  `watchTargets` is the only place a tailer is ever built and it asserts the
  store-unreachable invariant; `rescan` and `discoverChanged` are its only two
  callers. The cycle header used to say "rescan is the only thing that builds a
  tailer" — the probe EXTENDED that contract explicitly rather than routing
  around it, and is bound by every clause of it (including abandoning the rest of
  the pass when the store cannot answer for a file's cursor).

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
  transcript, `w*` a workflow journal. ANY OTHER PREFIX IS A MAPPING THIS
  READER IS MISSING (ERROR record, once per path). It is still discovered as
  `tail.KindResidueSpool` so the reader states its decision, and it is NEVER
  READ: no conversion can be selected, so nothing could render it (see "Only
  what is rendered is read").
- A TRANSCRIPT THAT IS GONE BEFORE ITS FIRST BYTE IS AN ORDINARY END, NOT A
  HELD ATTRIBUTION (owner ruling, 2026-09-13). Workspace attribution is the
  FIRST thing done to a discovered transcript, and a vendor session directory
  deleted between the scan and that read leaves nothing to attribute and nothing
  to wait for — no bytes were skipped, because none were ever read. So an
  `fs.ErrNotExist` from the attribution read is stated at INFO with
  `reason=transcript_vanished`, once per file, and summarized when it is startup
  backlog. A transcript that is PRESENT and cannot be attributed keeps its
  WARNING: that one is a real hold an operator must look at.
- A TRANSCRIPT WITHOUT ITS META IS HELD, NEVER DROPPED: `agent-<id>.meta.json`
  is the ONLY source of the agent's type, spawn depth, model and worktree, so
  the transcript is discovered, stated ONCE, re-checked every rescan, and not
  tailed until the meta appears.
- THE VENDOR WRITES ONE FILE NAME FOR TWO DOCUMENTS, and the second is not a
  defect:
  - the SUBAGENT shape (`subagents/agent-<id>.meta.json`) always states
    `toolUseId` beside `agentType`, `description` and `spawnDepth` (and,
    variously, `parentAgentId`, `model`, `isFork`, `cwd`, `stoppedByUser`,
    `worktreePath`/`worktreeBranch`/`spawnedWithWorktree`/`inheritedWorktreePath`,
    `worktreeCleanlyRemoved`). `toolUseId` IS the identity.
  - the WORKFLOW shape (`subagents/workflows/wf_<id>/agent-<id>.meta.json`)
    states `agentType` (`workflow-subagent`), `spawnDepth` and `model`, plus
    `worktreePath`/`spawnedWithWorktree` when the agent got a worktree. IT
    CARRIES NO `toolUseId` AND NO `description`, because NO TOOL CALL SPAWNED
    IT. Such an agent is attributed to its WORKFLOW RUN and PARENT SESSION,
    both of which the path already carries, and its transcript is ingestible —
    as workflow residue, which is all workflow converts to this wave. Holding
    it for an id the vendor never writes held every workflow agent forever.
  - a meta naming NEITHER a `toolUseId` nor an `agentType` names nothing at
    all, and so does a workflow-shaped meta found OUTSIDE a `wf_<id>`
    directory: both are held, at ERROR, because they will not fix themselves.
- A HOLD IS A CONDITION, NOT AN EVENT. Every rescan re-evaluates every
  transcript, so a per-pass warning is a record per file per pass forever —
  one stuck agent wrote 4988 identical `discover-meta` records in 13 minutes.
  The per-transcript hold state (`internal/discover`, keyed by path, carrying
  the REASON, the instant it began and a repeat count) makes each record say
  something new: the FIRST hold is stated at its own level (`warn` for an
  absent meta, `error` for one that is present and unusable), a hold whose
  REASON CHANGED is stated again at `warn` (it is a different fact about the
  same file), a repeat of the same reason is VERBOSE and carries
  `repeat_count`, and a RELEASE is an `info` lifecycle edge. Every one of them
  carries the hold `reason` in its own context key. The STANDING set is
  restated as one periodic `discover-holds` info record ("N transcript(s) held
  ...; oldest since T"), bounded by `DefaultHoldSummaryInterval`; nothing held
  states nothing.
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

- `b*` — a detached SHELL spool. Raw bytes, `KindShellSpool`, its rendered
  tail keyed `bash:<run>:tail` under the spawning call's tool_use_id.
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
- anything else — an unmapped vendor task kind. Logged at ERROR once, stated as
  `spool-skip` `reason=unrecognized_prefix`, and never read.

## Owner resolution and the held spool

`owner.go`, `held.go`. A spool's owner is looked up by task id against the
spawning call the converter read out of a tool result, and by the exact output
path the vendor named — nothing else. Filename similarity is deliberately not
evidence and is never consulted. A task two different calls claim resolves to
NOTHING (ERROR record): guessing between two claims is how one run's output
lands in another run's card.

- An unclaimed spool is HELD: discovered, re-checked every rescan, not read.
- AN AGED UNOWNED SPOOL IS STILL NOT READ. Past the hold window
  (`UnownedSpoolWindow`, replaceable with `--unowned-spool-window`) its lapse is
  stated ONCE (`hold-expired`, INFO, `reason=spool_unclaimed`) and it stays
  held: re-checked every rescan and read FROM ITS START the moment a launch
  claims it. It used to be read whole, tailed forever and dropped at the write
  path as residue — large growing test logs costing a read and a cursor write
  per poll for rows nobody stored. A spool ALREADY on disk when the reader first
  scanned is startup backlog and is summarized instead (see "Startup catch-up").
- A CLAIM IS STATED: `spool-claim` (INFO, path, task, activity) names the kind
  the spool is read as. A refusal (two claims, a path mismatch) is stated at
  ERROR once per path and verbose on every later rescan.
- A RENAMED CLAIMED SPOOL IS FOLLOWED by its `dev:inode` identity
  (`followRename`), never by its name: the same identity as the watched old
  path re-points the claim, anything else is resolved as a new spool.

### Only what is rendered is read

Owner rule, 2026-09-23: "store output files efficiently; anything not needed
for rendering isn't needed at all". `held.go unrenderedSpool` is the one place a
spool is decided never-read, each decision stated once per path as
`spool-skip` (INFO, `path`, `task_id`, `reason`; repeats verbose):

- `unrecognized_prefix` — no a/b/w prefix, so nothing could render it.
- `transcript_symlink` — an `a*` spool that is a LINK to the subagent's own
  `subagents/agent-<id>.jsonl`. Discovery normally resolves the link onto that
  transcript (one target, `discover.Normalize`); a link whose transcript does
  not exist yet is still seen under its own spelling and is skipped here, so the
  transcript is never copied twice.
- `spool_vanished` — gone before it could be examined.
- An `a*` spool whose link-ness cannot be established (`Lstat` fails for any
  reason but absence) is not read this pass, stated at WARN, and examined again
  next rescan.

A claimed `b*` spool is read for its `bash:<run>:tail` and terminal, which the
daemon's detached-shell bubble renders. Residue rows are unchanged: none is
written, and none already stored is touched.

## Startup catch-up summarizes the backlog

A HOLD IS A CONDITION, NOT AN EVENT — and so is a LOST run and a transcript
whose workspace will not resolve. A restarted sidecar re-derives the owner's
whole historical corpus from files: hundreds of spools whose spawning sessions
are long gone, runs that went silent weeks ago, transcripts from directories
that no longer exist. Each of those was a single WARNING once, long ago; a
restart that re-stated every one wrote 1063 warnings in one realtest window (405
`hold-expired`, 403 `lost-policy`, 210 `resolve-transcript-workspace`). This is
the same inverted-pyramid flood-on-rescan the `discover-meta` holds already
leveled, arriving through three more paths.

- THE BOUNDARY IS THE FIRST PRODUCTION CYCLE. `beginCycle` stamps
  `processStartMs` off the cycle's own clock, once, the first time reading
  begins; a store bounce that re-enters the cycle leaves it where it is. Both
  the LOST tracker (`stale.SetProcessStart`) and the rescan-driven paths key on
  that one instant.
- AN ITEM IS BACKLOG IFF ITS OWN CLOCK PREDATES THE START. The clock is the
  file's mtime (a spool's, a transcript's) or the run's last-activity mtime,
  never our read time — the same fact `swept_up` already reads. A backlog item's
  stale condition was already true before we started, so it is CATCH-UP; an item
  whose clock is at or after the start went stale WHILE we watched, so it is a
  newly-arising condition.
- CATCH-UP IS SUMMARIZED, STEADY STATE IS STATED PER ITEM. A catch-up
  conclusion is accumulated per class and stated once, at WARN, through the one
  `catchup-summary` operation, carrying the class in `reason`, the count in
  `repeat_count`, and the oldest item's age in the message
  (`spool_unclaimed`, `workspace_unattributed`, and the LOST arms
  `went_silent`/`swept_up`/`file_vanished`). A steady-state item is stated per
  item exactly as before, at that item's own level (`hold-expired` is INFO).
- NOTHING IS SILENCED. Every catch-up item is still stated, dropped to DEBUG, so
  the per-file detail is retrievable behind the summary, and the totals always
  ride the summary. The owner still sees "405 spools expired" — just not as 405
  lines.
- THE SUMMARY IS A PER-PASS EDGE. The rescan-driven tallies
  (`catchupSpools`, `catchupWorkspaces` in `held.go`) are flushed at the end of
  each rescan (deferred, so an abandoned pass still reports what it demoted) and
  reset; the LOST tracker summarizes the backlog subset of each sweep inside
  `state`. An empty backlog states nothing.

### The catch-up window levels the corpus walk itself

The rules above level the CONCLUSIONS a restart re-derives. The WALK that
re-derives them floods the same way: a restart rewinds every transcript, picks
up every restored byte, re-reads every spawning call in those bytes, re-holds
every unclaimed spool and re-concludes every dead run. One boot generation on
the owner's machine held 9,337 `launch`, 9,337 `record-spawn`, 7,128
`hold-spool`, 6,748 `tail-pickup`, 6,172 `boot-rewind` and 4,789 `lost-policy`
INFO records and rolled the 64 MB durable log five times over.

- THE WINDOW IS A PROCESS-WIDE PHASE, NOT A PER-ITEM TEST. These six operations
  span four packages and none of them holds the item's own clock, so the
  boundary is the boot walk rather than each file's mtime. `catchupOperations`
  (`cycle.go`) names the six; `logging.Logger.BeginCatchup` opens the window
  before any file is read.
- ONLY INFO IS LEVELED. A WARN or an ERROR raised during the walk is a real
  conclusion and keeps its level. An operation the window does not name is
  untouched — this is a named list, never a blanket quieting of the boot.
- THE WINDOW CLOSES ON THE FIRST DRAINED POLL PASS. `pollAll` latches
  `drainedPass` only where it falls out of its loop having walked EVERY watcher;
  an abandoned pass (a store outage, a shutdown) has not drained the corpus, so
  the window stays open. Both latches are process-lifetime: a later store bounce
  must not reopen a window whose summaries were already stated.
- A PASS IS NOT A TICK. The walk is sliced (see "A poll pass yields the tick"),
  so `drainedPass` is about the PASS: it latches when the last of the roster the
  pass enrolled has been polled, across however many ticks that took.
- THE CLOSE STATES ITSELF. `EndCatchup` writes one INFO `catchup-summary` per
  operation that demoted anything (`reason` names the operation, `repeat_count`
  the total; an operation that demoted nothing states nothing), and `cycle.go`
  then writes one INFO `catchup-end`. A subject asserting a STEADY-STATE
  per-item record waits on `catchup-end` (`awaitCatchupEnd`), never on the first
  production cycle — between the two the walk is still draining.
- NOTHING IS SILENCED, HERE EITHER. The demoted record is written in full at
  DEBUG, so a subject that asserts the per-item detail reads the log with
  `AGENT_REPL_LOG_LEVEL=debug`.

## A poll pass yields the tick to discovery

`pollAll` used to walk the whole watched set to completion inside one tick. On a
restart's corpus walk that tick lasted MINUTES, and nothing else on the poll
timer ran while it did: not `discoverChanged`, so no new transcript was found,
and not the first poll of a file that had just been found. Realtest 9, sweep
rt-run37: the boot walk ran 23:44:21–23:46:45, a fresh workspace's turn concluded
at 23:46:41 inside it, and its answer rows reached the store at ~23:47:44.

- ONE PASS, MANY SLICES. A pass polls watchers in a stable order for at most
  `PollInterval / 2` and resumes at the next pending watcher on the following
  tick. The bound is DERIVED, never configured: a fraction of an interval the
  operator already chose cannot be misset against it.
- THE FIRST WATCHER OF A TICK IS ALWAYS POLLED, whatever the bound says. A slice
  that expired before any work was done would be a pass that never advances.
- A FILE DISCOVERED MID-PASS IS ENROLLED AT THE HEAD, which is the whole point:
  behind a boot walk's thousands of watchers it would wait out the very minutes
  the change probe exists to save.
- THE PASS OWNS "EVERY WATCHER, EXACTLY ONCE". `pollPass.seen` is the roster it
  enrolled and `pending` what is left of it, so a resumed pass neither re-walks
  what it did nor skips what it has not — which is what `drainedPass`, and so
  the catch-up window's close, stands on.
- A SUSPENSION RETIRES THE PASS with the tailers it was walking, so an abandoned
  pass can never close the catch-up window. `rekeyRotations` and the
  abandon-the-pass arms are unchanged.
- IT STATES NOTHING NEW AT INFO. The spent slice is one DEBUG record naming how
  many watchers it walked and how many remain.

## Shutdown is bounded

A stop that is asked for is a stop that happens.

- A STORE WRITE ALREADY ON THE WIRE IS NOT CANCELLED AT ONCE. The signal gives
  it `shutdownSettle` (cycle.go, 250ms) to answer, because cancelling a sent
  write does not RECALL it: the store's transaction is its own, and net/http
  tells the handler the client is gone only once the connection closes, which
  for an exiting process is after it has exited. An instant cancel therefore
  destroyed nothing but this process's knowledge of whether the batch landed —
  and left the store committing a batch nobody was waiting for, which is how an
  ordinary two-rpc read of the store (book, then cursor) came back looking like
  a cursor standing past records that were never stored.
- WHAT MAKES AN ABANDONED WRITE SAFE IS THE STORE'S TRANSACTION, NOT THE
  CANCEL. Records and cursor advance commit together, so the store holds both or
  neither; a write this process never got an answer for is UNKNOWN, not torn,
  and the next boot resolves it by resuming from whichever cursor the store
  holds. The shutdown record says exactly that and never claims the write was
  undone.
- A STORE CALL THIS PROCESS WITHDREW IS NOT A STORE FAILURE, ON EITHER VERB.
  `context.Canceled` can only come from the sidecar cancelling its own cycle
  context on the way out — a call the store failed to answer in time comes back
  `DeadlineExceeded` — so `storeclient` states the withdrawal at DEBUG and the
  narration belongs to the layer that knows a shutdown is running: `storeWrite`
  for a batch, `attempt` and `cursorFor` for a cursor recovery, each one INFO
  `shutdown`. The error value is returned to the caller unchanged either way,
  and a DEADLINE keeps its ERROR because that is a fact about the store.
- THE LOG DRAIN IS THE ONE UNBOUNDED WAIT, AND IT IS NOW BOUNDED.
  `logging.Logger.Close` waits for the forwarding queue to drain, and the
  closing forward loop probes and dials the daemon ONCE PER QUEUED RECORD. With
  the daemon gone and a boot's backlog queued (42,044 undelivered records in one
  generation) that wait ran past three minutes while launchd waited on a service
  it had already asked to stop. Production calls `CloseWithin`
  (`logging.DefaultShutdownDrain`, 5s) — a large multiple of every healthy
  teardown observed here, which is sub-millisecond from the `shutdown` record to
  the `exit` record.
- A BOUND THAT FIRES IS STATED. `CloseWithin` writes one INFO `shutdown-drain`
  record naming how many records were still queued and the daemon address it was
  forwarding to, through the durable sink, then exits.
- LAUNCHD HAS ITS OWN CEILING. Both plists state `ExitTimeOut` (20s)
  explicitly rather than leaving it to a default. The service's own bound sits
  well inside it, so reaching launchd's timeout means the process is stuck
  somewhere it has no bound of its own and SIGKILL is the right answer.

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

A CONCLUSION ABOUT A BACKLOG RUN IS CATCH-UP, NOT A NEW EVENT. A restart
re-derives every historical run at once, so a run whose last activity predates
`processStartMs` is summarized rather than stated on its own (see "Startup
catch-up"); only a run that went stale while the sidecar watched is a per-item
WARNING. `state` partitions each sweep — the boot sweep included — into the two.

A FILE THAT WENT WITH ITS WHOLE TREE IS AN ORDINARY END, NOT A LOSS (owner
ruling, 2026-09-13). When a watched file disappears the reader stats its PARENT
DIRECTORY. A directory that is gone too means nobody unlinked a file out from
under us — a harness deleted its run directory, a vendor session directory went
wholesale — so the committed offset is the last thing that file ever had and
there is nothing for an operator to act on. Every record on that path is then
INFO: the `file-vanished` record carries `reason=tree_removed`, and the
`lost-policy` grace-clock and conclusion records say the directory went with it.
A file unlinked while its directory STANDS keeps its WARNING, because that is a
genuine unlink under the reader.

- ONLY A DEFINITE ABSENCE COUNTS. `treeRemoved` (cycle.go) answers true for
  `fs.ErrNotExist` and nothing else; a stat that fails for a permission change
  or an unresponsive mount is not evidence the tree was removed, and reading it
  as one would quietly downgrade a real unlink.
- A FILE THAT WAS ALREADY READ TO ITS END LOST NOTHING EITHER (owner ruling,
  2026-09-13). The same INFO path is taken when the file's directory STANDS but
  nothing was outstanding: a spool whose terminator (`[exited with code N]`,
  `[killed]`) had been read carries `reason=ended` — the vendor reaps a task's
  output file once the task is reaped — and a file whose committed offset equals
  the size the last successful poll saw carries `reason=fully_read`. The
  committed offset moves only on a store ack, so a batch in flight, a held
  frame, or a tailer mid-tail all leave bytes outstanding and keep the WARNING,
  as does a tailer that never completed a poll and so has no size to compare.
  `vanishReason` (cycle.go) is the single place the three are decided.
- THE REASON IS A VOCABULARY, NOT A PILE OF BOOLEANS. `Lost.BenignEnd` carries
  it through to the conclusion as a string whose empty value is the genuine
  loss, so two ways of ending the same way cannot both be true of one file.
- IT CHANGES NO CONCLUSION AND NO WIRE ARM. The run is still LOST and
  `DetachedLost` still carries `file_vanished`. `lost-terminal` joins back to the
  sweep on the `reason` key, so that key stays the wire's own arm on every
  `lost-policy` record; `tree_removed` rides the `file-vanished` record, which
  names a FILE rather than a run and has no arm to collide with.

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
    EVERY SPOOL CONVERTER IMPLEMENTS IT, the residue one included. Reading a
    spool as residue states what could be made of its BYTES and never that the
    run is unknown: a restart with a transcript backlog routinely reads the
    launch line naming a spool's spawning call AFTER that spool's hold expired,
    and the demoted watcher is never rebuilt — so the reader knows the run, and
    a stop that reached a handler unable to spell one left the run open in every
    consumer (realtest 3, 21 runs in one pass).
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
- THE TERMINAL ITSELF IS ONE IMPLEMENTATION, `handler.RunOutput`: the bounded
  output buffer, the file coordinates the write identity is digested from, the
  attribution, and the `Cancelled`/`Lost` frames. A handler that can be asked
  for a terminal EMBEDS it rather than accumulating its own. Two handlers
  spelling "the same terminal" from two private accumulations is how the shapes
  drift, and the difference is invisible until a consumer reads two runs settled
  two different ways.

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
  record has no key at all: nothing of one is stored (see "Keep-alive" below).
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
- `residue.go` holds only the declared-residue handler for a CLAIMED `w*`
  spool. A spool no call claimed, or whose prefix is unmapped, is never read
  (see "Only what is rendered is read"), so there is no envelope-level
  last-resort handler any more.
- The EXEMPT SET is different: known built-ins deliberately not carried are
  DROPPED entirely — never `AgentUnmodeled`, never residue.
- The RESIDUE KINDS NEVER PERSISTED are the second such drop, and the ONLY place
  the mandate's "ends up in the store" half is narrowed. Every line is still
  READ, framed and CLASSIFIED; two named kinds are then not written. See the
  section below.
- A shape the schema cannot express is a SCHEMA GAP to surface loudly, never a
  record to drop.

## Logging

`internal/logging` is the ONLY diagnostic API. Direct output through `fmt`,
`log`, `slog` or an ad hoc logger is forbidden, except the documented
pre-logger bootstrap failure and the sink-emergency path.
`logging_bypass_test.go` enforces that boundary; its only sanctioned writers
are `main.go:reportFatal` and `internal/logging/logging.go:writeAll`.

`AGENT_REPL_LOG_LEVEL` is the ONE process-wide threshold:
`debug|info|warn|error`, default `info`. An invalid value is a bootstrap
failure before the rotating sink is opened. Production passes the parsed
`agentrepl/logging.Level` to `logging.NewDurableOnlyAtLevel`; focused tests and
foreground harnesses may use `logging.NewAtLevel`.

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
- Every config-root file resolves `workspace_dir` from an authoritative
  transcript `cwd`, never by decoding the lossy project slug,
  and derives `workspace_id` with the shared workspace digest. Spawn
  observations carry that identity plus `claude_session_id` to task spools.
  These three identifiers are promoted top-level record fields and every
  downstream tail/handler/converter logger inherits them.
- RETIRED ADDRESSING KEYS ARE GONE AND STAY GONE: `seq`, `from_seq`,
  `replay_*_seq`. `claude_session_id` is attribution, not addressing, and is
  required on file-scoped records when the owning transcript is known.
- Request boundaries, state decisions, and per-record/per-batch success
  diagnostics use `logging.Bound.LogVerbose`, which emits `level=debug` and
  `verbosity=verbose`. Lifecycle edges are `info`; invariant violations and
  refusals are `warn`; owned failures are `error`.
- A PER-FILE RECORD IS A HOT RECORD HERE, and `watch` is one. Discovery has no
  age bound: every transcript ever written under either config root is watched
  for the life of the process, which on a working machine is thousands of files
  nothing will ever append to again. One normal-verbosity record each cost
  megabytes per boot that said nothing but "still here". The COUNT is the
  lifecycle fact, so `rescan` states it once per pass — how many files the pass
  started watching and how many are watched now — and a pass that changed
  nothing states nothing. WHICH file, and of what kind, is verbose detail.
- FILE-SCOPED DIAGNOSTICS GO THROUGH `agentrepl.v1.AgentRepl.ClientLog` with
  the `sidecar` runtime arm, the sidecar's timestamp and verbosity class, its
  PID and Claude session in context, and the complete daemon-minted workspace
  ref read from `WatchWorkspaceRoster`. The daemon address is re-read from
  `<state-dir>/daemon.addr` for every record so handover changes the destination
  without a sidecar restart.
- A DIRECTORY'S WORKSPACE REF IS ONLY EVER USED WHILE THE ROSTER STILL HOLDS IT.
  Nothing pushes a roster change into `internal/daemonclient`, and the roster
  moves under it: a workspace is forgotten and the SAME directory is registered
  again under a NEW daemon-minted id. Realtest 9, sweep rt-run39, is what that
  cost — one cached `(address, dir)` slot, re-read only when the cached DIR
  differed, so a directory re-registered as `54578ede3d834dea` kept forwarding
  against the forgotten `af24557b1ddd4b9c` and had every one of its file-scoped
  diagnostics refused and written unattributed. Four rules hold the invariant,
  and none of them is a subscription:
  - THE CACHE IS A MAP KEYED PER NORMALIZED DIRECTORY. One slot also meant two
    directories forwarding in turn evicted each other and re-read the roster for
    every single record.
  - EVERY ENTRY CARRIES THE INSTANT ITS ROSTER WAS DELIVERED, and an entry older
    than `workspaceRefFreshness` is re-read before it is used again. The bound is
    the sidecar's own poll interval: a ref may be at most one poll pass behind
    the roster, the same granularity everything else about a pickup is read at.
  - A DIFFERENT DAEMON ADDRESS DROPS THE WHOLE MAP. Ids are minted per daemon and
    mean nothing across a handover.
  - A `no longer registered` REFUSAL IS THE ONLY PUSH THIS CACHE WILL EVER GET,
    so `Forward` acts on it: the directory's entry is expired, the roster is read
    ONCE more, and the record is sent again with the ref the roster holds now,
    before anything is reported `forward_undelivered`. One re-read, not a ladder
    — a refusal that survives the fresh ref is a directory the roster genuinely
    no longer holds, which is the `ErrForwardWorkspaceUnresolvable` path below,
    unchanged.

  A replacement is stated ONCE, at `info`, as `workspace-ref-replaced` carrying
  the directory and both ids. It is GLOBAL rather than file-scoped on purpose: a
  record carrying `workspace_dir` is queued for forwarding, and this one is
  written from inside the forwarder, including inside the bounded drain at Close
  where enqueuing another forward panics. Nothing about an ordinary
  re-registration is a `warn`.
- THE STORE ROWS NEVER CARRIED THE MINTED ID, and that is why the fix above is
  the whole fix. `tail.Context.WorkspaceID` — the `workspace_id` on every
  `tail-pickup`, and what `handler.attribute` copies onto a conversion — is the
  SHARED DIRECTORY DIGEST, `md5hex(clean(abs(dir)))[:8]`, derived once in
  `internal/discover.ResolveWorkspace` from the transcript's authoritative `cwd`.
  It is a function of the PATH, so a directory registered again under a new
  daemon id keeps the identical digest, and `store.v1` carries no workspace field
  at all. The daemon-minted ref is a ClientLog DESTINATION and nothing else.
- GENUINELY GLOBAL SERVICE RECORDS stay in the global rotating sink only. A
  forwarding failure writes one global record per daemon address and outage
  window and never fails the file-plane operation that produced the diagnostic.
- A DAEMON THAT IS BOOTING IS NOT A DAEMON THAT IS GONE. Its ~10s boot
  reconciliation is listening and answering nothing, so the first ClientLog of
  a sidecar that came up beside it deadlines. Forwarding therefore climbs a
  RETRY LADDER — six attempts over a doubling 250ms..5s backoff, spanning
  ~12.75s — and the ladder is ABANDONED at Close, because a process that is
  exiting does not wait out an outage. The one failure record is `warn` and
  carries `attempt`: the count is what separates "slow to boot" from "not
  there", which a bare failure could never say.
- AN UNDELIVERABLE DIAGNOSTIC IS NOT A DISCARDED ONE. When the ladder is
  exhausted the file-scoped record itself is written to the GLOBAL durable
  sink, marked `forward_undelivered` with the daemon address, attempt count and
  cause, keeping its workspace attribution. Dropping it, which is what this
  loop used to do, silently swallowed every file-plane diagnostic for the whole
  of a daemon outage.
- AN UNRESOLVABLE WORKSPACE IS FORWARDED UNATTRIBUTED, NOT WARNED, NOT RETRIED.
  A file-scoped diagnostic can name a dir that is not a real workspace -- a
  macOS temp-root, an unknown path -- and so will never appear in the roster.
  `WatchWorkspaceRoster` replays its COMPLETE current snapshot to a fresh
  subscriber, so `resolveWorkspace` concludes on the FIRST delivered roster: if
  that snapshot lacks the dir, the dir is unresolvable and `Forward` returns
  `logging.ErrForwardWorkspaceUnresolvable` AT ONCE, never holding the standing
  stream open to its deadline waiting for a workspace that will never register.
  The forward ladder does NOT retry that sentinel; `forwardLoop` narrates it as
  `sidecar.logging.forward-deferred` at DEBUG and the record still lands in the
  GLOBAL durable sink via the same `forward_undelivered` no-loss path. This is
  the roster-delivered-but-absent case ONLY: a roster stream that errors or
  never delivers a snapshot is a transport transient handled by the pid/boot
  sentinels above, not an unresolvable workspace.
- Lifecycle records persist in
  `~/.cache/agent-repl/log/shim-claude-sidecar.log` (`--log`).
- THE DURABLE LOG IS THE ONLY COPY, AND IT IS BOUNDED. `--log` is opened
  through `agentrepl/logging`.`OpenRotating`: it appends to what it finds and
  ROLLS AT A BYTE CAP into a fixed number of generations (`<path>.1` newest
  through `<path>.N` oldest), so the file plane's disk footprint is
  `(N+1) x cap` no matter how long the process runs. It does NOT roll on open —
  this is a launchd service bounced by every deploy and every crash, and
  rolling per boot would evict every generation of real history.
- THE TERMINAL IS NOT A SECOND LOG. Production builds the logger with
  `logging.NewDurableOnly`, so ordinary records go to the durable sink ALONE.
  Under launchd stderr is a plain append-only file the process neither owns nor
  can roll; mirroring every record there was an unbounded second copy of an
  already-rotated log, and it reached 6.2 GB beside a 666 MB `--log` on the
  owner's machine. The terminal keeps exactly two things: the BOOTSTRAP errors
  written before a logger exists (a malformed window, an unopenable log), and
  the SINK-EMERGENCY record, which must not re-enter the failed durable sink.
  `logging.New`'s two-sink mirroring stays for tests and foreground runs, where
  both sinks are the caller's to manage.

Read sidecar records and harvest run windows through `../../../bin/logs.sh`;
the full path, rotation, attribution, and level-switch table is in
`../../../AGENTS.md`.

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
go build ./... && go vet ./... && ../../../bin/background.sh go test -race ./...
```

Every test run goes through `../../../bin/background.sh`, at background
priority; the bare `go test` lines below are the arguments to prefix with it.

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
| `waitBudget` | `integration/helpers_test.go` | 50s | **10s** | ~3x the slowest whole subject (2.82s, `TestMockKeepAliveTurnsStoreNothing`; 0.92s alone). A subject's own wall time bounds every wait inside it. The old 50s cited a ~16.6s max for `TestMockScenarios/!subagent`, which measures **0.33s** — the number was ~50x its own premise. Costs nothing on green. |
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
- **A Connect client of the SHIM keeps the request body it promised.**
  `udsHTTPClient` wraps its transport in `ownedRequestBody`
  (`integration/helpers_transport_test.go`), and the wrapper is not optional.
  connect-go releases a declared-length request payload the instant `Do`
  returns, and the shim answers a stream on the request HEAD, so the head beats
  the body write and the transport reads EOF at offset zero on a request that
  already announced its length — `http: ContentLength=48 with Body length 0`,
  after which it TEARS THE CONNECTION DOWN under every other call riding it.
  The failure surfaces on whichever WatchAgent tail happened to share the
  connection, as `incomplete envelope: … use of closed network connection`, so
  it names an innocent scenario. The daemon fixed the h2c shape of the same
  defect in `daemon/internal/shimclient/transport.go`; the two must stay in
  step.
- **A subject that must act between two things the sidecar does back to back
  stops the sidecar, it does not out-run it.** `writeGate`
  (`integration/helpers_test.go`, on both the fake store and the recording
  proxy) withholds the ANSWER to one chosen batch. A batch is written
  synchronously inside the cycle and the cursor advances only on a durable
  success, so a store that has not answered is a cycle that has not moved on.
  That is what the hold subjects use instead of a 500ms (or 5s) poll interval.
  Release the gate inside the subject: a process frozen in a withheld write
  leaves `shutdownSettle` (250ms) after SIGTERM, not at once, so a subject that
  must end one where it stands has `sidecarProc.Kill`.
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

A page line, a detached run's frame, an unserved item, or a drop. There is no
fifth, and nothing on disk is ever silently lost.

- A **page line** names its book (`StorePageLine.page_agent_id`). A subagent's
  constituents form ITS OWN book; the SPAWN that created it is a line in the
  parent's.
- A **run frame** (`StoreAgentBash`) wraps the spawning call's unit id and is
  structurally unpaginatable.
- An **unserved item** is `vendor_specific`
  (understood, deliberately not carried — the follow-up is a CONVERTER),
  `unknown` (parsed, not modeled — the follow-up is a MODEL), or `unparsed`
  (unreadable — a FAILURE, carrying source, offset, parse_error and bounded raw).
- A **drop** is the exempt set, one of the RESIDUE KINDS NEVER PERSISTED
  (below), or a KEEP-ALIVE TURN'S RECORD (below). Never residue, never
  `AgentUnmodeled`.

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
- RESOLUTION IS AN IN-MEMORY LOOKUP ON THE POLL PATH, and one directory question
  stands behind it. `rekeyRotations` resolves EVERY WATCHER ON EVERY TICK, and
  `Resolve` used to glob `<state>/shim/*/vendor-id/<id>.json` for every id in
  neither map — ~2900 globs a second on the owner's machine, ~1100 of them for
  cold runs that never resolve. A 10s `sample` of the live sidecar (pid 96084)
  found it holding 76–101% of a core inside `filepath.Glob` under `Resolve`, with
  poll ticks running seconds long. The index now remembers the ids it looked for
  and did not find, and `Index.RecheckLinks` — one readdir of `<state>/shim` plus
  a stat per workspace, asked ONCE per tick by `rekeyRotations` — drops those
  misses when a link directory's mtime moved. Same shape as the discovery change
  probe, and for the same reason: a link file lands INSIDE
  `shim/<key>/vendor-id/`, so one stat answers for every id under it. `Refresh`
  clears the misses outright and is the backstop for a write the mtime missed.
- THE BOOK IS RE-RESOLVED BY EVERY POLL AND EVERY RESCAN (`cycle.go`'s
  `rekeyRotations`, called from `pollAll` and `rescan`), because discovery order
  is not causal order: a link that lands after the transcript was first seen
  moves the watched file's book, and UN-PARKS it when the refusal that parked it
  was that very book move — the one refusal that stops being true. Nothing is
  duplicated, because `write_id` and `upsert_key` are digested from a file
  position that did not move.
- A ROTATION'S TRANSCRIPT IS HELD UNREAD UNTIL A RECORD NAMES ITS BOOK
  (`rotation.go`). Re-keying moves only the records read AFTER the link lands;
  the ones already committed under the file's own id stay there, and the store
  then SKIPS the shim's stream-plane writes of the same keys as book moves, so
  the daemon's watch on the conversation's real book never sees the cleared
  cut or the turn's answer (e2e `TestSecondRotateUnderRotatedIdentity`,
  2026-09-23: read at .322, link at .323, `final_answer_unresolved`). So a MAIN
  transcript that no record names, that OPENS WITH A CLEAR (or has not yet
  written the user record that says), in a project directory where another
  transcript IS named by a shim record, is not read at all: stated once at INFO
  (`rotation-hold`), re-examined on every poll after `rekeyRotations`, and read
  from its start the moment the link lands. Evidence only, no timer. A
  conversation the shim never recorded keeps R9's own-id default; a clear run
  outside the shim in a directory a shim conversation shares stays held.
- A FIRST ATTRIBUTION IS NOT A BOOK MOVE. A spool aged into residue before its
  launch line was read is watched with NO book, and the same pass is what
  finally gives it one. That arm is INFO and names its resolution source; only
  a move between two NON-EMPTY books is WARN, and only the parked arm may claim
  the shim's identity files said anything, because a spool's answer comes from
  the OWNER INDEX and is legitimately a `toolu_` id no identity file holds.
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
  - NOT `question:<tool_use_id>`: an ask is STREAM-OWNED (`internal/convert/
    streamowned.go`). Both planes can name it, which is exactly why only one
    may write it — the shim gates the ask and holds the answer as a repeated
    `chosen`, while the transcript states it only as the vendor's
    comma-joined string, which question.proto's retired tag 4 says cannot be
    split back. So this reader converts neither the AskUserQuestion call nor
    its result, and drops both (a decision, never residue).
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
- A RESPONSE THAT PRODUCES NO UNITS AT ALL IS LEGITIMATE, AND THE RECORD THAT
  REPORTS IT NAMES THE REAL CAUSE. There are two, and they are different
  decisions: the EXEMPT SET (the tool is dropped at the call and at the result
  alike) and the DEFERRED ANNOUNCE (the unit is real and appears at the call's
  RESULT, because the subagent spawn's `created_agent_id` is not knowable until
  the launch answers). The record used to assert the exempt set unconditionally,
  which is a lie on the commonest shape it fires for — a response whose only
  block is a spawn — and it sent a reader hunting a modelling gap into a
  decision that was never taken. Each block that produced nothing answers WHY;
  the record renders the distinct causes in block order. A response producing
  nothing for a cause NO BLOCK NAMED is stated as a modelling gap, because that
  is what it is. The accounting still has nowhere to land either way, so the
  record stays loud.
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

### Residue is never persisted

Owner ruling 2026-09-13 (`docs/STORE-VOLUME-PROPOSAL.md` item 1). ONLY TYPED
ENTRIES ARE PERSISTED. Every residue outcome — `vendor_specific` of any kind,
`unknown`, and `unparsed` —
is classified, counted, and NOT WRITTEN.

WHY: NOBODY READS IT. Those three arms have zero readers anywhere downstream —
not in the daemon, not in the webapp, not in the editor — and the measured store
(1.64 GB, 612,214 rows) is overwhelmingly made of them. The volume was stored
for nothing.

- `internal/convert/neverpersist.go` holds the predicate (`IsResidue`) and the
  counting label (`ResidueLabel`); `cycle.go withholdResidue` applies it.
- IT SITS IMMEDIATELY ABOVE `storeWrite`, THE SIDECAR'S ONLY DOOR TO THE STORE.
  Residue is minted by the converter, by three handlers and by the detached-stop
  seam, so a filter at any producer is a filter the next producer forgets. A rule
  enforced at the one write path is a rule about the SIDECAR rather than about
  the callers that happen to exist today.
- THE DISCOVERY MANDATE AND THE CLASSIFICATION ARE UNTOUCHED. Every line is still
  read, framed and filed under its own arm by the branch it always was, and the
  golden-corpus census still pins the vendor_specific kinds the converter mints —
  a new kind appearing there is still a mapping regression. Only the write is
  skipped.
- KEEPALIVE IS NOT RESIDUE, and it is not withheld here. A keep-alive's record
  is dropped by the converter itself, per RECORD, before any entry leaves it
  (see "Keep-alive"), so no entry on the `unserved_item.keepalive` arm is minted
  at all; rows on that arm written before 2026-09-23 stand in the store as they
  are.
- THE FORWARD-COMPAT ARGUMENT MOVES TO THE COUNTS. `unknown` used to be kept on
  the grounds that its stored row IS the coverage for a vendor behavior nobody
  has modelled. The classification, the per-record `residue-drop` label and the
  per-file summary are that coverage now — and nothing is unrecoverable, because
  the sidecar's sources are the vendor's own DURABLE files: the day a residue arm
  earns a model, the file is simply re-read.
- THE CURSOR STILL ADVANCES. It rides the batch, not the entries
  (`PollResult.Changed` turns on the offset, not the entry count), so a batch
  whose every record was residue still commits the reader's position — the bytes
  were read, and re-reading them would produce the same nothing. A batch left
  with no entries AND no cursor advance is not sent at all.
- LOGGING. Each withholding is DEBUG (`residue-drop`), carrying the residue label
  on `reason` and NO `upsert_key` — it announces no row, and a key nobody can
  look up is exactly what the field-set contract forbids. The boot walk states
  one INFO `residue-drop-summary` PER FILE at the catch-up edge
  (`cycle.go summarizeWithheldResidue`), carrying the counts by label; the
  inferred batches, which name no file, are summarized under their own record. A
  file that withheld nothing states nothing.

### The residue shape catalog

Owner ruling 2026-09-13 (`docs/REALTEST-JUDGEMENT-CALLS.md`, "the
unmodelled-line shape catalog"). Withholding a residue line takes its bytes out
of the store and, with them, the only evidence the vendor emits that line at
all. THE SHAPE IS WHAT SURVIVES: one row per distinct recursive key structure,
so the vendor's API stays discoverable at a cost bounded by the number of shapes
rather than by traffic.

`internal/convert/shape.go` renders and hashes it; `cycle.go withholdResidue`
contributes one observation per withheld line, at the same door that withholds
it. The rows live in the store's `residue_shapes` table and are read back with
`ListResidueShapes` (see that module's AGENTS.md).

**THE CANONICAL RENDERING, AND EVERY CLAUSE IS TESTED**
(`internal/convert/shape_test.go`):

- an OBJECT renders as `{key:shape,...}` with its keys SORTED and recursed, so
  the shape does not depend on the order the vendor serialized them in;
- an ARRAY renders as `[shape]` where the element shape is the MERGE of every
  element — the union of their keys — so a list of ten near-identical content
  blocks is one shape and not ten. An always-empty array renders `[]`;
- a SCALAR contributes only its JSON TYPE (`string`, `number`, `bool`, `null`)
  and never its value, which is what makes two lines differing only in what they
  say one shape;
- a position that took SEVERAL forms renders them all, sorted and joined with
  `|`, so a union is stated rather than resolved by picking a winner.

**THE ID WILDCARD, AND IT IS THE LOAD-BEARING CLAUSE.** An object key that looks
like a GENERATED ID is replaced by the single key `*`, and several id keys in one
map MERGE under it exactly as an array's elements do. Without it a map keyed by
tool-call id mints a brand-new structure per line, which is the one failure mode
that turns a bounded catalog back into a copy of the corpus. A key is an id when
it is:

- a uuid in the canonical 8-4-4-4-12 hex form;
- `toolu_`-prefixed or `msg_`-prefixed with something after the prefix — the
  vendor's own two spellings;
- a hex run of SIXTEEN characters or more. Shorter runs are excluded on purpose:
  `deadbeef` is also a word;
- nothing but digits, which is an array-shaped map's index.

Everything else is a vocabulary key and is kept VERBATIM. The rule is narrow
deliberately: a shape whose key names have been guessed away describes no API.

**THE HASH** is SHA-256 over the rendering, lowercase hex, and it is the
catalog's primary key.

**WHAT ELSE THE OBSERVATION CARRIES.** The `kind` is the withheld tally's own
label (`ResidueLabel`), so a `--kind` filter matches what the summary said. The
`first_example` is the residue arm's own payload: for `unparsed` that IS the
vendor's bytes, and for `vendor_specific`/`unknown` it is the decoded payload
re-serialized as COMPACT JSON — this door sits above the converter, so the
frame's original bytes are no longer in hand and the example agrees with the
vendor's line in every key and value while differing in key order and
whitespace. Bytes that are not JSON at all have no key structure and share the
one `<unparsable>` row, whose example says why the parse failed.

**DEDUP AND DELIVERY.** Observations are deduped WITHIN the batch by hash, first
example kept, because a boot walk reads thousands of lines of one shape and one
observation per line would send the catalog the very volume it exists to avoid
storing. They ride the same `WriteBatchRequest` as the records and the cursor
advance, so an observation taken from bytes that advance consumes commits with
it or not at all. A BATCH OF ONLY RESIDUE IS THEREFORE SENT — carrying no
entries and only its observations — because an observation dropped for looking
like an empty batch is a shape no re-read ever observes again. A batch with no
entries, no cursor advance and no shapes is still not sent.

**THE SUMMARY.** The per-file `residue-drop-summary` states `new_shapes`: how
many hashes THIS PROCESS saw for the first time. A shape seen in an earlier
batch still rides the wire (the store's count must rise) but is not news.

### Keep-alive

NOTHING OF A KEEP-ALIVE IS STORED, ON EITHER PLANE (2026-09-23). No purpose
needs the rows: the keep-alive's send, its answer and the rewind anchor are the
shim's in-memory state, a resume reads the vendor's own file, and nothing reads
a stored keep-alive row (the store serves only page lines and bash runs, and the
daemon has no store client). So the shim writes nothing for one, and the
converter drops every entry a keep-alive's record converts to — one DEBUG
`keepalive-skip` record per record, carrying no `upsert_key`. The record is
still read and converted in full, so every join it opens or settles stays warm.

THE RULE READS THE TRANSCRIPT'S LINKS, NEVER ARRIVAL ORDER (`internal/convert/
keepalive.go`). The vendor's file is a tree whose records interleave: a real
prompt after a rewind parents onto the anchor while the keep-alive's late
records still parent onto the keep-alive, a compaction's summary query runs
beside one, a task notification opens a turn while one is pending. The old
one-bool rule served the keep-alive's `.` whenever another prompt landed first,
and lost the bit across a restart. Now:

- a USER record with a `promptId` is the keep-alive's when that promptId is a
  keep-alive prompt's; a prompt (prose, not meta, not a compaction summary)
  whose first text block BEGINS with `<!--agent-repl:keepalive-->` makes its
  promptId one. A different promptId is a different turn, whatever its parent —
  a task notification or a peer message chained onto a keep-alive stays served;
- a USER record with no `promptId` (an older CLI) is classified by the marker
  when it is a prompt, and follows its parent otherwise;
- EVERY OTHER RECORD FOLLOWS ITS PARENT (`parentUuid`). A record with none — a
  compaction boundary, the CLI's unchained bookkeeping — is no keep-alive's, so
  a compaction beside a keep-alive is served: the conversation really was
  compacted, and the shim clears its rewind anchor on it for that reason.

A RESTART LOSES NOTHING. A tailer hands a `tail.Primer` handler the file's
bytes before the first frame it delivers, once, before the first `Handle`; the
session transcript handler seeds the converter from them with the same rule
(`SeedKeepalive`, DEBUG `keepalive-seed`). The boot rewind alone is not enough:
it stops at the LAST turn start, which is another prompt whenever one landed
between the keep-alive's prompt and its reply. A failed prefix read fails the
poll, which commits nothing and primes again next time.

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
HELD, not read), refused loudly.

ONE ROW PER KIND OF FACT, and ONLY WHAT IS RENDERED IS STORED (owner ruling
2026-09-23: output beyond what is rendered is not stored):

- `bash:<run>:start` — a start row, if a producer ever mints one. The sidecar
  does not: a spool exists only after the STREAM plane announced the launch.
- `bash:<run>:tail` — the run's RENDERED TAIL, `AgentBash.tail`, superseded
  WHOLE by every batch. It holds at most `AGENT_BASH_TAIL_CAP_BYTES`
  (conversation.v1 `AgentBashTailCap`, the one constant the daemon draws by
  too) and states the `bytes_omitted` and `lines_omitted` before it. The
  handler cuts it exactly as the renderer draws it (a line start once anything
  is omitted), so the daemon draws it verbatim and nothing past what is drawn
  is ever written.
- `bash:<run>:terminal` — the single terminal, however often it is restated.

THERE IS NO OFFSET AND NO GAP DETECTOR. The retired delta model keyed one row
per delta by `from_offset` and needed the deltas contiguous from 0, so a
claimed spool's WHOLE file was stored; a snapshot that states its own omitted
count cannot have a hole. Rows under the retired `bash:<run>:<from_offset>`
spelling are left in the store as outmoded.

THE WINDOW IS A PURE FUNCTION OF THE FILE'S PREFIX (`RunOutput.Absorb`). A batch
that does not begin where the window ends — a restarted process's first batch,
a batch re-read after an unacknowledged write, a file reset to zero — RESEEDS
the window by streaming the file's prefix (64 KiB at a time), so the tail it
writes is the one it supersedes, never one started over from the resumed
cursor and never one holding a re-read batch twice. The tail row's write
identity is digested from where the window ENDS, so a re-read mints the same
identity and the same bytes. A reseed that cannot read the file WITHHOLDS the
batch's tail (ERROR) and the next batch reseeds; a window missing the prefix
would state wrong omitted counts for the rest of the run.

The bound firing is recorded ONCE PER RUN at INFO (`run-output-bound`).

`EXIT=<code>` → `AgentBash.success.completed` with `termination.exited`. The
matching is strict (last line of the batch, newline-terminated, line-start, at
most three digits) because `EXIT=` is common as ordinary output — 23 of the 44
real spools carrying it have it only mid-line. The raw codec carries nothing, so
whether a batch BEGINS a line is answered from whether the previous batch ended
on a newline — which the handler alone knows, and without which a marker
arriving on its own poll (the ordinary case) was never detected at all.

A TERMINAL CARRIES THE RUN'S OUTPUT, not the batch's: the handler holds the
run's window, bounded at `AGENT_BASH_TAIL_CAP_BYTES` (`maxRememberedOutput`),
and states `partial{bytes_omitted}` past the bound rather than claiming
`whole`. The daemon draws a detached run's ending from the terminal (exit,
cancel, lost) and its body from the tail row, never the terminal's output, so a
larger bound stored bytes for nobody.

### Bounded writes

Every batch is one store transaction, so a batch sized by the file was a write
sized by the file (one held the store 163s on 2026-09-23). `internal/tail`
bounds each Poll at `MaxBatchBytes` (1 MiB) and `MaxBatchFrames` (128); the
cursor stops at the first frame past the bound. A bounded batch reports
`PollResult.More` (never for a batch holding a frame) and `pollAll` re-polls
that file at the head of the pass, inside the same slice deadline. Each bounded
batch supersedes the run's one tail row, so a long spool costs one bounded row
per batch and the store never holds more than the tail.

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
