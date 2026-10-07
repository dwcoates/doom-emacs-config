# Subagent liveness is a level owned by the vendor process (2026-10-07)

The decision record for detecting every way detached work ends. The `.proto`
files are the contract; this file records what changed, why, and what an
implementer must know that the schema does not say.

## The defect that prompted it

After the 2026-10-07 deploy handed the `doom` workspace to a new daemon, its
footer showed three subagents running that had finished days earlier. Evidence,
by tier:

- Observed (daemon log, 13:38:58): `daemon.footer.live_work_taken` admitted 13
  subagents, and `daemon.sessionwatcher.live_work_retired` retired none of
  them afterwards (every retirement was `bash_terminal`).
- Observed (store): the `agent` table marks all 12 subagents of main agent
  `69a42c9f…` ended.
- Code: `daemon/internal/sessionwatcher/route.go`
  `routeDetachedWorkLocked` admits an item live from ANY
  `conversation.v1.AgentDetachedWork` frame it routes, replayed history
  included, after `reconcileLiveWorkLocked` has already applied
  `SessionStarted.live_work`. Only a terminal frame retires it.
- Observed (transcript): every one of those subagents' endings was written as
  an `attachment` of type `queued_command` (a notification that arrived
  mid-turn), or as a `stopped` notification with no `<tool-use-id>` after a
  host restart.
- Code: `shim-claude-sidecar/internal/convert/attachment.go`
  `attachmentLine` has no `queued_command` case and withholds the record.

So liveness was rebuilt from edges in a record, and any missing end left the
work live forever.

## Vendor facts the design rests on

From `claude-agent-sdk` 0.3.280 `sdk.d.ts` (declared types):

- `background_tasks_changed` is a LEVEL with replace semantics, per vendor
  process, empty at a process start (`sdk.d.ts:3547`).
- `task_notification` states `completed | failed | stopped`, and
  `reason: 'worker_restart'` for a task the resumed process found orphaned
  (`sdk.d.ts:5738`).
- The level's order against the bookends is unspecified; in practice the
  level comes first.

## Principles stated by the owner

- Termination is detected resiliently in every mode: completion, failure, a
  stop, a restart, a crash, a lost notification.
- The resolution of what is reported (progress, token totals) does not
  change.

## Decisions

### 1. The live-work level

- WHAT: `conversation.v1.SessionUpdate.live_work` (tag 34,
  `conversation.v1.SessionLiveWork`).
- WHY: liveness belongs to the process that runs the work. An announcement or
  a terminal in a record can never decide it.
- CONSEQUENCES:
  - The shim pushes the level from its folded `background_tasks_changed`
    state (`agent-shim/claude/shim/src/engine/detached.ts`) whenever the
    membership changes. It pushes an EMPTY level the moment the vendor
    process ends or restarts, before anything the next process states.
  - The daemon's live set is EXACTLY the latest level, or
    `SessionStarted.live_work` at an opening. `routeDetachedWorkLocked`
    stops admitting; announcements and terminals feed the views only. The
    shim stream ending empties the set (the existing `departed` conclusion).
  - A history replay can therefore never make work live.

### 1a. The level carries ids only, and shells are not in it

- WHAT: `conversation.v1.SessionLiveWork.live_work` is
  `repeated conversation.v1.DetachedWorkId`, as the vendor's own level is.
- WHY: an item's description (kind, agent) is its announcement's, or
  `SessionStarted.live_work`'s at an opening; the level only says what runs.
  A handle the level names before its announcement arrives is held PENDING
  (counted for freeness) until the announcement describes it.
- SHELLS: a detached shell's end has ONE writer, the sidecar, which writes
  its terminal once the spool is read to the end. A shell the vendor already
  finished is still live until its output is ingested and drawn, so shells
  stay on the record path (announcement in, sidecar terminal out) and the
  level never names them. The shim's re-announcement names the level's items
  plus the shells the record holds open.

### 1b. The session contract stamp

- WHAT: `conversation.v1.SessionStarted.contract` (tag 10,
  `conversation.v1.SessionContract`), ordered revisions like
  `conversation.v1.AgentActivityContract`.
- WHY: a deploy hands a workspace to a new daemon before every shim is
  replaced. A pre-stamp shim pushes no level, so a daemon that read
  liveness only from levels would count none of its work and could bounce
  it mid-subagent. Under `SESSION_CONTRACT_UNSPECIFIED` the daemon reads
  liveness from announcements and terminals, as the expected old data it
  is; at `SESSION_CONTRACT_LIVE_WORK_LEVEL` only the level counts. A level
  from an unstamped shim is a producer breach, refused at ERROR.

### 2. Settling, and the endings with no stated outcome

- WHAT: `frontend.v1.FeedSubagent.settling` (tag 8,
  `frontend.v1.FeedSubagentSettling`),
  `conversation.v1.DetachedLost.process_ended` (tag 4), and
  `frontend.v1.FeedSubagentLost.process_ended` (tag 4).
- WHY: an item that leaves the level before its outcome lands must not draw
  as running, and must not draw a guessed outcome.
- CONSEQUENCES:
  - The shell lost row gains the same arm,
    `frontend.v1.FeedShellLost.process_ended` (tag 4), drawn when the
    process ending emptied the set.
  - Left the level, no terminal yet, while this daemon watched the process
    that dropped it: `settling`.
  - No terminal, and the process that ran it is gone (the shim stream ended,
    the level emptied at a restart, or the item was never in any level this
    daemon saw): lost, `process_ended`.
  - A terminal that lands later ALWAYS replaces either one; a terminal is
    the one source of an outcome.

### 3. The restart outcome

- WHAT: `conversation.v1.AgentSubagentFailure.worker_restarted` (tag 6,
  `conversation.v1.AgentSubagentWorkerRestarted`) and
  `frontend.v1.FeedSubagentSettled.restarted` (tag 6).
- WHY: the vendor's `worker_restart` was being drawn as `stopped_by_user`,
  which says a person stopped it.
- CONSEQUENCES:
  - Both converters map `reason: 'worker_restart'`, on the stream and in the
    transcript, to this arm.

### 4. Every transcript form of an ending is read

- No contract change. The sidecar (the one reader of the transcript) reads a
  `<task-notification>` carried by an `attachment` of type `queued_command`
  exactly as one carried by a user message
  (`shim-claude-sidecar/internal/convert/attachment.go`).
- A `stopped` notification that names no `<tool-use-id>` is the resumed
  process's account of a run it found orphaned. The sidecar settles it by its
  `<task-id>` through this stream's own launch receipt (`Converter.spawnedRuns`),
  as `worker_restarted`; a task id no launch on the stream carries stays
  residue. A notification whose summary names several runs states only one
  `<task-id>`, and the rest are not read out of prose: their outcome stays
  lost `process_ended`, which the level makes correct.

## Replacement tests (orchestrator's specs)

- Daemon unit: an `AgentDetachedWork` replayed from history admits nothing; a
  level push admits and an empty level retires; the shim stream ending
  retires everything; left-the-level-without-terminal resolves `settling`;
  never-in-a-level-without-terminal resolves lost `process_ended`; a later
  terminal replaces both.
- Shim unit: the level push on every membership change; the empty push on a
  process end and on a restart, ordered before the next process's frames;
  `worker_restart` maps to `worker_restarted`.
- Sidecar and shim converter unit: `queued_command` task notifications
  settle the spawn; a unit-less `stopped` settles by task id.
- E2E (`e2e/subagents_e2e_test.go`
  `TestADetachedSubagentWhoseProcessDiedIsNotLiveAfterAColdBoot`): a detached
  subagent the record shows running, whose shim was killed, is replayed by a
  cold-booted successor as stopped, and the footer counts no live agent.
