# Merge queue subscriptions

Any workspace can subscribe, through `/merge-queue`, to a merge queue's state
changes — one repository's queue or every repository's — at a level of detail
it picks (every change; only when the queue advances; only when it depletes).
Each subscribed change reaches the subscribing workspace's agent as a notice
delivered under the hood through its shim, carrying a snapshot of the queue,
and never drawn in the webapp. A subscription ends when the workspace
unsubscribes or closes.

The owner pre-approved the contract changes for this work (2026-10-14), so the
design was settled by the implementing session without a per-increment
selection round; each decision below records the reasoning a reviewer needs.

## Core design principles

- **One delivery path to a workspace's agent.** In the owner's terms: "find how
  the daemon already delivers under-the-hood notifications to a workspace's
  agent and reuse that path; do not invent a second delivery mechanism."
  - Consequence: a notice is a deferred prompt submitted through
    `prompthandler` exactly as `mergeworkspace.Service.HandBack` submits an
    async merge's handback (`PromptOrigin` + `wsm.DeliveryDeferred` + a minted
    idempotency key + a brief from `prompts/`). The only contract addition on
    that path is the origin value that names it.
  - Not claimed: that notices share the handback's feed marker. The handback
    is drawn as a badge; a notice is drawn as nothing.

## Landed changes

### 1. `UpdateMergeQueue` gains `subscribe` and `unsubscribe` arms

- **What**: `agentrepl.v1.UpdateMergeQueueRequest.action` gains
  `subscribe` (11) and `unsubscribe` (12), after master's racing arms (9, 10);
  `UpdateMergeQueueError.cause` gains `not_subscribed` (13). The detail is a
  oneof of three empty arms (`every_change`, `advances`, `depletes`).
- **Why**: subscription is queue control in the owner's framing ("new verbs,
  scoped to one repository's queue or all, like pause/resume are"), and every
  other queue control is an `UpdateMergeQueue` arm that the command-file
  ingress mirrors one entry type per arm. Putting it anywhere else would give
  the skill's verbs two routes into the daemon.
- **Scope rule**: at most one subscription per (subscriber, scope), where the
  scope is one repository or every repository. Subscribing again replaces the
  detail. An unscoped unsubscribe ends only the unscoped subscription. This
  mirrors pause/resume (the unset ref is the every-repository switch) while
  keeping each scope ended exactly as it was begun; the rejected alternative
  — an unscoped unsubscribe ending everything — makes one verb mean two
  things depending on what else the workspace holds.
- **`not_subscribed` is an error arm**, matching the sibling no-op refusals
  in the same oneof (`not_paused`, `no_such_queued_merge`,
  `rollouts_not_held`). The command-file path answers it as the APPLIED
  outcome `not_subscribed`, exactly as it answers `no_such_queued_merge` as
  `not_queued`, so an agent asking to be unsubscribed is satisfied when it is
  not subscribed.
- **Levels nest**: depletes ⊂ advances ⊂ every change. "Advances" is a merge
  leaving the queue (landed, failed, or taken off); "depletes" is a merge
  leaving with nothing left in line. A merge only REQUESTED (its asking turn
  still running) is never a member, so neither its recording nor its removal
  is reported — the same membership rule the queue tab draws (`inLine`).
- **Reorders** (`move_up`, `move_down`, the race's yield, the dequeue offer's
  prioritize) are reported at every-change detail. The owner's list did not
  name them, but they change the order the snapshot reports, and a
  subscriber at that level asked for every change.
- **Consequences**:
  - Every client that switches on `UpdateMergeQueueRequest.action` or
    `UpdateMergeQueueError.cause` has new arms to handle or reject
    (`daemon/internal/server/admin.go`, `daemon/internal/server/validate.go`,
    `daemon/internal/merge/refusals.go`; the Emacs and webapp fakes decode
    only what they send).
  - A new command-file entry type pair (`merge_subscribe`,
    `merge_unsubscribe`) with `repository_dir` and a `detail` field
    (`daemon/internal/commandfile/entry.go`).

### 2. `conversation.v1.PromptOrigin.PROMPT_ORIGIN_MERGE_QUEUE_NOTICE = 36`

- **What**: the origin every notice is submitted under.
- **Why**: the origin is the closed attribution vocabulary the feed and the
  status surfaces route by. The owner ruled the notice "NOT drawn in the
  webapp", and `resolve/feed.PromptRowHidden` decides that by origin, so the
  notice needs its own value; reusing `MERGE_HANDBACK` would draw a badge.
- **Consequences**:
  - `daemon/internal/resolve/feed/prompt.go` `PromptRowHidden` answers true for
    it; `AuthorLabel` names it "Merge queue" for any other reader.
  - `daemon/internal/promptqueue/deliver.go` `mergeQueueOrigins` includes it,
    so a notice's turn raises no desktop banner (the queue raises only its two).
  - `lisp/wire-common.el` lists it among the shim-only origins no client sends.
  - The agent's REPLY to a notice is drawn like any reply: only the daemon's
    own words are hidden. Accepted by the owner's framing (the notification is
    not drawn; the agent's work is the agent's).

### 3. `UpdateMergeQueueError.subscriber_closed` (14)

- **What**: `subscribe` naming a closed workspace is refused.
- **Why**: a close is what ends a subscription, so a subscription taken by a
  workspace that is already closed would never end, and its notices would be
  submitted to a session nobody runs. The store refuses it inside the same
  transaction that writes the row, so the check and the write cannot be
  separated by a close.
- **Consequences**: `daemon/internal/merge/refusals.go` maps the store's
  refusal to the arm; the command-file path quarantines the entry with the
  cause, as any refusal.

## Non-contract decisions recorded for the implementation

- **Persistence model.** Subscriptions are durable (wsm layout 55,
  `merge_queue_subscriptions`, keyed by subscriber and repository key; the
  every-repository scope is stored as the empty key). Why durable: a landing
  in the daemon's own checkout redeploys and restarts the daemon, so a
  restart-scoped subscription would end at the very change it exists to
  report. The rollout hold is durable for the same reason.
- **Close ends it structurally.** `wsm.MarkClosed` deletes the
  workspace's subscriptions in the same transaction that marks it closed, and
  the table's foreign key cascades on `Forget` (nuke). Every close path —
  `CloseWorkspace`, kill, nuke, a one-shot's landing close, the boot's
  closes — goes through one of the two, so no close path can leave a
  subscription behind; a hook in the workspace verbs could be missed by a
  path that bypasses them.
- **Events are emitted at the queue's writes, never diffed.** The
  orchestrator's DB is already wrapped so every write that adds or removes a
  queue row republishes the roster's async set (`merge/asyncmerges.go`); the
  same wrapper now reports each write's queue change. Two daemons share the
  store across a handover, so a state-diffing observer in each would report a
  change the other daemon made a second time; a write is made by exactly one
  daemon, so a change is reported exactly once. Step changes come from the
  orchestrator's one fact publication (`orchestrator.publish`), reported only
  when this daemon had already published facts for the merge, so a successor
  resuming a merge does not re-announce the step its predecessor reported.
- **Depletion is decided under one lock with the write.** The write and the
  read of what remains in line are serialized per daemon, so two merges
  leaving at once cannot both see an empty queue and report two depletions.
- **Delivery never blocks the queue.** Changes go to an in-memory FIFO drained
  by one goroutine (`internal/mergenotice`), which reads the subscriptions at
  delivery time, renders the brief, and submits. The orchestrator therefore
  never waits on, or re-enters through, the prompt queue.
- **CORRECTION (2026-10-14): the notice loop outlives the merge drain.** The
  record first said a notice still pending at exit is recorded as undelivered
  at ERROR. That was wrong: the exit stops background loops BEFORE the merge
  drain, so a merge landing inside the drain reported its landing to a loop
  that had already stopped (caught by the integration test
  `TestStoppingTheDaemonInsideAMergesTerminalStampsTheLandingWithNoFailedWrites`).
  Root cause: the loop's lifetime was tied to the serving context rather than
  to the last moment a queue write can happen. The loop now has its own
  lifetime (`cmd/agent-repld/notices.go`), ended by the teardown step
  `stop_merge_notices` right after `drain_merges`, and it delivers every
  pending change before it returns (a submission during teardown is held by
  the prompt queue for the successor, as an async merge's handback is). A
  change reported after that can only come from a merge the drain abandoned to
  the successor; it is recorded at INFO as untold, matching the contract's
  "a change made while no daemon serves is never reported".
