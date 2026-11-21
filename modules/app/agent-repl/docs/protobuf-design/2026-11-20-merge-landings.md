# Merge landings and the merge queue's size

Agents asked "how many merged through the queue today" counted
`merge(master): <branch>` commits on master's first-parent history, told the
queue-bypass landings apart only by their paired changelog commits, and
reported only the bypass landings or folded them in with the queue's. The
daemon's own landing record (`daemon.merge.finish`) lives in each workspace's
log, so the central log held none of them. The owner asked (2026-11-20) that
the agent API answer both questions directly: the queue's size now, and the
landings of a window split into the queue's and everything else, from
records the daemon writes at landing time naming the path.

Design ran in autonomous mode.

## Assumptions (step 1)

- Objective: answer queue size and landings-by-path from daemon records, never
  from commit-message conventions.
- What is built: a durable per-landing record in the daemon's store (wsm
  `merge_landings`), one recording helper (`merge.recordLanding`) every
  landing site calls, two RPCs, a status addition, two CLI verbs.
- No frontend component: this is the agent API section of `agentrepl.v1`, so
  figma→idl does not govern it.
- Systems: daemon (store, merge orchestrator, server, CLI), `bin/land-direct.sh`
  (reports its landings), the merge-queue skill. The webapp regenerates TS
  bindings only.
- Non-additive changes: none. `MergeQueueStatus` gains a field.

## Iteration sequence

Default sequence: the transport is the existing Connect service; the endpoint
inventory is `ListMergeLandings` (read) and `RecordDirectLanding` (the
script's report), plus `MergeQueueStatus.size`; the canonical
`oneof result { success; error; }` convention carries forward; then each
shape in turn.

## Landed changes

### `MergeQueueStatus.size` (`MergeQueueStatusSize`)

- What: five counts beside the entries: `in_queue`, `running`, `waiting`,
  `out_of_line`, `off_queue`. The first is the sum of the next three, which
  partition `entries` by stage; `off_queue` is the length of `off_queue`.
- Why: an agent asked how big the queue is reads numbers instead of counting
  lists by stage.
- Consequence accepted: the counts duplicate what the lists already say. They
  are resolved by the server from the same members it lists, in one function
  (`server.mergeQueueStatusOf`), so the two cannot disagree; a test pins the
  counts against the entries.
- `in_queue` is a sum of its siblings, which the completeness rule would
  normally question. It stays because "how many are in the queue" is the
  question agents get, and it is not a returned/total pair: nothing is
  omitted.

### `merge_landing.proto` (shared)

- What: `MergeLandingThroughQueue` and `MergeLandingOtherwise`, two messages,
  and `MergeLandingOtherwisePath`, a oneof of `queue_bypass`,
  `deploy_mediator`, `direct`.
- Why two entry messages rather than one with a path oneof holding a `queue`
  arm: a queue landing in the otherwise list, or a bypass landing in the
  queue list, is then unrepresentable. "Merged through the queue means the
  queue list only" is a fact of the schema.
- `MergeLandingThroughQueue.workspace` is always set (every queued merge runs
  in a workspace); `MergeLandingOtherwise.workspace` is optional, because
  land-direct.sh can run outside any workspace's worktree.
- `MergeLandingWorkspace` carries the name as it stood at landing time, so a
  closed or renamed workspace still reads as the agent knew it.
- `direct` exists because the daemon cannot stop land-direct.sh being run by
  a caller holding neither role; recording it honestly beats dropping it.

### `ListMergeLandings`

- What: repository (required), `since_ms` (required), `until_ms` (unset =
  now), inclusive at both edges. Success: the repository, the resolved
  window, a coverage oneof (`complete`, or `partial` with
  `recorded_since_ms`), and the two lists, each with its `count`.
- Coverage is a oneof, not a timestamp beside the lists, so "is this window
  fully recorded" is an arm, never a comparison every agent makes.
- `count` duplicates each list's length, on the owner's request that the
  answer carry counts.

### `RecordDirectLanding`

- What: repository, branch, landed head, optional caller. Success answers the
  path recorded. Errors: unknown repository, unknown caller.
- The caller says nothing about the path. The daemon classifies: the
  repository's deploy mediator's branch is `deploy_mediator` whoever ran the
  script (the daemon's finish runs it in the main checkout, whose workspace is
  the main workspace); else a caller holding the repository's queue bypass
  mode is `queue_bypass`; else `direct`.
- Rejected alternative: a flag on land-direct.sh naming the path. It would
  make the split a convention of the caller again, which the request rules
  out.

## Store and helper (consuming code)

- The persistence model is wsm `merge_landings` (`daemon/internal/wsm/mergelandings.go`),
  keyed by (queue key, head); the protocol model is the two entry messages.
  The mapping's one owner is `server.mergeLandingsOf`.
- Recording began is `merge_landings_recording.began_at`, stamped by the DDL
  that creates the table, so a migrated store and a fresh one each know their
  own start.
- Every landing site calls `merge.orchestrator.recordLanding`
  (`daemon/internal/merge/landings.go`): the queue's landing
  (`run.recordQueueLanding`, from `concludedTerminal`), the mediator's
  (`remediationLand`), and the script's report (`RecordDirectLanding`).
  `TestEveryLandingPathWritesItsRecord` drives one landing per
  `wsm.LandingPaths` entry and fails for a path with no site.
- Only landings onto the main branch are recorded: a queue merge into a
  parent workspace's branch (`deployTarget.toMain` false) and a deploy that
  lands nothing locally are not landings onto the repository.

## No backfill from git history

A one-time backfill is not sound, so none is made:

- The path is exactly what git cannot say. The only signal is the
  `docs: changelog line for the queue-bypass landing of <branch>` commit, a
  message convention the request rules out as a source.
- That signal cannot tell a queue-bypass landing from the deploy mediator's:
  both run land-direct.sh and both write the same changelog commit.
- A repository with no remediation changelog leaves no such commit, so its
  direct landings would be counted as the queue's.

Windows reaching back before recording began are answered `partial`, naming
the instant, instead.
