# Merge bubble: nested rebasing and tests tabs

## Problem

The owner wants the merge bubble's strip to carry ONE tab for rebasing and ONE
for tests, instead of a flat run of rounds (`rebasing | conflicts |
rebasing (2) | tests | fixing | tests (2) | …`).

- **Rebasing** holds a subtab per replayed commit. A commit whose replay
  needed conflict resolution carries, inside its own subtab, the agent stream
  of that resolution.
- **Tests** holds a subtab per full suite run. Each run's subtab holds two
  sub-subtabs: the run's test output, and the agent stream of the fixing work
  that followed it.

Today `frontend.v1.FeedMergeTabRebasing` carries only a replay count and
narration lines, so no per-commit subtab can be drawn from the wire, and a
conflicts tab does not name the commit it resolves. The daemon
(`daemon/internal/merge/process.go`) knows every commit's sha and subject and
which commit each conflict stopped on.

## Systems involved

- `proto/src/frontend/v1/feed.proto` (the merge tab family).
- The daemon's merge run (`daemon/internal/merge`): tab rounds, the progress
  record and its resume paths.
- The webapp's merge body (`webapp/src/feed/merge/`).

## Rulings

The owner answered "continue" to the proposed layout and questions
(2026-10-12), which adopts every recommendation as made:

- The layout is the two drawings in the `FeedMergeTab` family comment: one
  rebasing tab whose subtabs are commits, one tests tab whose subtabs are runs,
  each run holding an output and a fixing sub-subtab.
- Nesting is carried by `FeedRow.parent`: every subtab is its own sub-feed
  row, parented to its top-level tab's row, and every agent row is parented to
  the subtab row that holds it. The webapp groups nothing itself.
- A merge that starts over replays again under the SAME rebasing tab; the
  second replay's commit subtabs carry round 2 ("(2)").
- A cleanly replayed commit's subtab shows its short hash, its subject and its
  own narration lines.
- A commit whose conflict `git rerere` settled shows a "resolved as before"
  line and no agent stream.
- A run that passed has no fixing sub-subtab (it is dropped, not greyed).

## Landed changes

### 1. The tab family is nested (feed.proto)

- **What**: `FeedMergeTab` gains `rebase_commit` and `tests_run` arms; the
  `conflicts` and `fixes` arms are retired. `FeedMergeTabRebasing` and
  `FeedMergeTabTests` gain a `waiting_on_user` arm; the tests tab's `suites`
  and `log` move to `FeedMergeTabTestsRun.output`. A conflict resolution is
  `FeedMergeTabRebaseCommit.conflict`; a fixing attempt is
  `FeedMergeTabTestsRun.fixing`.
- **Why**: the owner wants one rebasing tab and one tests tab, with each
  commit and each run a subtab and its agent stream inside it.
- **Consequences**:
  - The daemon's ledger rounds (`wsm.TabInterval`) and its progress record
    keep their internal kinds (`conflicts`, `fixes`): they are the daemon's
    own bookkeeping, and the footer and resume paths read them. Only the
    projection onto feed rows changes (`daemon/internal/merge/tabs.go`).
  - The merge's output address for a conflict resolution is the commit's
    row, and for a fixing attempt the run's row.
  - The rebasing and tests tabs are no longer append-only: they are one row
    each, drawn live again when a merge starts over. Every other top-level
    tab keeps its rounds.
  - ACCEPTED COST: durable rows recorded before this change that carry the
    retired `conflicts` or `fixes` arms decode with no kind. The daemon does
    not draw them on restore (it records each at INFO), so an old merge
    bubble opens without those tabs, and that old resolution's agent rows
    draw as loose rows beneath its strip.

### 2. A fixing attempt is its own row (feed.proto)

- **What**: the fixing sub-subtab is the `tests_run_fixing` arm
  (`FeedMergeTabTestsRunFixing`), a row parented to its run's row, instead of
  an optional field on `FeedMergeTabTestsRun`. A fixing attempt's agent rows
  are parented to the fixing row.
- **Why**: a row is replaced whole on every push. With the fixing on the run's
  row, every change of the fixing's state (live, waiting on the user,
  settled) had to republish the run's suites and their output. The daemon
  would then have had to keep that output in memory, and keep it in its
  progress record so a resumed fixing attempt could redraw the run. As its
  own row, the fixing is published alone, and the run's row is written only
  by the run.
- **Consequences**: the run's subtab draws its output from its own row and
  its fixing sub-subtab from its child row. A commit's conflict resolution
  stays on the commit's row, because the owner asked for it as a section of
  the commit's tab rather than a tab of its own.

### 3. Rebased onto master's async merges (feed.proto, daemon, webapp)

- **What**: the branch was rebased onto master after master had landed async
  merges, the pick tab, and merge-tab interrupts.
  - `FeedMergeTab.rebase_commit` and `tests_run` moved to tags 13 and 14,
    and `tests_run_fixing` to tag 15, because master's `pick` arm took 12.
  - The per-tab `workspace_switch` fields are gone from
    `FeedMergeRebaseCommitConflict` and `FeedMergeTabTestsRunFixing`, as
    master retired them on every agentic tab. An async merge's agent rows are
    mirrored into the requester's bubble, parented to the same row: a commit's
    subtab for a conflict, a run's fixing row for a fix.
  - A merge-tab interrupt (`agentrepl.v1.InterruptRequest.merge_tab`) names
    the row the live agent stream is drawn in: the pick or a prompt tab, a
    commit's subtab, or a fixing row. The run records that row when it stands
    the agentic round live (`run.liveAgentic`).
  - `mergetab.Agentic` counts a commit subtab as agentic only once it carries
    a conflict section.
  - The stop control sits above every running agent stream, including a
    commit's conflict section.
- **Why**: the owner asked for the branch to be rebased onto master and
  queued.
- **Consequences**:
  - Master's conflict retries (up to `MaxConflictAttempts` per commit) and
    restart re-prompts keep each attempt in the same commit's subtab. A retry
    ends the attempt's section with its line, and the next attempt opens a new
    section.
  - Master's fixing re-prompt after a restart reuses the run's fixing row.
