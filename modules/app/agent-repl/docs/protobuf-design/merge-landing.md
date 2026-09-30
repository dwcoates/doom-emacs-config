# Merge landing: a PR that already merged upstream, and a local merge that lands only once gated

Two defects in how the daemon lands a workspace, both reached on 2026-09-28/29.

A workspace whose GitHub PR has already merged into `origin/<default>` has no
way to say so. The `/create-or-update-workspace` skill's `merge --pr-was-merged`
writes `"pr_was_merged": true` on the command file's merge entry, meaning
"advance the local default branch to `origin/<default>` and close the
workspace, instead of merging its commits", and `create-or-update-pr` and
`check-cicd` send it once they have confirmed the PR merged. The daemon never
modelled it: the command-file reader dropped the field and ran an ordinary
local merge of commits that were already upstream, and now that the reader
decodes strictly it quarantines the file instead. Either way the footer never
honestly reaches `merged` for that workspace.

A local merge lands on the target BEFORE its test gate runs: the daemon runs
`git merge --no-ff` in the target worktree, gates that merge commit, and
relies on resetting the target on failure. A daemon exit mid-merge (lease
c8a3a664006f46c1, 2026-09-28) left an ungated merge commit on master, because
boot recovery released the orphan lease without undoing the merge. The owner's
required flow is: rebase the workspace branch onto the target tip, resolving
conflicts along the way; run the gate on the rebased branch, repairing
failures on the branch; and only once every suite passes, merge the rebased
branch into the target as a non-fast-forward merge.

## Core design principles

- **A merge reaches no client until the turn that asked for it has ended
  (owner, 2026-09-29).** The daemon reports nothing about a workspace's merge
  (bubble, footer status, substatus, activity) while the turn that requested
  it is still in flight, and this holds structurally, not by a surface
  choosing to hide something.
  - Consequences for the contract: no client-facing arm, substatus or
    activity names a "waiting for the turn to end" state; the first merge
    fact any client sees is already past the turn's end. The first footer
    substatus of a merge is "enqueued" (or later).
  - Reopens: today's `frontend.v1.FooterSubStatusMergingEnqueuing` ("the merge
    is being enqueued"), which exists only because the merge was reported
    before admission.
  - Does not claim: anything about when the daemon RECORDS the request. The
    request is still recorded when it arrives, so it survives a daemon exit
    before the turn ends; only its admission and every report of it wait.
- **Footer text is for a human user (owner, 2026-09-29).** Every substatus
  and activity line is short plain words a non-developer reads, never an
  identifier spelling and never an internal mechanism (webapp `AGENTS.md`,
  "FOOTER TEXT IS FOR A HUMAN USER").

- **Each failure mode is its own turquoise status; the substatus is the
  area (owner, 2026-09-29).** Something that went wrong while the workspace
  stays usable gets a bespoke status naming the failure ("merge failed",
  "turn failed"), and its substatus names where it failed ("conflicts",
  "tests"). There is no catch-all "needs attention" status.
  - Does not claim: what "turn failed"'s substatuses are. The owner left that
    separation open, and it is outside this change.

- **A merge runs in the workspace that asked for it (owner, 2026-09-30).**
  No workspace is ever created for a merge. The requesting workspace's feed
  carries the merge bubble, its footer carries the merge status, and its own
  session does the conflict resolution and test fixing, because that session
  holds the context that makes the resolution good.
  - Consequences for the contract: a merge request names the requesting
    workspace AND what it merges (its own branch, another workspace's branch,
    a branch that is no workspace, or its own branch already merged upstream).
  - Reopens: the `claude-repld merge-queue -branch` landing workspace
    (`merge-queue/<bare>-landing`), which exists only because the queue could
    merge nothing but a workspace.
  - Does not claim: where the rebase's files live. The lead ruled (below)
    that the rebase happens in a worktree checked out on the branch being
    merged.
- **A requesting workspace may stay open after its merge (owner,
  2026-09-30).** A request can ask the daemon not to close the workspace
  once its own branch lands, so it can go on to further merges.
- **If master moved while a merge ran, the merge starts over (owner,
  2026-09-30).** The whole process repeats from the rebase onto the new tip;
  a gate result is only ever trusted for the exact tip it ran on.
- **The test log is a link (owner, 2026-09-30).** The merge bubble's tests
  tab names the failure log statically and draws it in the link blue;
  clicking it opens the log in Emacs in a split on the right, beside the
  agent-repl panels.
- **The owner pre-authorized every remaining design decision on this change
  (2026-09-30: "everything else you can implement as you see fit").** The
  lead's rulings are recorded below as they are made, each with its reason.

## Lead rulings (2026-09-30), under the owner's pre-authorization

- **The rebase runs in a worktree checked out on the branch being merged.**
  The requesting workspace's own worktree for its own branch; the branch's
  existing worktree when it has one; otherwise a worktree the daemon creates
  for the branch under its state directory and removes after the merge. The
  repair turns are the requesting workspace's own session, told the directory
  they work in.
- **Merging another workspace's branch closes that other workspace once the
  branch lands**, since its work is done; the requesting workspace is never
  closed by a merge of a branch that is not its own.
- **`keep_open` belongs to the own-branch source only.** It means nothing
  for the other sources, which never close the requester, so it rides inside
  that source's arm rather than beside the source.

## Context (step 1)

- **The daemon does not verify a PR-merged assertion (owner ruling, 2026-09-29).**
  The signal exists to prompt the daemon to take the requisite action, which
  is to pull the default branch in the repository's main worktree, not to
  re-establish that the PR merged. The callers that send it
  (`create-or-update-pr`, `check-cicd`) already confirmed the merge on GitHub
  before sending, so a daemon-side containment check would duplicate their
  confirmation.
  - Consequence: the daemon trusts the caller. A caller that sends the signal
    for a PR that did not merge gets its workspace closed and marked merged
    with its commits never landed; the guard against that is the caller's
    confirmation, not the daemon.
  - Consequence: the pull itself can still fail (a local default branch that
    has diverged from upstream, a dirty main worktree, no network). That is an
    ordinary failure of the action the signal asked for, surfaced as the
    merge's failure, and is not a verification of the assertion.

- **The merge bubble stays in the feed, collapsed to one line (owner
  ruling, 2026-09-29).** Unexpanded it is a single line, the way a subagent row
  is; expanded it looks as the merge bubble does today, tabs included. The
  agent responses its tabs carry today (conflict resolution, test
  remediation) stay in their tabs AND are also drawn in the workspace's main
  feed as ordinary turns.
  - Consequence: each such response is drawn in two places. Under figma→idl
    that is two resolved copies of one fact, one per component, never one
    shared message the client fans out.

- **The footer's merging substatuses and their activity lines (owner,
  2026-09-29).** Status is "merging" throughout; the substatus is the phase,
  in plain words, and the activity line under it is the phase's own salient
  detail, cleared when the substatus ends.
  - "enqueued k/n": n is how many merges are WAITING in the repository's
    queue, not counting the one being worked on; k is this workspace's
    1-based place among them, so 1 means next to be taken and there is never
    a 0. Activity: the workspace currently being merged and its substatus,
    coarse (one line per substatus change of that merge, never finer), so the
    line is not chatty.
  - "preprocessing": the configured before-merge prompt is running.
    Activity: the prompt text itself.
  - "rebasing k/n": k of n commits replayed onto the target's tip.
    Activity: the rebase command running for the current commit, and any
    failure it hits.
  - "conflict resolution": the agent is resolving a rebase conflict; the
    merge usually returns to "rebasing" afterwards. Activity: the conflicting
    commit and how many files conflict, then the resolution agent's own
    notifications, the same way a normal turn's take the line.
  - "testing": no detail in the substatus. Activity: a line as each suite
    starts, and as each finishes, a passed suite preceded by a green check.
    The expanded footer lists every suite; that section opens and is selected
    when "testing" begins and closes, returning to whatever other sections
    stand, when it ends. Its shape follows the existing expanded-footer
    practice (the owner leaves that design to the orchestrator).
  - "fixing attempt x/y": the agent is repairing failed suites; the merge
    returns to "testing" afterwards. Activity: which suites are being fixed.
  - "committing": the non-fast-forward merge commit into the target.
    Activity: that merge commit's first line.
  - "updating main": a PR-merged merge pulling the default branch in the
    repository's main worktree. Activity: the step it is on (fetching, then
    fast-forwarding to the new tip).
  - "postprocessing": the configured after-merge prompt is running.
    Activity: the prompt text itself.
- **A failed merge leaves the queue and hands the workspace back (owner,
  2026-09-29).** When conflict resolution or test repair fails, the merge is
  removed from the queue at once, which unblocks the merges behind it and
  ends the merging status. The workspace's status becomes "merge failed",
  with the substatus "conflicts" or "tests": the workspace is usable and the
  user decides what happens next.
  - SUPERSEDED the same day: this entry first read status "idle" with
    substatus "conflicts failed" or "tests failed". Idle is green (ready),
    and a failed merge is something that went wrong while the workspace stays
    usable, which is turquoise; the existing turquoise "merge failed" status
    already means exactly that, so it is kept and the failure's area moves to
    its substatus (owner, 2026-09-29).
  - Reopens: the 2026-09-28 ruling that made a stopped merge the
    `merge_conflict` status (with its `parked` step, whose composer delivered
    what the user typed to the merge's agent) and a failed merge the
    `merge_failed` status. Neither state survives: nothing is parked, and the
    user's next prompt is an ordinary turn of the workspace.
  - The substatus stands like every other substatus: until the next one
    replaces it (owner, 2026-09-29).
  - A failed conflict resolution LEAVES THE REBASE IN PROGRESS, exactly
    where it stopped (owner, 2026-09-29). The point of handing the workspace
    back is for the user to help move the rebase along, so the daemon never
    aborts it.
- **"fixing attempt x/y" (owner, 2026-09-29).** The fixing substatus names
  its attempt: x is the current attempt and y the maximum number of fix
  attempts, a structural invariant of the merge (the same bound that decides
  when fixing has failed), never a guess.

## Landed changes

### 1. The merge runs in the requesting workspace; rebase first; nothing parks (2026-09-30)

- **Decided by** the owner's rulings of 2026-09-29 and 2026-09-30 above,
  with the lead's rulings under the owner's pre-authorization.
- **What changed, on the wire:**
  - `agentrepl.v1.MergeWorkspaceRequest.source` names what the requesting
    workspace merges: its own branch (with `keep_open`), another workspace,
    a branch that is no workspace, or its own branch already merged upstream.
    New refusals: `unknown_source_workspace`, `unknown_branch`.
  - `agentrepl.v1.OpenInEditorRequest` takes a `target` oneof: a workspace
    file (the old path and line), or a merge test log named by the
    `frontend.v1.FeedMergeTestLogToken` the bubble served. The log lives in
    the daemon's state, outside the worktree, so it is named by token rather
    than path. New refusal: `unknown_merge_test_log`.
  - The merge bubble's tabs: `rebasing` (with progress and narration)
    replaces the retired `merge` tab; `committing` and `updating_main` are
    new; `conflicts` and `fixes` lose `parked`; `fixes` carries its attempt
    and the maximum; `tests` carries the round's log link.
  - The footer: the merging substatuses are enqueued k/n, preprocessing,
    rebasing k/n, conflict resolution, testing, fixing attempt x/y,
    committing, updating main and postprocessing; every older arm is
    retired. Each step's own activity line is one salient arm,
    `merge_step`, whose arm is the step, replacing `merging_commit`.
    `merge_failed` gains its area substatus (conflicts, tests, other) and is
    turquoise; the `merge_conflict` status is retired. The expanded footer
    gains a 🧪 chip and a merge tests panel, which the daemon focuses when
    testing begins through the existing focus edge; the panel empties when
    testing ends, which unsets the chip.
  - The roster: `merge_enqueuing` and `merge_conflict` are retired;
    `merge_failed` is turquoise.
  - The host stream: `merge_parked` is retired.
- **Consequences, accepted:**
  - Emacs needs no change to open the log on the right: its one shared
    popup (`lisp/popup.el`) already opens a right side window at half the
    frame; only the request's target changes.
  - A merge of a branch that is no workspace no longer makes a landing
    workspace, so `claude-repld merge-queue -branch` and the merge-queue
    skill send `source.branch` from the calling workspace instead.
  - `fixes` round and `attempt` agree by construction only if the daemon
    uses one bound for both; the daemon states that bound once.
- **Obviated and removed in the same change:** `FooterStatusMergeConflict`,
  `FooterSubStatusMergingEnqueuing` and the other retired substatus
  messages, `FooterStatusActivityMergingCommit`, `FeedMergeTabParked`,
  `FeedMergeTabParkedLine`, `FeedMergeTabMerge`, `FeedMergeMergeLine`,
  `RosterRowStatusMergeEnqueuing`, `RosterRowStatusMergeConflict`,
  `HostComposerMergeParked`: each was referenced only by the arm retired
  here.
