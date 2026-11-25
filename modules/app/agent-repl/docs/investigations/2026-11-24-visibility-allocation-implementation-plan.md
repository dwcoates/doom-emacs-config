# Visibility and workspace resource allocation: implementation plan

Branch: `outline-subsection-visibility` (workspace `outline-subsection-visibility`).
Companion records, read both first:

- `docs/protobuf-design/2026-11-24-sidebar-visibility-and-allocation.md` (the
  design record; its "settled rules" section is partly SUPERSEDED by this plan
  where they differ: this plan is newer and wins).
- `webapp/src/sidebar/AGENTS.md` (the harness home of these rules; update it
  to match this plan as part of the work).

Every rule below is an owner ruling of 2026-11-24 unless marked otherwise.
Where this plan says "decided", the owner settled it; where it says
"assumption", the implementer may proceed on it and must record it in the
design record.

## 1. Where the branch stands

Already on the branch (committed; see `git log master..HEAD`):

1. Folds cover by the nearest container only (`openRegion.fold`,
   `daemon/internal/resolve/sidebar/taborder.go`).
2. `frontend.v1.RosterOpenRow.fold_cover` (uncovered, hidden, surfaced)
   replaces `surfaced`; the webapp hides rows by `.covered` / `.surfaced`
   (`webapp/src/sidebar/row.ts`, `webapp/src/styles.css`); Emacs decodes it
   (`lisp/wire-roster.el`). Subsection headers are never hidden by a section
   fold.
3. Viewed: cleared ONLY by a turn's start (`resolver.SetTurn`,
   `wsState.clearViewedForTurn`), never by a turn's end or any status change.
   Persisted through the viewed sink into wsm `viewed_status`, seeded at boot
   (`wsState.restoreViewed`).
4. Ten-minute view exemption (`sidebar.ViewExemption`, `wsState.viewExempt`).
5. Every recorded interaction logged at INFO (`daemon.workspace.interaction`,
   `daemon/internal/workspace/select.go`).
6. Recency walk reaches every open workspace, unheld ones selected by ref
   (`lisp/commands.el` `agent-repl--recency-walk`, `lisp/host.el`
   `agent-repl-host-request-switch`, `lisp/roster.el`
   `agent-repl-roster-open-entries`, `lisp/recency-reach.el`).
7. Mediators and queue-bypass holders always visible and running
   (`wsm.Workspace.AlwaysShown`, `QueueBypassHeld` read on the record).
8. Page verdict `frontend.v1.RosterRow.page` (replaces `dormant`), derived in
   `daemon/internal/resolve/sidebar/page.go` (`pageVerdict`, page clock);
   Emacs follows it (`lisp/page-unload.el`); page reasons `allocated` /
   `deallocated`.
9. Shim hold `wsm.ShimHold` derived beside it (`shimHoldOf`), the cache keeper
   parks by it (`daemon/internal/cachelapse/standdown.go` `parkWaits`,
   `Deps.Hold`, `Keeper.ShimHoldChanged`; wired in
   `daemon/cmd/agent-repld/graph.go`, `shimHoldForwarder` in `forward.go`).
   `StandDownGrace` is retired.
10. Close and backlog interrupt a running turn first
    (`workspace.verbs.interruptBeforeLeaving`, `close.go`, `select.go`); a
    backlogged workspace gets one last keep-alive beat (`cachelapse`
    schedule's `spent`, `wsm.Workspace.KeptWarm`).
11. Docs: `webapp/src/sidebar/AGENTS.md`; the definition of visible; `SPC p p`
    scope (`lisp/commands.el` `agent-repl--switch-candidates`).

## 2. Why the first landing failed (must all be fixed)

The merge gate (`~/.claude-emacs/merge-logs/ea10c6254def4695-tests-1.log`,
63 cases) failed for these reasons. Rerun these exact cases first with
`agent-repld test run -rerun` (the daemon recorded them for this branch), and
read the archive for each.

1. THE SHIM HOLD PARKS A SESSION BEFORE ITS FIRST TURN. `shimHoldOf` answers
   no hold for an idle, visible, unselected, viewed workspace, so the keeper
   parks a session brought up for a prompt, a cold gate's answer, or a merge
   step before the turn starts. Daemon integration tests that failed for it
   include `TestADisplacedTurnIsCapturedAtLeaseAcquisitionAndResubmittedExactlyOnceAtRelease`,
   `TestAHiddenWorkspacesCutTurnIsResumedByTheSuccessorsBoot`,
   `TestAMergeOverATurnWithDetachedWorkEndsOnlyTheTurnAndWaitsForTheWork`,
   `TestAnAnsweredGateWhoseReopenFailsStandsAgain`,
   `TestAnswerColdGateClearEchoesExactly`, `TestAnswerColdGateCompactEchoesExactly`,
   `TestAPromptHeldAtTheColdGateIsSentByCompactAndResume`,
   `TestARelaunchedDaemonReplaysARepairTurnInItsTabAlone`,
   `TestARestartOverAMergeWaitingForItsWorkspaceRestartsAndTheMergeLands`,
   `TestCreateWorkspaceDeployHooksAreRecordedAndReadBackByALaterDeploy`,
   `TestCreateWorkspaceUnderTheVendorGuardAloneStartsTheSession`,
   `TestKillWorkspaceLeavesTheWorktreeAndBranchIntact`,
   `TestNukeWorkspaceKillsTheLiveSessionBeforeAnyGitCommandRuns`,
   `TestRosterStatusFollowsTheSessionLifecycle`,
   `TestStartSessionResumeColdStandsAGateBlockingReopenUntilAnswered`, and e2e
   `TestTurns`, `TestWebappLayer`, `TestAForksFeedReadsItsParentsTurnAboveItsOwn`.
   Fix: section 3.1's initializing guard (structural, no timer).
2. `lisp/test-integration-roster.el`: 40 cases time out ("waiting for the
   standing tab"). Most likely the roster JSON its fake daemon or fixtures
   push lacks the now-required `RosterRow.page` and `RosterOpenRow.foldCover`,
   so Emacs's strict decoder refuses every push. Find the fixture builder
   (search `lisp/testsupport/fakedaemon` and `lisp/test-integration-*.el` for
   `visibility` / `unselectedVisible`) and add both fields. Verify by reading
   the decode-refusal records in the archive.
3. `lisp/test-commands.el`: `agent-repl-test-commands-recency-ring-leaves-out-a-folded-tab`
   and `agent-repl-test-commands-recency-walk-steps-through-the-shared-helper`
   assume the old walk (drawn tabs only, `agent-repl--step-workspace`). The
   first asserts the retired behavior: replace it with the new rule (the ring
   holds a folded workspace). The second pins that the walk goes through a
   shared helper: make `agent-repl--recency-walk` satisfy the helper the test
   names, or restate the test on `agent-repl--step-workspace-to` if that is
   now the shared helper (read the test first).
4. `lisp/test-core.el`: one case (read the archive).
5. `TestCloseWorkspaceBlockedCarriesTheQuietChecksEvidence`: assumes a running
   turn blocks a close. A close now interrupts it (and per 3.6 ends the
   session); keep the test's intent with a blocker that still blocks (live
   detached work, held prompts, a queued merge).
6. `TestEveryGoFileIsGofmtClean`: fixed on the branch (commit `82a53a26ed`);
   rerun to confirm.

## 3. The rules to implement

Definitions:

- VISIBLE: a workspace that gets a NUMBER in the sidebar's Open section
  (`frontend.v1.RosterOpenRowTab` numbered; the daemon is the source of that
  number, `taborder.go`). That is an open workspace that no fold has rendered
  invisible as inactive. Backlogged, closed, killed, nuked and merged
  workspaces are never visible. A folded-away row stays visible (surfaced)
  while it is selected, working, unread, starting (initializing), priority
  P0.5 or P1, a standing mediator, or a queue-bypass holder; the rows above a
  visible row stay visible.
- Do NOT use the word "keeper" anywhere in code or harness; say "visible" and
  name the condition.
- ALLOCATIONS: a workspace's webapp page, its shim, and Emacs's slot for it
  (perspective, top bar, composer, host stream). They closely correspond to
  visibility; the SELECTION is the one workspace that holds them without
  being visible. `SPC p p` (and the recency walk) may select a workspace that
  is not visible or not even open; it holds its allocations while selected.
- NOT VISIBLE AND NOT SELECTED MEANS NO PAGE, NO EMACS SLOT, AND A PARKED
  SHIM, structurally: every allocation reads visibility first.

### 3.1 Shim: never parked while

1. INITIALIZING: spawned, starting, reviving, or brought up for work whose
   turn has not started yet (a prompt, a cold gate's answer/resume, a merge
   step, a settings pick). There must be NO GAP between initializing and
   working: design it so the hold passes from "brought up for work" to
   "working" under one lock at the turn's start (the prompt queue's delivery
   path, `daemon/internal/promptqueue`, already serializes delivery; its
   `Reviving(ws)` already shields a revival until the prompt is handed over;
   extend that idea so the roster/keeper see one continuous busy span:
   a pending submission counts as working until its turn starts or the
   submission is withdrawn). No timer, no retry: structurally impossible.
2. WORKING: a turn, detached work, a permission ask or question, /clear or a
   compaction.
3. A keep-alive beat awaiting its answer (existing rule).
4. SELECTED: the selection holds a running shim up. Selecting a workspace
   whose shim is parked does NOT unpark it (select, look, open and history
   reads leave a parked session parked; `wsm.Workspace.Runs`'s comment that
   selection revives must be corrected to match, and the code with it if it
   revives).
5. Remote control on (existing rule).
6. A standing mediator, a queue-bypass holder, or a one-shot.

### 3.2 Shim: otherwise it parks when

1. IDLE HOLD ENDS: 15 minutes after the LATER of its last turn's end and the
   user's last interaction with it (decided). Example: turn ends 0:00, user
   views 0:05, it parks 0:20 unless viewed again. This replaces the current
   `shimHoldOf` unread/left rules (the 10 s / 15 min "leave grace" is a PAGE
   rule only, no longer a shim rule). The last interaction instant is
   `RosterRowLastInteracted` / `wsState.interactedAt` / wsm
   `last_interacted_at` (durable, so it survives restarts).
2. It stops being visible.
3. A keep-alive beat it was restored only for has been answered (existing).
4. SYNC MERGE: parked when it waits in line (existing
   `drain.Controller.MergeQueuedDown`); UNPARKED when it is evicted from the
   queue, and when it reaches any active stage (rebase, test gate, test repair,
   conflict resolution, any merge prompt). Verify the unpark on eviction
   exists; add it if not.
5. BACKLOGGED: its running turn is interrupted (done), its page unloads, its
   shim parks after one more keep-alive beat (done), then no further beat.
6. It stands at a cold gate (existing).
7. Settings visit: a shim a model / permission-mode / effort pick started
   parks when the user switches away with no turn run (existing
   `drain.Controller.VisitDown`; keep as is, decided).

### 3.3 Close, kill, nuke

- A close ENDS THE SESSION the same way a kill does (decided): its running
  turn is interrupted first (done), the shim is parked until its next
  keep-alive, that one last beat goes out, and it is never unparked for a
  later keep-alive. Today close leaves the session up; change it so close's
  session lifecycle equals kill's in this respect. Read how kill ends a
  session (`daemon/internal/workspace`, kill verb) and how a closed
  workspace's last round works (`cachelapse` `spent`, closures.go) and make
  close and kill share one path (extract a helper if both do the same steps).

### 3.4 Page

- Never unloaded while shown on screen.
- A workspace that is not visible (and not selected) has no Emacs slot and no
  page at all: folded away while not visible, backlogged, closed, killed,
  nuked, merged, or left after the recency walk / `SPC p p` reached it while
  not visible. Emacs's slot must be tied structurally to visibility (the
  number) plus the selection; it already opens a tab exactly for rows visible
  or selected (`agent-repl-roster-desired-tabs`, `lisp/roster.el`); make sure
  every allocation path reads that one fact and document it.
- A visible workspace off screen unloads its page by the daemon's verdict:
  - viewed: 10 s after the user switches away, or 15 min when the visit being
    left first marked it viewed (`PageLeaveGrace`, `PageFirstViewGrace`);
  - unread: 1 hour after its turn ended (`PageUnreadHold`);
  - a one-shot or a SYNC merge (async merges are ordinary workspaces):
    1 MINUTE after the user switches away (decided; replaces "at once"), and
    1 minute after it becomes a one-shot or enters its sync merge while not
    selected;
  - working, starting, top priority: loaded.

### 3.5 Viewed

- Raised only by the user's interaction: feed scroll, composer typing, feed
  navigation keys, a followed link. Not within 10 minutes of a turn's end
  (still counts toward recency). Not before the row has any status.
- Lowered ONLY when a turn starts (any turn: a prompt, /clear, compaction, a
  merge step, another agent's prompt; a keep-alive is not a turn).
- Nothing else changes it: not a turn's end, a deploy, a restart, a park, a
  revival, a fold, a link drop, a selection, a backlog, a reopen.
- Durable in wsm `viewed_status`; seeded at boot. Verify a handover/deploy
  never lowers it: add an integration test that a viewed workspace stays
  viewed across a daemon handover (find the existing handover integration
  harness in `daemon/internal/...` tests or `e2e/`).

### 3.6 Visibility changes (for the record and the tests)

- Becomes visible: its nearest folded container opens; while folded away it
  becomes selected / working / unread / initializing / P0.5-P1 / mediator /
  bypass holder; a row beneath it becomes visible; it is unbacklogged,
  reopened, a merge workspace revealed, or moved out of a fold.
- Stops being visible: a fold closes over it while none of those hold; while
  folded away the last of those stops holding; it is backlogged, closed,
  killed, nuked or merged; it is moved into a folded container.

### 3.7 Naming: hidden becomes backlogged

- The section the sidebar labels "Backlog" is BACKLOGGED in code and harness,
  never "hidden" (which reads as "not visible"). Rename every identifier,
  comment, doc and harness text whose "hidden" means the backlog: Go
  (`Workspace.Hidden`, `HiddenAt`, `InHiddenSection`, `SetHidden`,
  `recordHidden`, `hiddenSection`, `HiddenSectionLabel`, `KeepaliveHeld`'s
  doc), the store (`workspaces.hidden`, `hidden_at`: a migration renaming the
  columns, additive-safe per `daemon/internal/wsm/migrate.go`'s conventions),
  protobufs (`frontend.v1.RosterHiddenSection` and its rows,
  `agentrepl.v1.SetWorkspaceHidden` and its messages, `SendHiddenKeepalives`,
  status fields named `hidden`), Emacs (`lisp/`), the webapp (classes such as
  `hidden-section`, `HIDDEN_SECTION_CLASS`), the CLI verbs and their skill docs
  (`/agent-repl-workspaces`), and the metrics catalog.
- Do NOT rename "merge hidden" (`MergeHidden`, a hidden merge workspace drawn
  nowhere) or DOM `hidden`; those mean something else.
- Use `/create-or-update-protobufs` for the protobuf renames and
  `/agent-repl-update-harness` / `/create-or-update-skill` for harness text.
- This is the last phase; land it in its own commits after everything above
  passes.

## 4. Order of work

1. `agent-repld test run -rerun` to see the recorded failures as they stand.
2. Fix 2.2 to 2.6 (fixtures, recency tests, core test, close test).
3. Implement 3.1 (initializing guard, no gap), 3.1.4 (selection does not
   unpark), 3.2.1 (15 min after later of turn end and last interaction),
   3.2.4 (sync merge unpark on eviction), 3.3 (close ends the session like
   kill), 3.4 (one-minute rule for one-shots and sync merges). Each with unit
   tests of every branch and its error paths, recorded through the canonical
   logger.
4. Rerun the recorded cases until all pass.
5. Update `webapp/src/sidebar/AGENTS.md`, the design record, and every doc the
   change makes stale (`AGENTS.md` keep-alive section, `daemon/AGENTS.md`).
6. Section 3.7 rename.
7. Consolidation sweep over `git diff master...HEAD`; extract helpers in their
   own behavior-preserving commits with their own tests.
8. Land: the merge workspace lands it; version part `minor`; changelog line
   "Sidebar folds follow the nearest container, the viewed mark changes only
   when a turn starts, and the daemon decides which workspaces hold a page and
   a running shim; the hidden section is now called the backlog in code."

## 5. Testing rules

- Run only the tests the work added or changed, named one by one, through
  `agent-repld test run`; never the full suite (the gate runs it).
- A test name the runner does not know makes it refuse with no log: never
  pipe its output into `grep "$LOG"` with an empty LOG.
- Never `time.Sleep` in Go tests; use the injected clocks (`sidebar.WithClock`,
  `sidebar.WithPageClock`, `clocktest`).
