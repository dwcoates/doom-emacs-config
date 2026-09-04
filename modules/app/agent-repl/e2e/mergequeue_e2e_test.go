// mergequeue_e2e_test.go — SPEC.md section C, "Merge queue, including
// displaced turns" (entries #39-43). Drives the daemon's merge orchestrator
// (docs/overhaul/daemon.md §"Merge (daemon-synthesized)" and §"Queue, holds,
// leases — contract facts") end to end: a real daemon, a real shim-store, a
// real shim-claude-sidecar, and the real TypeScript shim running its
// `--fake` offline vendor, composed via World exactly as production wires
// them.
//
// GIT IS THE SCRIPTED FAKE the daemon harness already installs (project-lead
// reversal, 2026-09-02): World never sets Opts.SkipFakeGit, so
// harness.NewRepo/harness.Register/(*Repo).ScriptConflict/(*Repo).SetDirty
// are exactly the primitives daemon/integration/merge_test.go itself uses.
// The two-parent --no-ff commit, the landed range, a conflicted index and
// MERGE_HEAD, revert, worktree prune, porcelain markers, GIT_DIR precedence
// and git-version compatibility are NOT this file's facts — they belong to
// the git-client leaf's own tests (daemon/AGENTS.md's "Coverage
// deliberately not attainable" section). This file asserts merge-QUEUE
// orchestration and the displaced-turn contract against the daemon's own
// fixture git world, driven through the real shim rather than the fake one
// daemon/integration uses, so every turn a merge starts (a conflict brief,
// a resubmitted displaced turn) runs the real shim's default scenario
// instead of a hand-pushed fake-shim frame.
//
// Every unexported identifier this file introduces is prefixed `mq` so it
// cannot collide with another area file's helper of the same shape (all 20
// area files compile into one package).
//
// TWO PRODUCT WARNINGS SEEN ONCE UNDER UNCAPPED `-parallel`, MEASURED AND NOT
// REPRODUCED (2026-09-04):
//   - `daemon.feed.response_fragment_after_settle`, from
//     TestMergeParkedRecognizedFromLeaseState.
//   - an OpenFeed resolve-workspace "no such file or directory", from
//     TestMergeBubbleCoalescesIntoOneFeedRow/self-repo.
//
// Both were observed in a single whole-suite run left uncapped on `-parallel`
// (one world per test, far more concurrent daemons than cores). The
// measurement: these two were re-run at `-count=10` under `-parallel 32` and
// `-parallel 64`, nine times over, and NEITHER warning appeared in any of the
// 180 executions. One run of TestMergeParkedRecognizedFromLeaseState DID fail
// in that campaign, on a host carrying an unrelated load average near ten, and
// did not recur in the 80 executions that followed.
//
// So the fault is real but LOAD-DEPENDENT, not deterministic in these two
// tests, and the source is the daemon rather than the arrangement here: a
// response fragment resolved after its turn settled, and a feed opened against
// a workspace path already gone, are both daemon faults whenever they happen.
// This note is the evidence for whoever sees either again — it is a warning to
// root-cause in the daemon, never a flake to re-run past.
package e2e

import (
	"os"
	"path/filepath"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/integration/harness"

	"connectrpc.com/connect"
)

// ---------------------------------------------------------------------------
// Shared fixtures and helpers. Mirrors daemon/integration/merge_test.go's
// own mergeRepositoryRef/mergeCreateChild/mergeBranchOf, adapted to World
// (real store/sidecar/real shim) instead of harness.Daemon's fake shim: a
// child's turns run the real shim's default scenario rather than a
// hand-pushed ShimControl frame.
// ---------------------------------------------------------------------------

// mqCleanRepo mints a fresh fake repository with a passing bin/test-all.sh,
// so a merge with no scripted conflict lands cleanly through the tests tab.
func mqCleanRepo(t *testing.T) (*harness.Repo, *harness.Recorder) {
	t.Helper()
	repo := harness.NewRepo(t)
	script := harness.NewTestAllScript(t, repo.Dir)
	script.SetExitCode(0)
	script.SetStdout("mergequeue e2e: passed in 1s\n")
	return repo, script
}

// mqRepositoryRef registers a repository's main worktree and reads back the
// daemon-minted RepositoryRef from the roster.
func mqRepositoryRef(t *testing.T, w *World, repo *harness.Repo) *workspacev1.RepositoryRef {
	t.Helper()
	harness.Register(t, w.Daemon, repo.Dir)
	roster := w.WatchRoster()
	got := harness.AwaitView(t, w.Ctx(), roster, "the repository's roster section", func(r *frontendv1.WorkspaceRoster) bool {
		return mqFindRepoKey(r, repo.Dir) != nil
	})
	ref := mqFindRepoKey(got, repo.Dir)
	if ref == nil {
		t.Fatalf("no roster repository section for %s", repo.Dir)
	}
	return ref
}

// mqFindRepoKey finds a repository section by its worktree dir. A
// repository is keyed by its COMMON DIR, which for an ordinary checkout is
// `<worktree>/.git`, so a worktree's section is found under either spelling.
func mqFindRepoKey(r *frontendv1.WorkspaceRoster, dir string) *workspacev1.RepositoryRef {
	for _, s := range r.GetRepository().GetSections() {
		switch s.GetKey().GetRepository().GetDir() {
		case dir, filepath.Join(dir, ".git"):
			return s.GetKey().GetRepository()
		}
	}
	return nil
}

// mqCreateTopLevelChild creates a top-level child workspace (no parent, so
// its merge target is the repository's main worktree and default branch)
// with NO initial prompt: CreateWorkspace opens the workspace regardless
// (daemon.md invariant 11, "a created workspace is an opened one"), and an
// empty initial conversation lets each test drive its own turns explicitly
// through SubmitPrompt/AwaitTurnEnded rather than racing a prompt the
// fixture itself queued.
func mqCreateTopLevelChild(t *testing.T, w *World, repoRef *workspacev1.RepositoryRef, name string) *workspacev1.WorkspaceRef {
	t.Helper()
	resp, err := w.Client().CreateWorkspace(w.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository: repoRef,
		Form: &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{
			Name: mqStrPtr(name),
		}},
	}))
	if err != nil {
		t.Fatalf("CreateWorkspace(%s) = error %v, want a success", name, err)
	}
	ws := resp.Msg.GetSuccess().GetWorkspace()
	if ws.GetId() == "" {
		t.Fatalf("CreateWorkspace(%s) = %v, want a success carrying a workspace ref", name, resp.Msg)
	}
	w.WatchWorkspaceLogs(ws.GetDir())
	return ws
}

// mqBranchOf is the branch a mqCreateTopLevelChild workspace checked out —
// its directory's basename, since Name was passed as the branch name (the
// same convention daemon/integration/merge_test.go's mergeBranchOf relies
// on).
func mqBranchOf(ws *workspacev1.WorkspaceRef) string {
	return filepath.Base(ws.GetDir())
}

func mqStrPtr(s string) *string { return &s }

// mqRosterRow finds a workspace's own row anywhere in the repository
// grouping (top-level or nested under a parent), by workspace id.
func mqRosterRow(r *frontendv1.WorkspaceRoster, id string) *frontendv1.RosterRow {
	for _, s := range r.GetRepository().GetSections() {
		if row := mqFindRosterRow(s.GetRows().GetRows(), id); row != nil {
			return row
		}
	}
	return nil
}

func mqFindRosterRow(rows []*frontendv1.RosterRow, id string) *frontendv1.RosterRow {
	for _, row := range rows {
		if row.GetWorkspace().GetWorkspace().GetId() == id {
			return row
		}
		if found := mqFindRosterRow(row.GetChildren(), id); found != nil {
			return found
		}
	}
	return nil
}

// mqOpenFeedRows opens a feed (root when feed is nil, a sub-feed otherwise)
// and answers its current page's rows plus the token to tail it further.
func mqOpenFeedRows(t *testing.T, w *World, ws *workspacev1.WorkspaceRef, feed *frontendv1.FeedId) ([]*frontendv1.FeedRow, *agentreplv1.FeedWatchToken) {
	t.Helper()
	resp, err := w.Client().OpenFeed(w.Ctx(), connect.NewRequest(&agentreplv1.OpenFeedRequest{Workspace: ws, Feed: feed}))
	if err != nil {
		t.Fatalf("OpenFeed: %v", err)
	}
	success := resp.Msg.GetSuccess()
	if success == nil {
		t.Fatalf("OpenFeed = %v, want success", resp.Msg)
	}
	return success.GetPage().GetSuccess().GetRows(), success.GetWatch()
}

// mqFeedRowText answers the plain text of a FeedUserPrompt row's first text
// block, for matching a resubmitted turn by the user's own words.
func mqFeedRowText(row *frontendv1.FeedRow) string {
	for _, b := range row.GetUserPrompt().GetSuccess().GetBody().GetBlocks() {
		if t := b.GetText(); t != nil {
			return t.GetText()
		}
	}
	return ""
}

// mqFeedWatch is a root (or sub-) feed opened ONCE and tailed from that
// point, with every row it has ever delivered (the initial page, plus
// everything drained from the tail so far) retained in order.
//
// A ROOT feed MUST be opened before the action that will make it worth
// watching (e.g. before MergeWorkspace is called), never after: a landed
// merge tears its child workspace's worktree down as part of releasing the
// lease, and the root feed resolver serves a workspace's page by stat-ing
// that same worktree — a fresh OpenFeed(root) made after landing can find no
// such workspace to watch. Re-opening a SUB-feed (by its own FeedId) after
// landing is safe; only the ROOT feed carries this hazard. This mirrors
// daemon/integration/merge_test.go's own documented rationale for opening
// its root feed before every merge it enqueues.
type mqFeedWatch struct {
	t      *testing.T
	w      *World
	ws     *workspacev1.WorkspaceRef
	stream *harness.Stream[*frontendv1.FeedRow]
	seen   []*frontendv1.FeedRow
}

// mqOpenFeedWatch opens a feed (root when feed is nil) and begins tailing it,
// recording the page's own rows into Rows() immediately.
func mqOpenFeedWatch(t *testing.T, w *World, ws *workspacev1.WorkspaceRef, feed *frontendv1.FeedId) *mqFeedWatch {
	t.Helper()
	rows, token := mqOpenFeedRows(t, w, ws, feed)
	fw := &mqFeedWatch{t: t, w: w, ws: ws, stream: w.WatchFeed(token)}
	fw.seen = append(fw.seen, rows...)
	return fw
}

// Close ends the underlying stream.
func (fw *mqFeedWatch) Close() { fw.stream.Close() }

// Rows answers every row seen so far (the opening page, plus everything
// drained from the tail across every AwaitRow call made on this watch).
func (fw *mqFeedWatch) Rows() []*frontendv1.FeedRow { return fw.seen }

// AwaitRow answers a row satisfying pred, checking what has already been
// seen first, then draining the tail (recording every row drained, matching
// or not) until one does.
func (fw *mqFeedWatch) AwaitRow(what string, pred func(*frontendv1.FeedRow) bool) *frontendv1.FeedRow {
	fw.t.Helper()
	for _, row := range fw.seen {
		if pred(row) {
			return row
		}
	}
	for {
		row := harness.AwaitNext(fw.t, fw.w.Ctx(), fw.stream, what)
		fw.seen = append(fw.seen, row)
		if pred(row) {
			return row
		}
	}
}

// ---------------------------------------------------------------------------
// #39 — MergeLeaseRefusesSubmit. daemon.md §"Queue, holds, leases — contract
// facts": "A prompt arriving after a merge began is refused (never held)."
// The merge lease's error-on-submit policy is exercised while the daemon's
// own conflict-repair turn is genuinely in flight (StartTurn recorded, no
// terminal yet) — a window with real, measurable wall-clock width because
// the repair turn is answered by the REAL shim's default scenario, not a
// synchronous fake-shim push. This is deliberately NOT the parked case
// (#41): parked guidance is explicitly NOT refused, so this test's window
// is bounded to "turn open, not yet parked."
// ---------------------------------------------------------------------------

func TestMergeLeaseRefusesSubmit(t *testing.T) {
	t.Parallel()
	// Arrange: a workspace whose merge will hit a scripted conflict, so the
	// daemon starts a real conflict-repair turn on it.
	repo, _ := mqCleanRepo(t)
	w := NewWorld(t, WorldOpts{DaemonOpts: harness.Opts{SelfRepo: repo.Dir}})
	// The scripted conflict makes the no-fast-forward merge fail and opens
	// the merge tab; the refused submit is this test's own subject.
	w.ExpectWarnings("daemon.gitclient.merge_no_ff", "daemon.merge.merge_tab", "daemon.promptqueue.submit")
	repoRef := mqRepositoryRef(t, w, repo)
	child := mqCreateTopLevelChild(t, w, repoRef, "mq-lease-refuse")
	repo.ScriptConflict(repo.Dir, mqBranchOf(child), "conflict.txt")

	// Act: enqueue the merge (admits immediately — an empty queue) and wait
	// for the conflict-repair turn to be genuinely open.
	if _, err := w.Client().MergeWorkspace(w.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: child})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}
	w.AwaitWorkspaceLogOperationCount(child.GetDir(), harness.OpTurnOpened, 1)

	// Act: submit a fresh prompt to the SAME workspace while that turn is
	// still open (not yet parked).
	resp, err := w.Client().SubmitPrompt(w.Ctx(), connect.NewRequest(&agentreplv1.SubmitPromptRequest{
		Workspace:      child,
		Said:           mqSaid("please stop and do something else"),
		IdempotencyKey: newIdempotencyKey(t),
		Origin:         e2ePromptOrigin,
	}))

	// Assert: refused outright, never held.
	if err != nil {
		t.Fatalf("SubmitPrompt while a merge is in flight = transport error %v, want a SubmitPromptError.merging arm", err)
	}
	if resp.Msg.GetError().GetMerging() == nil {
		t.Fatalf("SubmitPrompt while a merge is in flight = %v, want SubmitPromptError.merging", resp.Msg)
	}
	if resp.Msg.GetSuccess() != nil {
		t.Fatalf("SubmitPrompt while a merge is in flight = %v, want no success (never held)", resp.Msg)
	}
}

// mqSaid builds the plain-text UserSaid every raw SubmitPromptRequest in
// this file sends, mirroring world_test.go's SubmitPrompt helper's own
// construction (that helper cannot be reused directly by the tests in this
// file that must inspect the raw, possibly-refused response instead of
// failing the test on one).
func mqSaid(text string) *conversationv1.UserSaid {
	return &conversationv1.UserSaid{Content: &conversationv1.UserContent{Blocks: []*conversationv1.UserContentBlock{
		{Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: text}}},
	}}}
}

// ---------------------------------------------------------------------------
// #40 — MergeBubbleCoalescesIntoOneFeedRow. daemon.md §"Merge
// (daemon-synthesized)": "the daemon coalesces everything produced during a
// merge into one feed bubble... a sub-feed (own FeedId, OpenFeed/WatchFeed —
// the same plumbing as subagent bubbles)." Driven for BOTH of the two
// methods daemon.md keys on self-repo-or-not: the daemon's own checkout
// (harness.Opts.SelfRepo) and an ordinary repository. In both cases the
// child workspace's ROOT feed carries exactly ONE row for the whole merge
// (the FeedMerge head); the phase detail (queue/merge/tests/... tabs) lives
// only on that row's OWN sub-feed, never as separate root-feed rows.
// ---------------------------------------------------------------------------

func TestMergeBubbleCoalescesIntoOneFeedRow(t *testing.T) {
	t.Parallel()
	for _, tc := range []struct {
		name     string
		selfRepo bool
	}{
		{name: "self-repo method", selfRepo: true},
		{name: "non-self-repo method", selfRepo: false},
	} {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			repo, _ := mqCleanRepo(t)
			opts := harness.Opts{}
			if tc.selfRepo {
				opts.SelfRepo = repo.Dir
				// The self-repo method runs the merge TEST GATE, whose
				// command line defaults to `bash bin/test-all.sh` in the
				// merge target worktree (daemon/AGENTS.md's
				// AGENT_REPL_TEST_ALL_SCRIPT row; resolved by
				// merge.TestCommandFor). This suite's repos are scripted
				// fixtures with no bin/test-all.sh, so the gate exited 127
				// and the merge could never reach its landed terminal.
				// Provide a passing gate script exactly as
				// daemon/integration/merge_test.go does.
				script := harness.NewTestAllScript(t, repo.Dir)
				script.SetExitCode(0)
				script.SetStdout("e2e: passed in 1s\n")
				opts.ExtraEnv = []string{"AGENT_REPL_TEST_ALL_SCRIPT=" + script.Path}
			}
			w := NewWorld(t, WorldOpts{DaemonOpts: opts})
			repoRef := mqRepositoryRef(t, w, repo)
			child := mqCreateTopLevelChild(t, w, repoRef, "mq-coalesce")

			// The root feed is opened and tailed BEFORE the merge is
			// enqueued: a landed merge tears the child workspace's worktree
			// down releasing the lease, and a root-feed open made afterward
			// can race that teardown and find no such workspace to watch
			// (see mqFeedWatch's own doc comment).
			root := mqOpenFeedWatch(t, w, child, nil)
			defer root.Close()

			// Act: a clean merge (no conflict, a passing test gate) lands.
			if _, err := w.Client().MergeWorkspace(w.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: child})); err != nil {
				t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
			}
			mergeRow := root.AwaitRow("the merge bubble's landed terminal", func(row *frontendv1.FeedRow) bool {
				return row.GetActivity().GetMerge().GetSuccess() != nil
			})

			// Assert: exactly ONE distinct root-feed row (by FeedId) ever
			// carried merge activity for this workspace's whole merge —
			// the bubble's own row is UPSERTED in place as it progresses
			// (feed.proto: "a later push with the same id replaces this row
			// whole"), never appended as a second root row.
			mergeFeedIDs := map[string]bool{}
			for _, row := range root.Rows() {
				if row.GetActivity().GetMerge() != nil {
					mergeFeedIDs[row.GetId().GetValue()] = true
				}
			}
			if len(mergeFeedIDs) != 1 {
				t.Fatalf("root feed carries %d DISTINCT merge-activity row ids, want exactly 1 (one coalesced bubble)", len(mergeFeedIDs))
			}

			// Assert: the phase detail lives on the bubble's OWN sub-feed
			// (own FeedId), never on the root feed as separate rows. Opening
			// a SUB-feed after landing is safe (only the root feed carries
			// the teardown hazard above).
			subRows, _ := mqOpenFeedRows(t, w, child, mergeRow.GetId())
			foundTab := false
			for _, row := range subRows {
				if row.GetMergeTab() != nil {
					foundTab = true
				}
				if row.GetActivity().GetMerge() != nil {
					t.Fatalf("the merge bubble's own sub-feed re-carries a FeedMerge head row: %v, want the head confined to root", row)
				}
			}
			if !foundTab {
				t.Fatalf("the merge bubble's sub-feed carried no FeedMergeTab row, want at least one (e.g. the merge or queue tab)")
			}
		})
	}
}

// ---------------------------------------------------------------------------
// #41 — MergeParkedRecognizedFromLeaseState. daemon.md §"Merge
// (daemon-synthesized)": "PARKED is recognized purely from lease state (no
// content classifier) and the conversational parked flow is the only resume
// path — no hand-resolution verb exists." Mirrors
// daemon/integration/merge_test.go's
// TestSubmitPromptWhileMergeParkedLandsInTheConflictsTabNotAsARefusal,
// adapted to the real shim: the conflict brief's own turn is answered by the
// real fake SDK's default scenario (no bang prefix), and its conclusion
// without resolving the scripted conflict is what parks the merge.
// ---------------------------------------------------------------------------

func TestMergeParkedRecognizedFromLeaseState(t *testing.T) {
	t.Parallel()
	// Arrange: park a merge on a scripted conflict.
	repo, _ := mqCleanRepo(t)
	w := NewWorld(t, WorldOpts{DaemonOpts: harness.Opts{SelfRepo: repo.Dir}})
	// The scripted conflict this test parks on: the no-fast-forward failure,
	// the merge tab it opens, and the conflicts record itself.
	w.ExpectWarnings("daemon.gitclient.merge_no_ff", "daemon.merge.merge_tab", "daemon.merge.conflicts")
	repoRef := mqRepositoryRef(t, w, repo)
	child := mqCreateTopLevelChild(t, w, repoRef, "mq-parked")
	repo.ScriptConflict(repo.Dir, mqBranchOf(child), "conflict.txt")
	if _, err := w.Client().MergeWorkspace(w.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: child})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}

	// The conflict brief's own turn concludes (the real shim's default
	// scenario), and — since nothing in this suite's harness surface can
	// clear a scripted conflict — the run PARKS rather than landing.
	footer := w.WatchFooter(child)
	defer footer.Close()
	fv := harness.AwaitView(t, w.Ctx(), footer.Stream, "the footer's parked substatus", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetMerging().GetParked() != nil
	})
	if fv.GetStrip().GetStatus().GetMerging().GetParked().GetLine() == "" {
		t.Fatalf("footer parked = %v, want a composed line", fv.GetStrip().GetStatus().GetMerging().GetParked())
	}
	host := w.WatchHost(child)
	hv := harness.AwaitView(t, w.Ctx(), host, "the host composer parked on the merge", func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
		return r.GetHost().GetExisting().GetLive().GetMergeParked() != nil
	})
	if hv.GetHost().GetExisting().GetLive().GetMergeParked() == nil {
		t.Fatalf("host composer = %v, want merge_parked", hv.GetHost())
	}
	roster := w.WatchRoster()
	rgot := harness.AwaitView(t, w.Ctx(), roster, "the roster's merge_conflict arm", func(r *frontendv1.WorkspaceRoster) bool {
		return mqRosterRow(r, child.GetId()).GetMergeConflict() != nil
	})
	if row := mqRosterRow(rgot, child.GetId()); row.GetMergeConflict() == nil {
		t.Fatalf("parked workspace roster status = %v, want merge_conflict", row)
	}

	// Act: submit conversational guidance to the SAME workspace while
	// parked — the ONLY resume path the contract names; there is no
	// dedicated hand-resolution verb.
	resp, err := w.Client().SubmitPrompt(w.Ctx(), connect.NewRequest(&agentreplv1.SubmitPromptRequest{
		Workspace:      child,
		Said:           mqSaid("please look again"),
		IdempotencyKey: newIdempotencyKey(t),
		Origin:         e2ePromptOrigin,
	}))

	// Assert: NOT refused — delivered as a real turn, never the merging
	// refusal #39 pins for the pre-parked window.
	if err != nil {
		t.Fatalf("SubmitPrompt while merge-parked = transport error %v, want it delivered, not refused", err)
	}
	if resp.Msg.GetError() != nil {
		t.Fatalf("SubmitPrompt while merge-parked = %v, want no refusal (the conversational parked flow is the only resume path)", resp.Msg)
	}
	if resp.Msg.GetSuccess().GetTurn().GetTurn().GetValue() == "" {
		t.Fatalf("SubmitPrompt while merge-parked = %v, want a minted turn", resp.Msg)
	}
}

// ---------------------------------------------------------------------------
// #42 — DisplacedTurnCapturedEndedThenResubmittedExactlyOnce. The most
// recently settled contract point in this doc set (commit 8ad7e279c,
// daemon.md §"...GIVE-UP RULES..."): "the displaced user turn is captured
// durably, then ENDED (KillTurn) once the capture is durable, then
// resubmitted exactly once at lease release, across a daemon bounce."
//
// A LANDED merge tears its workspace down (releasing the lease by closing
// the workspace), which cannot be the shape this test drives: there would be
// no session left to resubmit onto. daemon/integration/merge_test.go's own
// working test for this exact sentence
// (TestADisplacedUserTurnIsResubmittedExactlyOnceAcrossADaemonBounce) never
// lets its merge land either — it interrupts the merge with a daemon CRASH,
// then makes the target dirty so the restart REFUSES to resume it (an
// "abandoned", not "landed", release), and it is the boot-time recovery
// sweep (internal/merge/recover.go's recoverDisplaced, daemon.md's own
// citation) that performs the resubmit. That test lands its crash inside the
// narrow window between capture and the merge's own next step using an
// internal-only test hook (AGENT_REPL_MERGE_PAUSE_AFTER_CAPTURE /
// merge.Deps.PauseAfterCapture) that is not named in any of the six contract
// docs, PROTO-CHANGES.md, or the protos themselves — reproducing that exact
// hook is the daemon's own integration suite's job, not this cross-system
// suite's, per this task's own instruction to write to the contract and not
// to production internals discovered by reading source.
//
// This test reaches the SAME abandoned-release shape without that hook, by
// scripting a CONFLICT: the merge's own conflict-repair turn runs through
// the REAL shim (a genuine process round trip, wide compared to this test's
// own local RPCs), so crashing the daemon immediately after observing the
// DISPLACED turn's own end — a real, wire-visible, event-driven signal, not
// an internal rendezvous file — lands comfortably before the merge can reach
// its own terminal (park or land) on its own. The crashed daemon's restart,
// finding the target dirty, abandons the interrupted merge exactly as
// daemon/integration's own test relies on, and its boot recovery sweep
// resubmits the displaced turn.
// ---------------------------------------------------------------------------

func TestDisplacedTurnCapturedEndedThenResubmittedExactlyOnce(t *testing.T) {
	t.Parallel()
	const displacedText = "keep going"
	mqExpectedBounceWarnings := []string{
		"daemon.merge.conflicts", "daemon.gitclient.merge_no_ff", "daemon.merge.merge_tab",
		"daemon.rollout.reconcile", "daemon.merge.recover", "daemon.promptqueue.restore_holds",
	}

	// Arrange. The displaced turn is PARKED ON THE FAKE'S TURN GATE
	// (hibernation_e2e_test.go's turnGatePathEnv/turnGateTextEnv, documented
	// in agent-shim/claude/shim/src/fake/index.ts:110-114): a turn carrying
	// exactly the gate text does not begin emitting until the gate path
	// exists. Without it the real shim answers `keep going` in microseconds
	// and the turn is very likely already over by the time MergeWorkspace
	// admits — which is not a displacement at all.
	//
	// The gate stays SHUT for the whole displacement window — the turn must
	// still be in flight when the merge takes the workspace — and is opened
	// exactly once, below, after the boot sweep's resubmission has been
	// observed. It has to be opened there: the shim runs in its OWN PROCESS
	// GROUP (daemon/internal/shimclient/supervisor.go's Setpgid; "sessions
	// and turns OUTLIVE the daemon"), so the crash does NOT end it. The same
	// shim process, still carrying this gate's env from the first daemon, is
	// adopted by the second daemon and serves the resubmitted turn — whose
	// prompt is the displaced turn's OWN words and therefore matches the gate
	// text exactly. Gating on text unique to the original submission is not
	// available as an alternative: the resubmission is a verbatim replay of
	// that submission (this test's own `matches != 2` assertion depends on
	// it), so the only text that parks the original also parks the replay.
	// Opening the gate after the resubmission is observed keeps the
	// scenario's intent whole — captured, ended, then resubmitted exactly
	// once, and that resubmission runs to its own ordinary terminal.
	gatePath := filepath.Join(t.TempDir(), "displaced-turn-gate")
	repo, _ := mqCleanRepo(t)
	w := NewWorld(t, WorldOpts{DaemonOpts: harness.Opts{
		SelfRepo: repo.Dir,
		ExtraEnv: []string{
			turnGatePathEnv + "=" + gatePath,
			turnGateTextEnv + "=" + displacedText,
		},
	}})
	w.ExpectWarnings(mqExpectedBounceWarnings...)
	repoRef := mqRepositoryRef(t, w, repo)
	child := mqCreateTopLevelChild(t, w, repoRef, "mq-displaced")
	repo.ScriptConflict(repo.Dir, mqBranchOf(child), "conflict.txt")

	// The root feed is opened and tailed BEFORE the merge is enqueued (see
	// mqFeedWatch's own doc comment): this merge is abandoned by a crash
	// rather than landed, so the workspace in fact survives, but nothing
	// here depends on that — opening early is always the safe order.
	root := mqOpenFeedWatch(t, w, child, nil)
	defer root.Close()

	// Act: put a real turn in flight, then admit the merge on the SAME
	// workspace while it is open.
	turn := SubmitPrompt(t, w, child, displacedText)
	w.AwaitWorkspaceLogOperationCount(child.GetDir(), harness.OpTurnOpened, 1)
	if _, err := w.Client().MergeWorkspace(w.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: child})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}

	// Assert: the displaced turn is CAPTURED then ENDED — its own terminal
	// arrives on the feed.
	endedRow := root.AwaitRow("the displaced turn's own terminal", func(row *frontendv1.FeedRow) bool {
		return row.GetTurn().GetValue() == turn.GetValue() && row.GetTurnEnded() != nil
	})
	if endedRow.GetTurnEnded().GetInterrupted() == nil {
		t.Fatalf("the displaced turn's terminal = %v, want FeedTurnEndedInterrupted (KillTurn's feed-visible shape)", endedRow.GetTurnEnded())
	}

	// Act: crash the daemon RIGHT NOW — immediately after the ONE
	// event-driven signal that capture+end already happened, and before
	// this test does anything else that could let the merge's own
	// conflict-repair turn (which has not even started yet: it is a SECOND,
	// still-to-come StartTurn) begin or conclude. Make the target dirty so
	// the restart cannot resume the interrupted merge — the same real-git
	// fact daemon/integration/merge_test.go's own bounce test relies on
	// (repo.SetDirty is USABLE against this suite's scripted fake git).
	repo.SetDirty(repo.Dir, true)
	w.Kill()

	// Act: a second real claude-repld boots on the SAME state root, store
	// socket and lock dir.
	d2 := harness.StartDaemon(t, harness.Opts{
		StateDir:    w.StateDir,
		SelfRepo:    repo.Dir,
		ShimNode:    requireNode(t),
		ShimMain:    requireShimBundle(t),
		StoreSocket: w.Store.Socket,
		ExtraEnv:    append([]string{"AGENT_REPL_LOCK_DIR=" + w.LockDir}, buildIdentityEnv()...),
	})
	d2.ExpectWarnings(mqExpectedBounceWarnings...)
	// Re-point World at the new process: every w.Client()/w.WatchFeed/... call
	// from here on reaches d2, exactly as
	// daemon/integration/merge_test.go's own displacedTurnAcrossABounce
	// rebinds its fixture's `d` field after the bounce.
	w.Daemon = d2
	d2.AwaitRunLogOperation("daemon.merge.recover")

	// Assert: the boot sweep resubmitted the displaced turn EXACTLY ONCE,
	// carrying its own words, on a turn id of its own. The merge was
	// abandoned (never landed), so the workspace survived and a fresh
	// OpenFeed is safe.
	rootAfterBoot, _ := mqOpenFeedRows(t, w, child, nil)
	var resubmitTurn *conversationv1.TurnId
	matches := 0
	for _, row := range rootAfterBoot {
		if row.GetUserPrompt() != nil && mqFeedRowText(row) == displacedText {
			matches++
			if row.GetTurn().GetValue() != turn.GetValue() {
				resubmitTurn = row.GetTurn()
			}
		}
	}
	if matches != 2 { // the original submission, plus exactly one resubmission
		t.Fatalf("root feed carries %d rows with the displaced turn's own words, want exactly 2 (the original and one resubmission)", matches)
	}
	if resubmitTurn == nil {
		t.Fatalf("no resubmitted turn found carrying the displaced turn's own words on a turn id of its own")
	}

	// Act: the resubmission is now observed, so OPEN THE GATE. The adopted
	// shim parked this replay on the very gate that held the original (see
	// the arrangement above); releasing it lets the resubmitted turn end the
	// ORDINARY way, which is what the assertion below is about.
	if err := os.WriteFile(gatePath, nil, 0o644); err != nil {
		t.Fatalf("opening the turn gate for the resubmitted turn = error %v, want the gate created", err)
	}
	AwaitTurnEnded(t, w, child, resubmitTurn)

	// Assert: the mark is DOWN. A record still marked is one the next boot
	// would put back a second time.
	if n := w.DisplacedTurnCount(); n != 0 {
		t.Fatalf("turns still marked displaced after the resubmission = %d, want none", n)
	}

	// Assert: a SECOND bounce does not resubmit again — the claim that put
	// the turn back is durable, so the boot after it finds nothing owed.
	w.Kill()
	d3 := harness.StartDaemon(t, harness.Opts{
		StateDir:    w.StateDir,
		SelfRepo:    repo.Dir,
		ShimNode:    requireNode(t),
		ShimMain:    requireShimBundle(t),
		StoreSocket: w.Store.Socket,
		ExtraEnv:    append([]string{"AGENT_REPL_LOCK_DIR=" + w.LockDir}, buildIdentityEnv()...),
	})
	d3.ExpectWarnings(mqExpectedBounceWarnings...)
	w.Daemon = d3
	if n := d3.DisplacedTurnCount(); n != 0 {
		t.Fatalf("turns marked displaced after a second bounce on a settled state root = %d, want none (nothing was owed)", n)
	}
	// THE FEED HAS NO SHIM-LESS READ PATH. This third daemon adopted nothing
	// ("daemon.boot.adopt: no shim survives for this workspace" — the shim
	// that served the resubmission exited with its session), and daemon.md's
	// SPAWN ON MOUNT ruling is explicit: "mounting a parked workspace's
	// frontend IS an implicit revival — the shim spawns and ReadHistory
	// serves; there is no shim-less read path (the store isolation gate
	// stands absolute)", which workspace/open.go's Open implements. So a bare
	// OpenFeed here answers an empty page by contract, not by defect; the
	// mount is what makes the transcript replay, and the mount is exactly
	// what a frontend coming up against this daemon would do.
	if _, err := w.Client().OpenWorkspace(w.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: child})); err != nil {
		t.Fatalf("OpenWorkspace after the second bounce = error %v, want the parked workspace revived", err)
	}

	// Assert: the replay carries the displaced turn's words on exactly the
	// same number of rows as before — no fewer (the wait below only ends when
	// all of them have arrived) and no more (the probe window would see a
	// third). Counting rather than matching turn ids is deliberate: the words
	// are what "resubmitted twice" would duplicate.
	afterSecondBoot := mqOpenFeedWatch(t, w, child, nil)
	defer afterSecondBoot.Close()
	mqDisplacedRows := func(rows []*frontendv1.FeedRow) int {
		n := 0
		for _, row := range rows {
			if row.GetUserPrompt() != nil && mqFeedRowText(row) == displacedText {
				n++
			}
		}
		return n
	}
	afterSecondBoot.AwaitRow("the replayed rows carrying the displaced turn's words", func(*frontendv1.FeedRow) bool {
		return mqDisplacedRows(afterSecondBoot.Rows()) >= matches
	})
	if got := mqDisplacedRows(afterSecondBoot.Rows()); got != matches {
		t.Fatalf("root feed rows carrying the displaced turn's words after a second bounce = %d, want unchanged at %d (never resubmitted twice)", got, matches)
	}

	// A resubmission the boot sweep made would arrive as one more such row.
	// Nothing else can end this wait, so it necessarily waits out the probe.
	deadline := time.NewTimer(harness.ProbeWindow)
	defer deadline.Stop()
	for done := false; !done; {
		select {
		case row, ok := <-afterSecondBoot.stream.C:
			if !ok {
				done = true
				break
			}
			if row.GetUserPrompt() != nil && mqFeedRowText(row) == displacedText {
				t.Fatalf("a further root feed row carries the displaced turn's words after a second bounce, want unchanged at %d (never resubmitted twice)", matches)
			}
		case <-deadline.C:
			done = true
		}
	}
}

// ---------------------------------------------------------------------------
// #43 — FanWideCancel. E2E-EVENT-INVENTORY.md coverage-report item 6: a
// genuinely fan-wide (multi-agent) cancel, distinct from a one-at-a-time
// cancel. Ruling (SPEC.md §F, item 1): the shim's agent-spool EXIT=
// terminator defect this scenario once exposed is CLOSED — pinned by
// agent-shim/claude/shim/test/fake/scenarios/subagents.test.ts:299 ("writes
// NO EXIT line into a stopped AGENT's spool") — so this is a normal green
// test, not one written expecting failure.
//
// Driven via the `cancel-all` scenario (agent-shim/claude/shim/src/fake/
// scenarios/subagents.ts: prompt "!cancel-all"), which launches three
// detached items (two background agents, one background shell) and leaves
// all three LIVE — the scenario deliberately stops there, because the
// cancel itself is the caller's own stop, exactly what
// agentrepl.v1.Interrupt's `all_agents` target is: "EVERY live detached
// agent at once — the fan-wide stop" (endpoint_interrupt.proto). This is
// the file's one entry with no merge involved at all — filed here only
// because SPEC.md's fanout table assigns #39-43 to this writer.
// ---------------------------------------------------------------------------

func TestFanWideCancel(t *testing.T) {
	t.Parallel()
	// Arrange: a plain registered, opened workspace (no repository/merge
	// machinery needed for this scenario).
	repo := harness.NewRepo(t)
	w := NewWorld(t, WorldOpts{})
	ws := harness.Register(t, w.Daemon, repo.Dir)
	w.WatchWorkspaceLogs(repo.Dir)
	if _, err := w.Client().OpenWorkspace(w.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: ws})); err != nil {
		t.Fatalf("OpenWorkspace = error %v, want a success", err)
	}

	// Act: drive the scenario's own setup turn to completion — it launches
	// three detached items and leaves them live, then concludes normally.
	turn := SubmitPrompt(t, w, ws, "!cancel-all")
	AwaitTurnEnded(t, w, ws, turn)

	// Act: the fan-wide stop — one Interrupt call, target all_agents.
	resp, err := w.Client().Interrupt(w.Ctx(), connect.NewRequest(&agentreplv1.InterruptRequest{
		Workspace: ws,
		Target:    &agentreplv1.InterruptRequest_AllAgents{AllAgents: &agentreplv1.InterruptAllAgents{}},
	}))

	// Assert: it reached every live item in one call — not
	// detachedcancel_e2e_test.go's one-at-a-time cancel.
	if err != nil {
		t.Fatalf("Interrupt(all_agents) = error %v, want a success", err)
	}
	interrupted := resp.Msg.GetSuccess().GetInterruptedDetached()
	if interrupted == nil {
		t.Fatalf("Interrupt(all_agents) = %v, want InterruptedDetached", resp.Msg)
	}
	if interrupted.GetCount() != 3 {
		t.Fatalf("Interrupt(all_agents) stopped %d items, want 3 (two agents and a shell)", interrupted.GetCount())
	}

	// Assert: the live set is now genuinely empty — a second fan-wide stop
	// finds nothing running, proving the first one was truly fan-wide rather
	// than leaving a straggler behind.
	resp2, err := w.Client().Interrupt(w.Ctx(), connect.NewRequest(&agentreplv1.InterruptRequest{
		Workspace: ws,
		Target:    &agentreplv1.InterruptRequest_AllAgents{AllAgents: &agentreplv1.InterruptAllAgents{}},
	}))
	if err != nil {
		t.Fatalf("second Interrupt(all_agents) = error %v, want a success", err)
	}
	if resp2.Msg.GetSuccess().GetNothingRunning() == nil {
		t.Fatalf("second Interrupt(all_agents) = %v, want nothing_running (the fan-wide stop emptied the whole live set)", resp2.Msg)
	}
}
