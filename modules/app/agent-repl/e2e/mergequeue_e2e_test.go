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
// TWO RECORDS SEEN ONCE UNDER UNCAPPED `-parallel`, MEASURED AND NOT
// REPRODUCED (2026-09-04):
//   - `daemon.feed.response_fragment_after_settle`, from
//     TestMergeParkedRecognizedFromLeaseState. SETTLED SINCE, AND NOT A
//     FAULT: one block's frames reach the fold from two store planes that
//     share an upsert key and are not ordered against one another, so the
//     sidecar's settled `success` routinely lands between the shim's `start`
//     and its own trailing deltas. A settled frame restates the WHOLE, so the
//     dropped delta was already on screen. The daemon records it at DEBUG now
//     (daemon/internal/resolve/feed/response.go), and nothing here should
//     chase it.
//   - an OpenFeed resolve-workspace "no such file or directory", from
//     TestMergeBubbleCoalescesIntoOneFeedRow/self-repo. STILL A FAULT: a feed
//     opened against a workspace path already gone is a daemon fault whenever
//     it happens.
//
// Both were observed in a single whole-suite run left uncapped on `-parallel`
// (one world per test, far more concurrent daemons than cores). The
// measurement: these two were re-run at `-count=10` under `-parallel 32` and
// `-parallel 64`, nine times over, and NEITHER appeared in any of the 180
// executions. One run of TestMergeParkedRecognizedFromLeaseState DID fail in
// that campaign, on a host carrying an unrelated load average near ten, and
// did not recur in the 80 executions that followed.
//
// So the SECOND is real but LOAD-DEPENDENT, and its source is the daemon
// rather than the arrangement here. This note is the evidence for whoever
// sees it again — a warning to root-cause in the daemon, never a flake to
// re-run past.
package e2e

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
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
	harness.CommitWork(t, child.GetDir())
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
			harness.CommitWork(t, child.GetDir())
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
	harness.CommitWork(t, child.GetDir())
	if _, err := w.Client().MergeWorkspace(w.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: child})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}

	// The conflict brief's own turn concludes (the real shim's default
	// scenario), and — since nothing in this suite's harness surface can
	// clear a scripted conflict — the run PARKS rather than landing.
	footer := w.WatchFooter(child)
	defer footer.Close()
	fv := harness.AwaitView(t, w.Ctx(), footer.Stream, "the footer's parked substatus", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetMergeConflict().GetParked() != nil
	})
	if fv.GetStrip().GetStatus().GetMergeConflict().GetParked().GetLine() == "" {
		t.Fatalf("footer parked = %v, want a composed line", fv.GetStrip().GetStatus().GetMergeConflict().GetParked())
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
// lets its merge land either — it crashes the daemon inside the window
// between the capture and everything that would close it, then makes the
// target dirty so the restart REFUSES to resume the merge (an "abandoned",
// not "landed", release), and it is the boot-time recovery sweep
// (internal/merge/recover.go's recoverDisplaced) that performs the resubmit.
//
// THE CRASH LANDS IN A HELD WINDOW, NEVER A RACED ONE. claude-repld's own
// test seam, AGENT_REPL_MERGE_PAUSE_AFTER_CAPTURE, holds a merge run right
// after it captured and ended the displaced turn, and logs
// `daemon.merge.capture_pause` once it is held. This test used to race the
// crash against the merge's own next step instead — a scripted conflict whose
// repair turn it expected to start "comfortably" after the test's reaction —
// which is a bet on the host's speed, not a guarantee. Held, the merge never
// reaches git at all, so no conflict is scripted.
// ---------------------------------------------------------------------------

func TestDisplacedTurnCapturedEndedThenResubmittedExactlyOnce(t *testing.T) {
	t.Parallel()
	const displacedText = "keep going"
	// A daemon killed mid-merge writes no stand-down manifest
	// (daemon.rollout.reconcile), the restart refuses to resume the merge into
	// an unclean target (daemon.merge.recover), and a turn left in flight by a
	// killed daemon is closed by the next boot (daemon.promptqueue.restore_holds).
	mqExpectedBounceWarnings := []string{
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
			// Only the incumbent holds its merge; the daemons booted after the
			// crash recover it rather than run it.
			"AGENT_REPL_MERGE_PAUSE_AFTER_CAPTURE=" + filepath.Join(t.TempDir(), "capture.rendezvous"),
		},
	}})
	w.ExpectWarnings(mqExpectedBounceWarnings...)
	repoRef := mqRepositoryRef(t, w, repo)
	child := mqCreateTopLevelChild(t, w, repoRef, "mq-displaced")

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
	harness.CommitWork(t, child.GetDir())
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

	// Act: crash the daemon while its merge is HELD after the capture — the
	// pause's own record says the run can go no further — so nothing the
	// merge would do next (the git merge, a target bring-up) is under way.
	// Make the target dirty so the restart cannot resume the interrupted
	// merge — the same fact daemon/integration/merge_test.go's own bounce test
	// relies on (repo.SetDirty is USABLE against this suite's scripted fake
	// git).
	w.AwaitRunLogOperation("daemon.merge.capture_pause")
	repo.SetDirty(repo.Dir, true)
	w.Kill()

	// Act: a second real claude-repld boots on the SAME state root, store
	// socket and lock dir.
	d2Opts := w.SuccessorOpts(t)
	d2Opts.SelfRepo = repo.Dir
	d2 := harness.StartDaemon(t, d2Opts)
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
	d3Opts := w.SuccessorOpts(t)
	d3Opts.SelfRepo = repo.Dir
	d3 := harness.StartDaemon(t, d3Opts)
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
	// DISTINCT ROWS: Rows() holds every push, and a replayed prompt row is
	// pushed again when its turn's end settles it.
	mqDisplacedRows := func(rows []*frontendv1.FeedRow) int {
		ids := map[string]bool{}
		for _, row := range rows {
			if row.GetUserPrompt() != nil && mqFeedRowText(row) == displacedText {
				ids[row.GetId().GetValue()] = true
			}
		}
		return len(ids)
	}
	afterSecondBoot.AwaitRow("the replayed rows carrying the displaced turn's words", func(*frontendv1.FeedRow) bool {
		return mqDisplacedRows(afterSecondBoot.Rows()) >= matches
	})
	if got := mqDisplacedRows(afterSecondBoot.Rows()); got != matches {
		t.Fatalf("root feed rows carrying the displaced turn's words after a second bounce = %d, want unchanged at %d (never resubmitted twice)", got, matches)
	}

	// A resubmission the boot sweep made would arrive as one more such row.
	// Nothing else can end this wait, so it necessarily waits out the probe.
	//
	// A NEW ROW, NOT A NEW PUSH. The replay draws each prompt row and then
	// re-publishes it once its turn's end is replayed ("no longer working"),
	// and whether that restatement lands before or after the wait above is
	// how the stream happens to be drained — so counting pushes failed this
	// test on a row the feed already held. A resubmission is a turn of its
	// own, drawn as a row id the feed has not held.
	seen := map[string]bool{}
	for _, row := range afterSecondBoot.Rows() {
		if row.GetUserPrompt() != nil && mqFeedRowText(row) == displacedText {
			seen[row.GetId().GetValue()] = true
		}
	}
	deadline := time.NewTimer(harness.ProbeWindow)
	defer deadline.Stop()
	for done := false; !done; {
		select {
		case row, ok := <-afterSecondBoot.stream.C:
			if !ok {
				done = true
				break
			}
			if row.GetUserPrompt() != nil && mqFeedRowText(row) == displacedText && !seen[row.GetId().GetValue()] {
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

// ---------------------------------------------------------------------------
// The fake vendor's PROMPT MARKER — `e2e-fail-this-turn` — and the two
// OPPOSITE directions the merge pipeline classifies a configured action's
// failure in.
//
// WHY A MARKER AND NOT AN `!name`. Every other scenario is selected by a
// leading `!token`; this one is selected by a substring anywhere in otherwise
// ordinary prose (agent-shim/claude/shim/src/fake/registry.ts's
// FAIL_TURN_MARKER and selectScenario, which reaches FAIL_MARKER only after
// no `!token` matched). It exists because the merge pipeline's configured
// actions ARE their text — internal/merge/run.go's runConfiguredPrompt states
// it outright, "THE ACTION IS THE PROMPT, NOT A PROMPT'S NAME ... the recorded
// text is submitted verbatim" — so the only way to make a configured action's
// turn fail against the real vendor mock is to write an action a human would
// plausibly configure and bury the marker in it. The mock answers it with
// `error_during_execution` (failures.ts's FAIL_MARKER), which is
// AgentFailure.execution_error and therefore a FAILED turn close.
//
// WHAT THE TWO ARMS ARE. endpoint_create_workspace.proto:122-126 states the
// contract for the pair in one sentence: "before_ws_merge BEFORE the landing
// (its failure fails the run); postprocessing_prompt AFTER every commit lands
// (can never fail the run; its error rides the terminal status)". feed.proto
// repeats it per tab (:1907-1909 "its failure FAILS THE RUN"; :1988-1991 "its
// failure never fails the run (it rides the terminal)"). So ONE failing turn,
// moved from one arm to the other, must flip the merge's terminal between
// FeedMergeError.failed and FeedMergeSuccess — which is exactly the pair
// registry.ts's own note means by "classifies a before-action failure and an
// after-action failure in OPPOSITE directions".
//
// BOTH ARMS RUN THE SELF-REPO METHOD, and that is not incidental — see the
// production defect recorded at the after-arm's sub-test.
//
// This replaces the caller registry.ts still cites, `mergeactions_e2e_test.go`,
// which no longer exists; the marker had no counted e2e caller at all until
// this test.
// ---------------------------------------------------------------------------

// mqFailTurnMarker is the fake vendor's prompt marker, verbatim from
// agent-shim/claude/shim/src/fake/registry.ts's FAIL_TURN_MARKER.
const mqFailTurnMarker = "e2e-fail-this-turn"

// mqMarkedAction is a configured merge action a human would plausibly write,
// carrying the marker — the readable-prose shape the marker exists to allow.
const mqMarkedAction = "run the release checks before landing (e2e-fail-this-turn)"

// mqCreateChildWithActions is mqCreateTopLevelChild with configured merge
// actions recorded at creation, which is the only ingress that records them
// ("recorded in WSM at creation and read back by EVERY merge of this
// workspace on every ingress", endpoint_create_workspace.proto:66-68).
func mqCreateChildWithActions(t *testing.T, w *World, repoRef *workspacev1.RepositoryRef, name string, actions *agentreplv1.CreateWorkspaceMergeActions) *workspacev1.WorkspaceRef {
	t.Helper()
	resp, err := w.Client().CreateWorkspace(w.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository: repoRef,
		Form: &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{
			Name:         mqStrPtr(name),
			MergeActions: actions,
		}},
	}))
	if err != nil {
		t.Fatalf("CreateWorkspace(%s) = error %v, want a success", name, err)
	}
	ws := resp.Msg.GetSuccess().GetWorkspace()
	if ws.GetId() == "" {
		t.Fatalf("CreateWorkspace(%s) = %v, want a success carrying a workspace ref", name, resp.Msg)
	}
	return ws
}

// mqSelfRepoWorld mints a self-repo world whose merge test gate passes, so the
// self-repo method reaches its post-prompt rather than stopping at the gate.
func mqSelfRepoWorld(t *testing.T, repo *harness.Repo) *World {
	t.Helper()
	script := harness.NewTestAllScript(t, repo.Dir)
	script.SetExitCode(0)
	script.SetStdout("e2e marker: passed in 1s\n")
	return NewWorld(t, WorldOpts{DaemonOpts: harness.Opts{
		SelfRepo: repo.Dir,
		ExtraEnv: []string{"AGENT_REPL_TEST_ALL_SCRIPT=" + script.Path},
	}})
}

func TestFailMarkerFailsABeforeActionRunAndRidesAnAfterActionTerminal(t *testing.T) {
	t.Parallel()
	// A typo in the marker would leave both arms driving PLAIN PROSE, whose
	// turns conclude — so the before arm would fail with a confusing terminal
	// and the after arm would pass for the wrong reason. Stated here instead.
	if !strings.Contains(mqMarkedAction, mqFailTurnMarker) {
		t.Fatalf("the configured action %q does not carry the vendor's marker %q", mqMarkedAction, mqFailTurnMarker)
	}

	t.Run("before-action failure fails the run", func(t *testing.T) {
		t.Parallel()
		// Arrange: a child whose CONFIGURED before-merge action carries the
		// marker, so the real shim's fake vendor fails that turn.
		// harness.NewRepo directly, not mqCleanRepo: mqSelfRepoWorld writes
		// the passing gate script itself, and writing bin/test-all.sh twice
		// into one repo would leave two authors for one file.
		repo := harness.NewRepo(t)
		w := mqSelfRepoWorld(t, repo)
		repoRef := mqRepositoryRef(t, w, repo)
		child := mqCreateChildWithActions(t, w, repoRef, "mq-marker-before", &agentreplv1.CreateWorkspaceMergeActions{
			BeforeWsMerge: mqSaid(mqMarkedAction),
		})
		root := mqOpenFeedWatch(t, w, child, nil)
		defer root.Close()
		// Warning discipline: this run's own subject produces exactly one
		// daemon record — "a merge could not continue" (daemon.merge.abort,
		// internal/merge/terminal.go), the loud abort the failed precondition
		// is SUPPOSED to cause. It is declared rather than silenced.
		w.ExpectWarnings("daemon.merge.abort")

		// Act
		harness.CommitWork(t, child.GetDir())
		if _, err := w.Client().MergeWorkspace(w.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: child})); err != nil {
			t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
		}
		head := root.AwaitRow("the merge bubble's head", func(row *frontendv1.FeedRow) bool {
			return row.GetActivity().GetMerge() != nil
		})
		tabs := mqOpenFeedWatch(t, w, child, head.GetId())
		defer tabs.Close()

		// Assert: the pre_prompt tab settles FAILED, with the daemon's own
		// composed account of which action did not complete
		// (internal/merge/run.go's prePrompt: `the before-merge prompt %q did
		// not complete`, %q of the action's verbatim text).
		preTab := tabs.AwaitRow("the pre_prompt tab, settled", func(row *frontendv1.FeedRow) bool {
			return row.GetMergeTab().GetPrePrompt().GetSettled() != nil
		}).GetMergeTab().GetPrePrompt().GetSettled()
		if preTab.GetFailed() == nil {
			t.Fatalf("pre_prompt tab settled = %v, want the failed arm (the marked action's turn failed)", preTab)
		}
		wantSummary := fmt.Sprintf("the before-merge prompt %q did not complete", mqMarkedAction)
		if got := preTab.GetFailed().GetSummary(); got != wantSummary {
			t.Errorf("pre_prompt tab failure summary = %q, want %q", got, wantSummary)
		}

		// Assert: THE RUN FAILED — the direction this arm exists to pin. The
		// terminal is FeedMergeError's `failed` arm specifically, never
		// `abandoned` (which is eviction before ever reaching the front).
		terminal := root.AwaitRow("the merge bubble's terminal", func(row *frontendv1.FeedRow) bool {
			merge := row.GetActivity().GetMerge()
			return merge.GetError() != nil || merge.GetSuccess() != nil
		}).GetActivity().GetMerge()
		if terminal.GetError() == nil {
			t.Fatalf("merge terminal = %v, want FeedMergeError (a before-action failure fails the run)", terminal)
		}
		if terminal.GetError().GetFailed() == nil {
			t.Fatalf("merge error = %v, want the failed arm, never abandoned", terminal.GetError())
		}

		// Assert the SPECIFIC NEGATIVE that makes "BEFORE the landing" a fact
		// rather than a word: the run never reached a later phase at all, so
		// its sub-feed carries no merge, tests or post_prompt tab. A
		// before-action that failed AFTER the no-ff merge had already run
		// would satisfy every assertion above and still have landed work its
		// author's own precondition refused.
		//
		// THE SNAPSHOT IS A FRESH PAGE, NOT THE WATCH'S DRAINED PREFIX. The
		// watch above stopped draining the moment the pre_prompt tab settled,
		// so anything pushed after it would go unread and the negative would
		// be an artifact of when the reader stopped looking. The run has
		// reached its terminal by now, so nothing more can be pushed to this
		// sub-feed and its page IS the whole of it.
		subRows, _ := mqOpenFeedRows(t, w, child, head.GetId())
		for _, row := range subRows {
			switch tab := row.GetMergeTab(); {
			case tab.GetMerge() != nil:
				t.Errorf("the run opened a merge tab after its before-action failed: %v", row)
			case tab.GetTests() != nil:
				t.Errorf("the run opened a tests tab after its before-action failed: %v", row)
			case tab.GetPostPrompt() != nil:
				t.Errorf("the run opened a post_prompt tab after its before-action failed: %v", row)
			}
		}
	})

	for _, tc := range []struct {
		name     string
		selfRepo bool
	}{
		{name: "after-action failure rides the terminal", selfRepo: true},
		{name: "after-action failure rides the terminal outside the daemon's own repo", selfRepo: false},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			// Arrange: the SAME marked text, moved to the after-merge arm, run
			// under BOTH methods.
			//
			// The non-self arm was once the reverse: method's non-emacsRepo
			// branch returned postPrompt's error straight up, which execute()
			// turned into an abort, so for every repository that is not the
			// daemon's own an after-action failure DID fail the run — against
			// endpoint_create_workspace.proto's "can never fail the run",
			// feed.proto's "failure never fails the run (it rides the
			// terminal)" and postPrompt's own doc comment. FIXED: both methods
			// now run the after-action through run.go's single afterAction
			// helper, which warns and lets the run conclude, so the two cannot
			// drift apart again. This arm is what holds that.
			// harness.NewRepo directly, not mqCleanRepo: the self-repo world
			// writes the passing gate script itself, and writing
			// bin/test-all.sh twice into one repo would leave two authors for
			// one file.
			repo := harness.NewRepo(t)
			var w *World
			if tc.selfRepo {
				w = mqSelfRepoWorld(t, repo)
			} else {
				// The other method never runs the gate, so it needs no script.
				w = NewWorld(t, WorldOpts{})
			}
			repoRef := mqRepositoryRef(t, w, repo)
			child := mqCreateChildWithActions(t, w, repoRef, "mq-marker-after", &agentreplv1.CreateWorkspaceMergeActions{
				PostprocessingPrompt: mqSaid(mqMarkedAction),
			})
			root := mqOpenFeedWatch(t, w, child, nil)
			defer root.Close()
			// Warning discipline: exactly one daemon record, and it is the
			// contract's own — "the post-merge prompt failed; the merge still
			// landed" (daemon.merge.post_prompt, internal/merge/run.go's
			// afterAction). Its presence is half the fact this arm asserts:
			// the failure was RECORDED, not swallowed.
			w.ExpectWarnings("daemon.merge.post_prompt")

			// Act
			harness.CommitWork(t, child.GetDir())
			if _, err := w.Client().MergeWorkspace(w.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: child})); err != nil {
				t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
			}
			head := root.AwaitRow("the merge bubble's head", func(row *frontendv1.FeedRow) bool {
				return row.GetActivity().GetMerge() != nil
			})
			tabs := mqOpenFeedWatch(t, w, child, head.GetId())
			defer tabs.Close()

			// Assert: the post_prompt tab settles FAILED — the failure is
			// drawn, not swallowed.
			postTab := tabs.AwaitRow("the post_prompt tab, settled", func(row *frontendv1.FeedRow) bool {
				return row.GetMergeTab().GetPostPrompt().GetSettled() != nil
			}).GetMergeTab().GetPostPrompt().GetSettled()
			if postTab.GetFailed() == nil {
				t.Fatalf("post_prompt tab settled = %v, want the failed arm (the marked action's turn failed)", postTab)
			}
			wantSummary := fmt.Sprintf("the after-merge prompt %q did not complete", mqMarkedAction)
			if got := postTab.GetFailed().GetSummary(); got != wantSummary {
				t.Errorf("post_prompt tab failure summary = %q, want %q", got, wantSummary)
			}

			// Assert: THE RUN STILL LANDED — the opposite direction, from the
			// same failing turn. This is the whole point of the pair.
			terminal := root.AwaitRow("the merge bubble's terminal", func(row *frontendv1.FeedRow) bool {
				merge := row.GetActivity().GetMerge()
				return merge.GetError() != nil || merge.GetSuccess() != nil
			}).GetActivity().GetMerge()
			if terminal.GetSuccess() == nil {
				t.Fatalf("merge terminal = %v, want FeedMergeSuccess (an after-action failure rides the terminal)", terminal)
			}
		})
	}
}
