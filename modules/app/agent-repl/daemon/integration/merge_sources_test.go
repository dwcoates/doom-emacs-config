//go:build integration

package integration

import (
	"database/sql"
	"os"
	"path/filepath"
	"strings"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/integration/harness"

	"connectrpc.com/connect"
)

// merge_sources_test.go covers the merge that RUNS IN THE WORKSPACE THAT ASKED
// FOR IT (docs/protobuf-design/merge-landing.md, landed change 1): each
// source, the rebase-first process with its conflict resolution and bounded
// fixing, the start-over when the target moved, nothing parking, no merge fact
// before the requesting turn ends, and the test log opened by token.

// sourcedRepo is a self repository with a scripted gate and one created
// child workspace, whose merges take the full rebase-first process.
type sourcedRepo struct {
	d      *harness.Daemon
	repo   *harness.Repo
	script *harness.Recorder
	f      *fixture
}

func newSourcedRepo(t *testing.T) *sourcedRepo {
	t.Helper()
	f, d, repo, script := mergeCleanRepo(t)
	script.SetExitCode(0)
	script.SetStdout("daemon: starting\ndaemon: passed in 1s\n")
	f.repo = repo
	return &sourcedRepo{d: d, repo: repo, script: script, f: f}
}

// mergeAs asks for the fixture workspace's merge of one source, as the user.
func (s *sourcedRepo) mergeAs(t *testing.T, source *agentreplv1.MergeWorkspaceSource) {
	t.Helper()
	resp, err := s.d.Client().MergeWorkspace(s.d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: s.f.ws, Source: source}))
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("MergeWorkspace = (%v, %v), want the merge enqueued", resp.Msg.GetResult(), err)
	}
}

// awaitConcluded waits for the fixture bubble's terminal and answers it.
func (s *sourcedRepo) awaitConcluded(t *testing.T, root *harness.Stream[*frontendv1.FeedRow]) *frontendv1.FeedMerge {
	t.Helper()
	row := awaitRow(t, s.f, root, "the merge's terminal", func(row *frontendv1.FeedRow) bool {
		m := row.GetActivity().GetMerge()
		return m.GetSuccess() != nil || m.GetError() != nil
	})
	return row.GetActivity().GetMerge()
}

// awaitStartTurn waits for the daemon to hand the fixture's session a turn and
// for that turn to be open, and answers its request. n is how many turns the
// workspace will then have opened in all.
func (s *sourcedRepo) awaitStartTurn(t *testing.T, n int) string {
	t.Helper()
	req := s.f.shim.ExpectStartTurn()
	s.d.AwaitWorkspaceLogOperationCount(s.f.ws.GetDir(), harness.OpTurnOpened, n)
	return text(req.GetSaid())
}

func TestAnOwnBranchKeptOpenLandsAndLeavesTheWorkspaceOpen(t *testing.T) {
	t.Parallel()
	// Arrange.
	s := newSourcedRepo(t)
	root := s.f.watchRootFeed()
	harness.CommitWork(t, s.f.ws.GetDir())

	// Act.
	s.mergeAs(t, harness.OwnBranch(true))
	merge := s.awaitConcluded(t, root)
	s.d.AwaitLandingDeployed()

	// Assert.
	if merge.GetSuccess() == nil {
		t.Fatalf("the merge ended %v, want landed", merge.GetResult())
	}
	if _, err := os.Stat(s.f.ws.GetDir()); err != nil {
		t.Fatalf("the kept-open workspace's worktree is gone: %v", err)
	}
	var closed bool
	s.d.WithDB(func(db *sql.DB) {
		if err := db.QueryRow(`SELECT closed FROM workspaces WHERE id = ?`, s.f.ws.GetId()).Scan(&closed); err != nil {
			t.Fatalf("read the workspace's closed flag: %v", err)
		}
	})
	if closed {
		t.Fatal("a merge kept open closed its workspace")
	}
}

func TestABranchWithAWorktreeIsRebasedThereAndLands(t *testing.T) {
	t.Parallel()
	// Arrange: a subagent's branch checked out in a worktree of its own.
	s := newSourcedRepo(t)
	root := s.f.watchRootFeed()
	branchDir := s.repo.AddBranchWorktree("agent-fix")
	s.repo.CommitIn(s.repo.Dir, "main.txt", "moved\n")

	// Act.
	s.mergeAs(t, harness.BranchSource("agent-fix"))
	merge := s.awaitConcluded(t, root)
	s.d.AwaitLandingDeployed()

	// Assert: landed, the branch's worktree kept, the requester left open.
	if merge.GetSuccess() == nil {
		t.Fatalf("the merge ended %v, want landed", merge.GetResult())
	}
	if !s.repo.HasWorktree(branchDir) {
		t.Fatal("the branch's own worktree was removed")
	}
	if _, err := os.Stat(s.f.ws.GetDir()); err != nil {
		t.Fatalf("the requester's worktree is gone: %v", err)
	}
}

func TestABranchWithNoWorktreeGetsOneThatIsRemovedAfterTheMerge(t *testing.T) {
	t.Parallel()
	// Arrange: a branch checked out nowhere.
	s := newSourcedRepo(t)
	root := s.f.watchRootFeed()
	s.repo.AddBranch("agent-loose")

	// Act.
	s.mergeAs(t, harness.BranchSource("agent-loose"))
	merge := s.awaitConcluded(t, root)
	s.d.AwaitLandingDeployed()

	// Assert: the worktree the daemon made is gone, and was under its state.
	if merge.GetSuccess() == nil {
		t.Fatalf("the merge ended %v, want landed", merge.GetResult())
	}
	made := filepath.Join(s.d.StateDir, "merge-worktrees")
	entries, _ := os.ReadDir(made)
	if len(entries) != 0 {
		t.Fatalf("merge-worktrees holds %v after the merge, want the made worktree removed", entries)
	}
	for _, wt := range s.repo.Worktrees() {
		if strings.HasPrefix(wt, made) {
			t.Fatalf("the made worktree %s is still registered", wt)
		}
	}
}

func TestAnotherWorkspacesBranchLandsAndClosesThatWorkspace(t *testing.T) {
	t.Parallel()
	// Arrange: a second workspace with work; the first asks to merge it.
	s := newSourcedRepo(t)
	repoRef := mergeRepositoryRef(t, s.d, s.repo)
	other := mergeCreateChild(t, s.d, repoRef, "other", "the other work", nil)
	harness.CommitWork(t, other.ws.GetDir())
	root := s.f.watchRootFeed()
	roster := s.d.WatchRoster()

	// Act.
	s.mergeAs(t, &agentreplv1.MergeWorkspaceSource{Source: &agentreplv1.MergeWorkspaceSource_Workspace{
		Workspace: &agentreplv1.MergeWorkspaceSourceWorkspace{Ref: other.ws}}})
	merge := s.awaitConcluded(t, root)
	s.d.AwaitLandingDeployed()

	// Assert: the other workspace is merged and gone; the requester stays.
	if merge.GetSuccess() == nil {
		t.Fatalf("the merge ended %v, want landed", merge.GetResult())
	}
	awaitRoster(t, s.d, roster, "the other workspace under recently_merged", func(r *frontendv1.WorkspaceRoster) bool {
		for _, row := range r.GetRecentlyMerged().GetRows().GetRows() {
			if row.GetWorkspace().GetWorkspace().GetId() == other.ws.GetId() {
				return true
			}
		}
		return false
	})
	s.d.AwaitFileGone(other.ws.GetDir())
	if _, err := os.Stat(s.f.ws.GetDir()); err != nil {
		t.Fatalf("the requester's worktree is gone: %v", err)
	}
}

func TestAConflictIsResolvedByTheRequestersSessionAndTheRebaseContinues(t *testing.T) {
	t.Parallel()
	// Arrange: the target moved, and replaying the branch's work conflicts.
	s := newSourcedRepo(t)

	root := s.f.watchRootFeed()
	harness.CommitWork(t, s.f.ws.GetDir())
	s.repo.CommitIn(s.repo.Dir, "main.txt", "moved\n")
	s.repo.ScriptRebaseConflict(mergeBranchOf(t, s.f.ws), 1, "work.txt")

	// Act: the requester's own session resolves it.
	s.mergeAs(t, harness.OwnBranch(false))
	brief := s.awaitStartTurn(t, 2)
	s.repo.ResolveConflicts(s.f.ws.GetDir())
	pushConcludedTurn(s.f.shim, mainAgent, "resolved")
	merge := s.awaitConcluded(t, root)
	s.d.AwaitLandingDeployed()

	// Assert.
	if !strings.Contains(brief, s.f.ws.GetDir()) {
		t.Fatalf("the conflict brief = %q, want it to name the worktree it resolves in", brief)
	}
	if merge.GetSuccess() == nil {
		t.Fatalf("the merge ended %v, want landed after the resolution", merge.GetResult())
	}
}

func TestAResolutionThatGivesUpFailsInConflictsLeavesTheRebaseAndLetsTheNextMergeLand(t *testing.T) {
	t.Parallel()
	// Arrange: the front's conflict is left unresolved; a second workspace
	// waits behind it.
	s := newSourcedRepo(t)
	repoRef := mergeRepositoryRef(t, s.d, s.repo)
	behind := mergeCreateChild(t, s.d, repoRef, "behind", "the work behind", nil)
	harness.CommitWork(t, behind.ws.GetDir())
	harness.CommitWork(t, s.f.ws.GetDir())
	s.repo.CommitIn(s.repo.Dir, "main.txt", "moved\n")
	s.repo.ScriptRebaseConflict(mergeBranchOf(t, s.f.ws), 1, "work.txt")
	footer := s.d.WatchFooter(s.f.ws)
	roster := s.d.WatchRoster()
	behindRoot := behind.watchRootFeed()

	// Act.
	s.mergeAs(t, harness.OwnBranch(false))
	if _, err := s.d.Client().MergeWorkspace(s.d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: behind.ws, Source: harness.OwnBranch(false)})); err != nil {
		t.Fatalf("MergeWorkspace(behind): %v", err)
	}
	s.awaitStartTurn(t, 2)
	pushConcludedTurn(s.f.shim, mainAgent, "gave-up")

	// Assert: merge failed in conflicts, turquoise on the roster, rebase left.
	awaitFooter(t, s.f, footer, "merge failed in conflicts", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetMergeFailed().GetConflicts() != nil
	})
	awaitRoster(t, s.d, roster, "the roster's merge_failed", func(r *frontendv1.WorkspaceRoster) bool {
		return rosterRow(r, s.f.ws.GetId()).GetMergeFailed() != nil
	})
	if !s.repo.RebaseInProgress(s.f.ws.GetDir()) {
		t.Fatal("the failed resolution's rebase was not left in progress")
	}
	merge := awaitRow(t, behind, behindRoot, "the merge behind's terminal", func(row *frontendv1.FeedRow) bool {
		return row.GetActivity().GetMerge().GetSuccess() != nil
	})
	s.d.AwaitLandingDeployed()
	if merge.GetActivity().GetMerge().GetSuccess() == nil {
		t.Fatal("the merge behind the failed one did not land")
	}
}

func TestExhaustedFixingAttemptsFailTheMergeInTests(t *testing.T) {
	t.Parallel()
	// Arrange: every gate run fails.
	s := newSourcedRepo(t)
	s.d.ExpectWarnings("daemon.merge.tests", "daemon.scriptrunner.run")
	s.script.SetExitCode(1)
	s.script.SetStdout("daemon: starting\ndaemon failed after 1s with exit code 1\n")
	harness.CommitWork(t, s.f.ws.GetDir())
	footer := s.d.WatchFooter(s.f.ws)

	// Act: each fixing attempt's turn ends without a fix.
	s.mergeAs(t, harness.OwnBranch(false))
	for attempt := 1; attempt <= 3; attempt++ {
		s.awaitStartTurn(t, 1+attempt)
		pushConcludedTurn(s.f.shim, mainAgent, "fix-attempt")
	}

	// Assert.
	awaitFooter(t, s.f, footer, "merge failed in tests", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetMergeFailed().GetTests() != nil
	})
}

func TestATargetThatMovedBeforeCommittingStartsTheMergeOver(t *testing.T) {
	t.Parallel()
	// Arrange: the gate's first run fails, and while it is fixed the target
	// moves, so the gated branch is no longer on the tip it was rebased on.
	s := newSourcedRepo(t)
	s.d.ExpectWarnings("daemon.merge.tests", "daemon.scriptrunner.run")
	s.script.SetExitCode(1)
	s.script.SetStdout("daemon: starting\ndaemon failed after 1s with exit code 1\n")
	harness.CommitWork(t, s.f.ws.GetDir())
	root := s.f.watchRootFeed()

	// Act.
	s.mergeAs(t, harness.OwnBranch(false))
	s.awaitStartTurn(t, 2)
	s.repo.CommitIn(s.repo.Dir, "main.txt", "moved while fixing\n")
	s.script.SetExitCode(0)
	s.script.SetStdout("daemon: starting\ndaemon: passed in 1s\n")
	pushConcludedTurn(s.f.shim, mainAgent, "fixed")
	row := awaitRow(t, s.f, root, "the merge's terminal", func(row *frontendv1.FeedRow) bool {
		m := row.GetActivity().GetMerge()
		return m.GetSuccess() != nil || m.GetError() != nil
	})
	merge := row.GetActivity().GetMerge()
	s.d.AwaitLandingDeployed()

	// Assert: landed, after a second rebasing round that replayed the branch
	// onto the new tip (the first found it already on the old one).
	if merge.GetSuccess() == nil {
		t.Fatalf("the merge ended %v, want landed", merge.GetResult())
	}
	rebased := 0
	for _, call := range harness.World(t).Calls() {
		if len(call.Args) >= 4 && call.Args[2] == "rebase" && call.Args[3] == "-i" {
			rebased++
		}
	}
	if rebased != 1 {
		t.Fatalf("the branch was replayed %d times, want once: onto the moved tip", rebased)
	}
	s.f.awaitRowInFeed(row.GetId(), "the second rebasing round", func(row *frontendv1.FeedRow) bool {
		return row.GetMergeTab().GetRebasing() != nil && row.GetMergeTab().GetLabel().GetRound() == 2
	})
}

func TestABranchMergedUpstreamUpdatesTheDefaultBranchAndClosesTheRequester(t *testing.T) {
	t.Parallel()
	// Arrange.
	s := newSourcedRepo(t)
	upstream := s.repo.SetUpstream(harness.DefaultBranch)
	root := s.f.watchRootFeed()

	// Act.
	s.mergeAs(t, &agentreplv1.MergeWorkspaceSource{Source: &agentreplv1.MergeWorkspaceSource_MergedUpstream{
		MergedUpstream: &agentreplv1.MergeWorkspaceSourceMergedUpstream{}}})
	merge := s.awaitConcluded(t, root)
	s.d.AwaitLandingDeployed()

	// Assert.
	if merge.GetSuccess() == nil {
		t.Fatalf("the merge ended %v, want landed", merge.GetResult())
	}
	if got := s.repo.BranchHead(harness.DefaultBranch); got != upstream {
		t.Fatalf("the default branch is at %s, want upstream's %s", got, upstream)
	}
	s.d.AwaitFileGone(s.f.ws.GetDir())
}

func TestNoMergeFactReachesAnyClientBeforeTheRequestingTurnEnds(t *testing.T) {
	t.Parallel()
	// Arrange: the workspace's turn is in flight when its agent asks.
	s := newSourcedRepo(t)
	harness.CommitWork(t, s.f.ws.GetDir())
	s.f.submit("finish up and merge", "k-asking", origin)
	s.awaitStartTurn(t, 2)
	roster := s.d.WatchRoster()
	root := s.f.watchRootFeed()

	// Act: the agent's command file is applied while its turn runs.
	path := commandfileWrite(t, s.d, "workspace_commands_ask.json",
		`[{"type":"merge","project_dir":"`+s.f.ws.GetDir()+`"}]`)
	applied := filepath.Join(filepath.Dir(path), "applied", filepath.Base(path))
	s.d.AwaitFileExists(applied)

	// Assert: nothing about the merge on the roster or the feed.
	harness.ExpectNoPush(t, root, harness.ProbeWindow, "a merge row before the requesting turn ended")
	r := awaitRoster(t, s.d, roster, "the roster", func(*frontendv1.WorkspaceRoster) bool { return true })
	if row := rosterRow(r, s.f.ws.GetId()); row.GetMergeQueued() != nil || row.GetMerging() != nil {
		t.Fatalf("the roster drew %v before the requesting turn ended", row.GetStatus())
	}

	// Act: the turn ends.
	pushConcludedTurn(s.f.shim, mainAgent, "asked")

	// Assert: the merge is put in line and lands.
	merge := s.awaitConcluded(t, root)
	s.d.AwaitLandingDeployed()
	if merge.GetSuccess() == nil {
		t.Fatalf("the merge ended %v, want landed once the turn ended", merge.GetResult())
	}
}

func TestTheTestLogOpensThroughOpenInEditorByItsToken(t *testing.T) {
	t.Parallel()
	// Arrange: a merge kept open lands, so its workspace still answers.
	s := newSourcedRepo(t)
	harness.CommitWork(t, s.f.ws.GetDir())
	root := s.f.watchRootFeed()
	s.mergeAs(t, harness.OwnBranch(true))
	merge := awaitRow(t, s.f, root, "the merge's terminal", func(row *frontendv1.FeedRow) bool {
		return row.GetActivity().GetMerge().GetSuccess() != nil
	})
	s.d.AwaitLandingDeployed()
	tests := s.f.awaitRowInFeed(merge.GetId(), "the tests tab with its log", func(row *frontendv1.FeedRow) bool {
		return row.GetMergeTab().GetTests().GetLog() != nil
	})
	link := tests.GetMergeTab().GetTests().GetLog()

	// Act.
	resp, err := s.d.Client().OpenInEditor(s.d.Ctx(), connect.NewRequest(&agentreplv1.OpenInEditorRequest{
		Workspace: s.f.ws,
		Target:    &agentreplv1.OpenInEditorRequest_MergeTestLog{MergeTestLog: link.GetToken()},
	}))

	// Assert: relayed to Emacs as the log's path.
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("OpenInEditor = (%v, %v), want success", resp.Msg.GetResult(), err)
	}
	push := awaitView(t, s.f, s.f.host, "the open_in_editor push", func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
		return r.GetOpenInEditor() != nil
	})
	path := push.GetOpenInEditor().GetPath()
	if !strings.HasPrefix(path, filepath.Join(s.d.StateDir, "merge-logs")) {
		t.Fatalf("relayed path = %q, want the round's log under the state's merge-logs", path)
	}
	if body, err := os.ReadFile(path); err != nil || !strings.Contains(string(body), "daemon: passed") {
		t.Fatalf("the log = (%q, %v), want the gate's run", body, err)
	}
}

func TestAnUnknownTestLogTokenIsRefused(t *testing.T) {
	t.Parallel()
	// Arrange.
	f := newOpened(t, harness.Opts{})
	f.d.ExpectWarnings("daemon.merge.test_log", "OpenInEditor")

	// Act.
	resp, err := f.d.Client().OpenInEditor(f.d.Ctx(), connect.NewRequest(&agentreplv1.OpenInEditorRequest{
		Workspace: f.ws,
		Target:    &agentreplv1.OpenInEditorRequest_MergeTestLog{MergeTestLog: &frontendv1.FeedMergeTestLogToken{Value: "no-lease/1"}},
	}))

	// Assert.
	if err != nil || resp.Msg.GetError().GetUnknownMergeTestLog() == nil {
		t.Fatalf("OpenInEditor = (%v, %v), want unknown_merge_test_log", resp.Msg.GetResult(), err)
	}
}

func TestOpenInEditorRelaysAWorkspaceFileOntoTheHostStream(t *testing.T) {
	t.Parallel()
	// Arrange.
	f := newOpened(t, harness.Opts{})
	line := uint32(12)

	// Act.
	resp, err := f.d.Client().OpenInEditor(f.d.Ctx(), connect.NewRequest(&agentreplv1.OpenInEditorRequest{
		Workspace: f.ws,
		Target:    &agentreplv1.OpenInEditorRequest_WorkspaceFile{WorkspaceFile: &agentreplv1.OpenInEditorWorkspaceFile{Path: "lisp/core.el", Line: &line}},
	}))

	// Assert.
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("OpenInEditor = (%v, %v), want success", resp.Msg.GetResult(), err)
	}
	push := awaitView(t, f, f.host, "the open_in_editor push", func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
		return r.GetOpenInEditor() != nil
	})
	if got := push.GetOpenInEditor(); got.GetPath() != "lisp/core.el" || got.GetLine() != 12 {
		t.Fatalf("relayed %v, want lisp/core.el at line 12", got)
	}
}

func TestOpenInEditorRefusesAWorkspaceFileEscapingTheWorkspace(t *testing.T) {
	t.Parallel()
	// Arrange.
	f := newOpened(t, harness.Opts{})
	f.d.ExpectWarnings("daemon.workspace.open_in_editor", "OpenInEditor")

	// Act.
	resp, err := f.d.Client().OpenInEditor(f.d.Ctx(), connect.NewRequest(&agentreplv1.OpenInEditorRequest{
		Workspace: f.ws,
		Target:    &agentreplv1.OpenInEditorRequest_WorkspaceFile{WorkspaceFile: &agentreplv1.OpenInEditorWorkspaceFile{Path: "../../etc/passwd"}},
	}))

	// Assert.
	if err != nil || resp.Msg.GetError().GetPathEscapesWorkspace() == nil {
		t.Fatalf("OpenInEditor = (%v, %v), want path_escapes_workspace", resp.Msg.GetResult(), err)
	}
}

func TestAMissingConflictBriefFailsTheMergeLoudly(t *testing.T) {
	t.Parallel()
	// Arrange: the daemon's conflict brief is not there when it is needed.
	s := newSourcedRepo(t)

	if err := os.Remove(filepath.Join(s.d.PromptsDir, "merge-conflict-resolve.md")); err != nil {
		t.Fatalf("remove the brief: %v", err)
	}
	footer := s.d.WatchFooter(s.f.ws)
	harness.CommitWork(t, s.f.ws.GetDir())
	s.repo.CommitIn(s.repo.Dir, "main.txt", "moved\n")
	s.repo.ScriptRebaseConflict(mergeBranchOf(t, s.f.ws), 1, "work.txt")

	// Act.
	s.mergeAs(t, harness.OwnBranch(false))

	// Assert.
	awaitFooter(t, s.f, footer, "merge failed in other", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetMergeFailed().GetOther() != nil
	})
}

func TestALandedMergesLedgerRecordsEachStepsInterval(t *testing.T) {
	t.Parallel()
	// Arrange.
	s := newSourcedRepo(t)
	harness.CommitWork(t, s.f.ws.GetDir())
	s.repo.CommitIn(s.repo.Dir, "main.txt", "moved\n")
	root := s.f.watchRootFeed()

	// Act.
	s.mergeAs(t, harness.OwnBranch(true))
	s.awaitConcluded(t, root)
	s.d.AwaitLandingDeployed()
	s.d.Stop()

	// Assert.
	kinds := map[string]string{}
	s.d.WithDB(func(db *sql.DB) {
		rows, err := db.Query(`SELECT kind, outcome FROM merge_tab_intervals WHERE ended_at IS NOT NULL`)
		if err != nil {
			t.Fatalf("query merge_tab_intervals: %v", err)
		}
		defer rows.Close()
		for rows.Next() {
			var kind, outcome string
			if err := rows.Scan(&kind, &outcome); err != nil {
				t.Fatalf("scan: %v", err)
			}
			kinds[kind] = outcome
		}
	})
	for _, kind := range []string{"queue", "rebasing", "tests", "committing"} {
		if kinds[kind] != "succeeded" {
			t.Fatalf("intervals = %v, want %q succeeded", kinds, kind)
		}
	}
}

// pageRowOf answers the user-prompt row of a turn on a served page, nil when
// the page carries none.
func pageRowOf(page *frontendv1.FeedPage, turn string) *frontendv1.FeedRow {
	for _, row := range page.GetSuccess().GetRows() {
		if row.GetUserPrompt() != nil && row.GetTurn().GetValue() == turn {
			return row
		}
	}
	return nil
}

// A LATE READER SEES THE FEED A LIVE ONE SAW. The repair turn's rows are drawn
// in the merge's conflicts tab and mirrored onto the root feed live; a daemon
// relaunched afterwards rebuilds the feed from the store's book alone, and its
// replay must draw the same two copies, because the turn's record carries the
// mirrored address it ran at.
func TestARelaunchedDaemonReplaysAMirroredRepairTurnInItsTabAndOnTheRoot(t *testing.T) {
	t.Parallel()
	// Arrange: a conflict the requester's own session resolves, kept open.
	s := newSourcedRepo(t)

	root := s.f.watchRootFeed()
	harness.CommitWork(t, s.f.ws.GetDir())
	s.repo.CommitIn(s.repo.Dir, "main.txt", "moved\n")
	s.repo.ScriptRebaseConflict(mergeBranchOf(t, s.f.ws), 1, "work.txt")
	s.mergeAs(t, harness.OwnBranch(true))
	req := s.f.shim.ExpectStartTurn()
	s.d.AwaitWorkspaceLogOperationCount(s.f.ws.GetDir(), harness.OpTurnOpened, 2)
	repair := req.GetTurn().GetValue()
	s.repo.ResolveConflicts(s.f.ws.GetDir())
	pushConcludedTurn(s.f.shim, mainAgent, "resolved")
	head := awaitRow(t, s.f, root, "the merge's landed terminal", func(row *frontendv1.FeedRow) bool {
		return row.GetActivity().GetMerge().GetSuccess() != nil
	})
	s.d.AwaitLandingDeployed()
	livePage, _ := s.f.openFeedOnceCarrying("the repair prompt's root copy", func(p *frontendv1.FeedPage) bool {
		return pagePrompt(p, repair)
	})
	liveRoot := pageRowOf(livePage, repair)
	liveTab := s.f.awaitRowInFeed(head.GetId(), "the repair prompt in its tab", func(row *frontendv1.FeedRow) bool {
		return row.GetUserPrompt() != nil && row.GetTurn().GetValue() == repair
	})

	// Act: relaunch; the resumed shim serves the repair turn from the store.
	expectSessionKillRecords(s.d)
	if _, err := s.d.Client().UpdateShutdownSchedule(s.d.Ctx(), connect.NewRequest(&agentreplv1.UpdateShutdownScheduleRequest{
		Action: &agentreplv1.UpdateShutdownScheduleRequest_Now{Now: &agentreplv1.UpdateShutdownScheduleNow{
			Reason: drainReasonOperator("the late reader under test"),
		}},
	})); err != nil {
		t.Fatalf("UpdateShutdownSchedule{now} = %v, want the immediate shutdown accepted", err)
	}
	s.d.AwaitExit()
	s.d.WriteShimProfile(s.f.ws.GetDir(), harness.ShimProfile{
		ResumeHistory: harness.EncodeHistory(t, &conversationv1.HistoryEntry{
			Entry: &conversationv1.HistoryEntry_UserPrompt{UserPrompt: &conversationv1.AgentPrompt{
				Id:     &conversationv1.TurnId{Value: repair},
				Agent:  &conversationv1.AgentId{Value: mainAgent},
				Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR,
				Said:   req.GetSaid(),
			}},
		}),
	})
	d2 := harness.StartDaemon(t, harness.Opts{
		StateDir:   s.d.StateDir,
		ProfileDir: s.d.ProfileDir,
		ExtraArgs:  []string{"--default-config-dir", s.d.DefaultConfigDir},
	})
	f2 := &fixture{d: d2, repo: s.repo, ws: s.f.ws, t: t}
	if again := harness.Register(t, d2, s.f.ws.GetDir()); again.GetId() != s.f.ws.GetId() {
		t.Fatalf("RegisterWorkspace after the relaunch = %q, want the same workspace %q", again.GetId(), s.f.ws.GetId())
	}
	f2.host = d2.WatchHost(s.f.ws)
	f2.web = d2.WatchWeb(s.f.ws)

	// Assert: the root copy, at the identity the live reader saw.
	page, _ := f2.openFeedOnceCarrying("the replayed repair prompt's root copy", func(p *frontendv1.FeedPage) bool {
		return pagePrompt(p, repair)
	})
	if got := pageRowOf(page, repair); got.GetId().GetValue() != liveRoot.GetId().GetValue() {
		t.Fatalf("replayed root copy = %q, want the live root copy's %q", got.GetId().GetValue(), liveRoot.GetId().GetValue())
	}
	// Assert: the tab's row, at the identity and under the tab the live reader saw.
	tabPage, _ := f2.openFeed(head.GetId())
	var replayedTab *frontendv1.FeedRow
	for _, row := range tabPage.GetSuccess().GetRows() {
		if row.GetUserPrompt() != nil && row.GetTurn().GetValue() == repair {
			replayedTab = row
		}
	}
	if replayedTab.GetId().GetValue() != liveTab.GetId().GetValue() || replayedTab.GetParent().GetRow().GetValue() != liveTab.GetParent().GetRow().GetValue() {
		t.Fatalf("replayed tab row = %v, want the live tab row %v", replayedTab, liveTab)
	}
}
