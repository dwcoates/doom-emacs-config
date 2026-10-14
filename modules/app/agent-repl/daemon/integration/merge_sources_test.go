//go:build integration

package integration

import (
	"agentrepl/protohelpers/rosterwalk"
	"database/sql"
	"os"
	"path/filepath"
	"strings"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/integration/harness"
	"agent-repld/internal/merge"

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

// mergeAs asks for the fixture workspace's merge in one mode, as the user.
func (s *sourcedRepo) mergeAs(t *testing.T, mode harness.MergeMode) {
	t.Helper()
	resp, err := s.d.Client().MergeWorkspace(s.d.Ctx(), connect.NewRequest(harness.MergeRequest(s.f.ws, mode)))
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("MergeWorkspace = (%v, %v), want the merge enqueued", resp.Msg.GetResult(), err)
	}
}

// awaitConcluded waits for the fixture bubble's terminal and answers it.
func (s *sourcedRepo) awaitConcluded(t *testing.T, root *harness.Stream[*frontendv1.FeedRow]) *frontendv1.FeedMerge {
	t.Helper()
	row := awaitRow(t, s.f, root, "the merge's terminal", isMergeTerminal)
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

func TestAnAsyncMergeOfABranchWithAWorktreeLandsAndKeepsThatWorktree(t *testing.T) {
	t.Parallel()
	// Arrange: a subagent's branch checked out in a worktree of its own.
	s := newSourcedRepo(t)
	root := s.f.watchRootFeed()
	branchDir := s.repo.AddBranchWorktree("agent-fix")

	// Act.
	s.mergeAs(t, harness.Async("fix-merge", "agent-fix"))
	merge := s.awaitConcluded(t, root)
	s.d.AwaitLandingDeployed()

	// Assert: landed, the branch's worktree kept, the requester left open.
	if merge.GetSuccess() == nil {
		t.Fatalf("the merge ended %v, want landed", merge.GetResult())
	}
	if !s.repo.HasWorktree(branchDir) {
		t.Fatal("the branch's own worktree was removed without a cleanup")
	}
	if _, err := os.Stat(s.f.ws.GetDir()); err != nil {
		t.Fatalf("the requester's worktree is gone: %v", err)
	}
}

func TestAnAsyncMergeThatCleansUpRemovesTheBranchsWorktreeOnceItLands(t *testing.T) {
	t.Parallel()
	// Arrange: a subagent's branch checked out in a worktree of its own.
	s := newSourcedRepo(t)
	root := s.f.watchRootFeed()
	branchDir := s.repo.AddBranchWorktree("agent-fix")

	// Act.
	s.mergeAs(t, harness.Async("fix-merge", "agent-fix", harness.CleaningUp()))
	merge := s.awaitConcluded(t, root)
	s.d.AwaitLandingDeployed()

	// Assert: landed, and the branch's worktree removed while its branch stays.
	if merge.GetSuccess() == nil {
		t.Fatalf("the merge ended %v, want landed", merge.GetResult())
	}
	s.d.AwaitFileGone(branchDir)
	if s.repo.HasWorktree(branchDir) {
		t.Fatal("the branch's worktree is still registered after the cleanup")
	}
	if s.repo.BranchHead("agent-fix") == "" {
		t.Fatal("the cleanup deleted the branch, want it kept")
	}
	if _, err := os.Stat(s.f.ws.GetDir()); err != nil {
		t.Fatalf("the requester's worktree is gone: %v", err)
	}
}

func TestAnAsyncOneShotLandsAnotherWorkspacesBranchAndClosesThatWorkspace(t *testing.T) {
	t.Parallel()
	// Arrange: a second workspace with work; the first lands its branch async
	// and names it as the workspace to close.
	s := newSourcedRepo(t)
	repoRef := mergeRepositoryRef(t, s.d, s.repo)
	other := mergeCreateChild(t, s.d, repoRef, "other", "the other work", nil)
	harness.CommitWork(t, other.ws.GetDir())
	root := s.f.watchRootFeed()
	roster := s.d.WatchRoster()

	// Act.
	s.mergeAs(t, harness.Async("other-merge", mergeBranchOf(t, other.ws), harness.ClosingOnLanding(other.ws)))
	merge := s.awaitConcluded(t, root)
	s.d.AwaitLandingDeployed()

	// Assert: the other workspace is merged and gone; the requester stays.
	if merge.GetSuccess() == nil {
		t.Fatalf("the merge ended %v, want landed", merge.GetResult())
	}
	awaitRoster(t, s.d, roster, "the other workspace under recently_merged", func(r *frontendv1.WorkspaceRoster) bool {
		for _, row := range rosterwalk.MergedRows(r) {
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

func TestAnAsyncMergeIsRefusedWhatItCannotRunAsAsked(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name string
		mode func(s *sourcedRepo) harness.MergeMode
		want func(*agentreplv1.MergeWorkspaceError) bool
	}{
		{
			name: "the branch the requester has checked out",
			mode: func(s *sourcedRepo) harness.MergeMode { return harness.Async("own-merge", mergeBranchOf(t, s.f.ws)) },
			want: func(e *agentreplv1.MergeWorkspaceError) bool { return e.GetBranchCheckedOutInRequester() != nil },
		},
		{
			name: "a one-shot that names the requester",
			mode: func(s *sourcedRepo) harness.MergeMode {
				s.repo.AddBranch("agent-fix")
				return harness.Async("fix-merge", "agent-fix", harness.ClosingOnLanding(s.f.ws))
			},
			want: func(e *agentreplv1.MergeWorkspaceError) bool { return e.GetUnknownOneShotWorkspace() != nil },
		},
		{
			name: "a one-shot that names no workspace",
			mode: func(s *sourcedRepo) harness.MergeMode {
				s.repo.AddBranch("agent-fix")
				return harness.Async("fix-merge", "agent-fix", harness.ClosingOnLanding(&workspacev1.WorkspaceRef{Id: "nowhere", Dir: "/nowhere"}))
			},
			want: func(e *agentreplv1.MergeWorkspaceError) bool { return e.GetUnknownOneShotWorkspace() != nil },
		},
		{
			name: "a branch that does not exist",
			mode: func(*sourcedRepo) harness.MergeMode { return harness.Async("fix-merge", "no-such-branch") },
			want: func(e *agentreplv1.MergeWorkspaceError) bool { return e.GetUnknownBranch() != nil },
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			// Arrange.
			s := newSourcedRepo(t)
			s.d.ExpectWarnings("daemon.merge.enqueue")

			// Act.
			resp, err := s.d.Client().MergeWorkspace(s.d.Ctx(), connect.NewRequest(harness.MergeRequest(s.f.ws, tt.mode(s))))

			// Assert.
			if err != nil || !tt.want(resp.Msg.GetError()) {
				t.Fatalf("MergeWorkspace = (%v, %v), want the %s refusal", resp.Msg.GetResult(), err, tt.name)
			}
		})
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
	s.mergeAs(t, harness.OneShot())
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
	s.mergeAs(t, harness.OneShot())
	if _, err := s.d.Client().MergeWorkspace(s.d.Ctx(), connect.NewRequest(harness.MergeRequest(behind.ws, harness.OneShot()))); err != nil {
		t.Fatalf("MergeWorkspace(behind): %v", err)
	}
	// The session gives up on every attempt the commit's conflict gets.
	for attempt := 1; attempt <= merge.MaxConflictAttempts; attempt++ {
		s.awaitStartTurn(t, 1+attempt)
		pushConcludedTurn(s.f.shim, mainAgent, "gave-up")
	}

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
	s.mergeAs(t, harness.OneShot())
	for attempt := 1; attempt <= merge.MaxFixAttempts; attempt++ {
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
	s.mergeAs(t, harness.OneShot())
	s.awaitStartTurn(t, 2)
	s.repo.CommitIn(s.repo.Dir, "main.txt", "moved while fixing\n")
	s.script.SetExitCode(0)
	s.script.SetStdout("daemon: starting\ndaemon: passed in 1s\n")
	pushConcludedTurn(s.f.shim, mainAgent, "fixed")
	row := awaitRow(t, s.f, root, "the merge's terminal", isMergeTerminal)
	merge := row.GetActivity().GetMerge()
	s.d.AwaitLandingDeployed()

	// Assert: landed, after a second replay of the branch onto the new tip (the
	// first found it already on the old one), drawn under the one rebasing tab
	// as that replay's commit subtabs.
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
	s.f.awaitRowInFeed(row.GetId(), "a commit subtab of the second replay", func(row *frontendv1.FeedRow) bool {
		return row.GetMergeTab().GetRebaseCommit() != nil && row.GetMergeTab().GetLabel().GetRound() == 2
	})
}

func TestABranchMergedUpstreamUpdatesTheDefaultBranchAndClosesTheRequester(t *testing.T) {
	t.Parallel()
	// Arrange.
	s := newSourcedRepo(t)
	upstream := s.repo.SetUpstream(harness.DefaultBranch)
	root := s.f.watchRootFeed()

	// Act.
	s.mergeAs(t, harness.OneShotMergedUpstream())
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
	s.repo.Tag("0.0.1")
	harness.CommitWork(t, s.f.ws.GetDir())
	s.f.submit("finish up and merge", "k-asking", origin)
	s.awaitStartTurn(t, 2)
	roster := s.d.WatchRoster()
	root := s.f.watchRootFeed()

	// Act: the agent's command file is applied while its turn runs. A merge
	// onto agent-repl's own master names its version increment.
	path := commandfileWrite(t, s.d, "workspace_commands_ask.json",
		`[{"type":"merge","project_dir":"`+s.f.ws.GetDir()+`","one_shot":true,"version_part":"patch","changelog_line":"Fix the footer"}]`)
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
	s.mergeAs(t, harness.OneShot())
	merge := awaitRow(t, s.f, root, "the merge's terminal", func(row *frontendv1.FeedRow) bool {
		return row.GetActivity().GetMerge().GetSuccess() != nil
	})
	s.d.AwaitLandingDeployed()
	run := s.f.awaitRowInFeed(merge.GetId(), "the run with its log", func(row *frontendv1.FeedRow) bool {
		return row.GetMergeTab().GetTestsRun().GetOutput().GetLog() != nil
	})
	link := run.GetMergeTab().GetTestsRun().GetOutput().GetLog()

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
	// The missing brief is a fault the merge cannot continue past: its one
	// ERROR record is this test's subject.
	s.d.ExpectWarnings("daemon.merge.fault")
	if err := os.Remove(filepath.Join(s.d.PromptsDir, "merge-conflict-resolve.md")); err != nil {
		t.Fatalf("remove the brief: %v", err)
	}
	footer := s.d.WatchFooter(s.f.ws)
	harness.CommitWork(t, s.f.ws.GetDir())
	s.repo.CommitIn(s.repo.Dir, "main.txt", "moved\n")
	s.repo.ScriptRebaseConflict(mergeBranchOf(t, s.f.ws), 1, "work.txt")

	// Act.
	s.mergeAs(t, harness.OneShot())

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
	s.mergeAs(t, harness.OneShot())
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

// A LATE READER SEES THE FEED A LIVE ONE SAW. The repair turn's rows are drawn
// in the conflicted commit's subtab, and nowhere else, live; a daemon relaunched
// afterwards rebuilds the feed from the store's book alone, and its replay
// must draw them in the same tab and nowhere else, because the turn's record
// carries the address it ran at.
func TestARelaunchedDaemonReplaysARepairTurnInItsTabAlone(t *testing.T) {
	t.Parallel()
	// Arrange: a one-shot whose first conflicted commit the requester's own
	// session resolves (the repair turn under test), and whose second it gives
	// up on, so the merge FAILS and the requester stays open and served.
	s := newSourcedRepo(t)

	root := s.f.watchRootFeed()
	harness.CommitWork(t, s.f.ws.GetDir(), "work.txt")
	harness.CommitWork(t, s.f.ws.GetDir(), "more.txt")
	s.repo.CommitIn(s.repo.Dir, "main.txt", "moved\n")
	branch := mergeBranchOf(t, s.f.ws)
	s.repo.ScriptRebaseConflict(branch, 1, "work.txt")
	s.repo.ScriptRebaseConflict(branch, 2, "more.txt")
	s.d.ExpectWarnings("daemon.gitclient.merge_no_ff")
	s.mergeAs(t, harness.OneShot())
	req := s.f.shim.ExpectStartTurn()
	s.d.AwaitWorkspaceLogOperationCount(s.f.ws.GetDir(), harness.OpTurnOpened, 2)
	repair := req.GetTurn().GetValue()
	s.repo.ResolveConflicts(s.f.ws.GetDir())
	pushConcludedTurn(s.f.shim, mainAgent, "resolved")
	for attempt := 1; attempt <= merge.MaxConflictAttempts; attempt++ {
		s.f.shim.ExpectStartTurn()
		s.d.AwaitWorkspaceLogOperationCount(s.f.ws.GetDir(), harness.OpTurnOpened, 2+attempt)
		pushConcludedTurn(s.f.shim, mainAgent, "gave-up")
	}
	head := awaitRow(t, s.f, root, "the merge's failed terminal", func(row *frontendv1.FeedRow) bool {
		return row.GetActivity().GetMerge().GetError() != nil
	})
	// The failure is handed back to the requester as a turn of its own.
	s.f.shim.ExpectStartTurn()
	s.d.AwaitWorkspaceLogOperationCount(s.f.ws.GetDir(), harness.OpTurnOpened, 3+merge.MaxConflictAttempts)
	pushConcludedTurn(s.f.shim, mainAgent, "noted")
	liveTab := s.f.awaitRowInFeed(head.GetId(), "the repair prompt in its tab", func(row *frontendv1.FeedRow) bool {
		return row.GetUserPrompt() != nil && row.GetTurn().GetValue() == repair
	})
	if livePage, _ := s.f.openFeed(nil); pagePrompt(livePage, repair) {
		t.Fatalf("the live root feed carries the repair prompt %q, want it in its tab alone", repair)
	}

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
	successorOpts := s.d.SuccessorOpts()
	successorOpts.DurableState = true
	d2 := harness.StartDaemon(t, successorOpts)
	f2 := &fixture{d: d2, repo: s.repo, ws: s.f.ws, t: t}
	if again := harness.Register(t, d2, s.f.ws.GetDir()); again.GetId() != s.f.ws.GetId() {
		t.Fatalf("RegisterWorkspace after the relaunch = %q, want the same workspace %q", again.GetId(), s.f.ws.GetId())
	}
	f2.host = d2.WatchHost(s.f.ws)
	f2.web = d2.WatchWeb(s.f.ws)

	// Assert: the tab's row, at the identity and under the tab the live reader saw.
	replayedTab := f2.awaitRowInFeed(head.GetId(), "the replayed repair prompt in its tab", func(row *frontendv1.FeedRow) bool {
		return row.GetUserPrompt() != nil && row.GetTurn().GetValue() == repair
	})
	if replayedTab.GetId().GetValue() != liveTab.GetId().GetValue() || replayedTab.GetParent().GetRow().GetValue() != liveTab.GetParent().GetRow().GetValue() {
		t.Fatalf("replayed tab row = %v, want the live tab row %v", replayedTab, liveTab)
	}
	// Assert: the replay that drew the tab drew nothing of the turn on the root.
	if page, _ := f2.openFeed(nil); pagePrompt(page, repair) {
		t.Fatalf("the replayed root feed carries the repair prompt %q, want it in its tab alone", repair)
	}
}

// TestARelaunchedDaemonDrawsTheLandedMergesBubbleAgain covers the bubble a new
// daemon has no store to replay from: its head and its tabs are drawn again
// from the daemon's durable record, at the identity and order key the live
// reader saw.
func TestARelaunchedDaemonDrawsTheLandedMergesBubbleAgain(t *testing.T) {
	t.Parallel()
	// Arrange: a landed ASYNC merge, which leaves the requester open and
	// served, its bubble in the requester's feed.
	s := newSourcedRepo(t)
	root := s.f.watchRootFeed()
	s.repo.AddBranch("agent-fix")
	s.mergeAs(t, harness.Async("fix-merge", "agent-fix"))
	head := awaitRow(t, s.f, root, "the merge's landed terminal", func(row *frontendv1.FeedRow) bool {
		return row.GetActivity().GetMerge().GetSuccess() != nil
	})
	s.d.AwaitLandingDeployed()
	liveTabs, _ := s.f.openFeed(head.GetId())

	// Act: relaunch on the same state.
	expectSessionKillRecords(s.d)
	if _, err := s.d.Client().UpdateShutdownSchedule(s.d.Ctx(), connect.NewRequest(&agentreplv1.UpdateShutdownScheduleRequest{
		Action: &agentreplv1.UpdateShutdownScheduleRequest_Now{Now: &agentreplv1.UpdateShutdownScheduleNow{
			Reason: drainReasonOperator("the relaunch under test"),
		}},
	})); err != nil {
		t.Fatalf("UpdateShutdownSchedule{now} = %v, want the immediate shutdown accepted", err)
	}
	s.d.AwaitExit()
	successorOpts := s.d.SuccessorOpts()
	successorOpts.DurableState = true
	d2 := harness.StartDaemon(t, successorOpts)
	f2 := &fixture{d: d2, repo: s.repo, ws: s.f.ws, t: t}
	if again := harness.Register(t, d2, s.f.ws.GetDir()); again.GetId() != s.f.ws.GetId() {
		t.Fatalf("RegisterWorkspace after the relaunch = %q, want the same workspace %q", again.GetId(), s.f.ws.GetId())
	}

	// Assert: the head, landed, where it stood.
	page, _ := f2.openFeed(nil)
	var redrawn *frontendv1.FeedRow
	for _, row := range page.GetSuccess().GetRows() {
		if row.GetId().GetValue() == head.GetId().GetValue() {
			redrawn = row
		}
	}
	if redrawn.GetActivity().GetMerge().GetSuccess() == nil || redrawn.GetOrder().GetKey() != head.GetOrder().GetKey() {
		t.Fatalf("the relaunched root's merge head = %v, want the landed head at key %q", redrawn, head.GetOrder().GetKey())
	}
	// Assert: every tab the live reader saw.
	tabs, _ := f2.openFeed(head.GetId())
	if got, want := len(tabs.GetSuccess().GetRows()), len(liveTabs.GetSuccess().GetRows()); got != want || want == 0 {
		t.Fatalf("the relaunched bubble carries %d rows, want the live bubble's %d", got, want)
	}
}
