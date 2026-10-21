//go:build integration

package integration

import (
	"agentrepl/protohelpers/rosterwalk"
	"database/sql"
	"path/filepath"
	"strings"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/integration/harness"
	"agent-repld/internal/merge"
)

// async_merge_test.go covers the ASYNC (detached) merge
// (docs/protobuf-design/sync-async-merges.md): the merge workspace is hidden
// while it runs, its bubble is drawn in the requester's feed alone and placed
// rather than pinned, it lands into Recently Merged, and a merge that needs the
// user reveals it.

// mergeWorkspaceOf answers the merge workspace forked from REQUESTER, read off
// the registry, once the fork has registered it.
func mergeWorkspaceOf(t *testing.T, d *harness.Daemon, requester *workspacev1.WorkspaceRef) *workspacev1.WorkspaceRef {
	t.Helper()
	d.AwaitLogRecord(d.RunLogPath(), "the merge workspace's fork", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.mergeworkspace" && strings.HasPrefix(r.Message, "forked the hidden merge workspace")
	})
	var ref workspacev1.WorkspaceRef
	d.WithDB(func(db *sql.DB) {
		if err := db.QueryRow(`SELECT id, dir FROM workspaces WHERE merge_of = ?`, requester.GetId()).Scan(&ref.Id, &ref.Dir); err != nil {
			t.Fatalf("read the merge workspace of %s: %v", requester.GetId(), err)
		}
	})
	return &ref
}

// rosterNow answers the roster as a fresh watch first states it.
func rosterNow(t *testing.T, d *harness.Daemon) *frontendv1.WorkspaceRoster {
	t.Helper()
	return awaitRoster(t, d, d.WatchRoster(), "the roster as it stands", func(*frontendv1.WorkspaceRoster) bool { return true })
}

// liveRow answers WS's row in the roster's live sections, nil when it has none.
func liveRow(roster *frontendv1.WorkspaceRoster, ws string) *frontendv1.RosterRow {
	for _, row := range rosterwalk.AllRows(roster) {
		if row.GetWorkspace().GetWorkspace().GetId() == ws {
			for _, merged := range rosterwalk.MergedRows(roster) {
				if merged.GetWorkspace().GetWorkspace().GetId() == ws {
					return nil
				}
			}
			return row
		}
	}
	return nil
}

func TestAnAsyncMergeLandsWithItsBubblePlacedInTheRequestersFeed(t *testing.T) {
	t.Parallel()
	// Arrange.
	s := newSourcedRepo(t)
	root := s.f.watchRootFeed()
	roster := s.d.WatchRoster()
	s.repo.AddBranch("agent-fix")

	// Act.
	s.asyncMerge(t, "async-fix", "agent-fix")
	merge := awaitRow(t, s.f, root, "the async merge's terminal", isMergeTerminal)
	s.d.AwaitLandingDeployed()

	// Assert: landed, drawn placed in the requester's feed, and the merge
	// workspace shown only under Recently Merged.
	if merge.GetActivity().GetMerge().GetSuccess() == nil {
		t.Fatalf("the async merge ended %v, want landed", merge.GetActivity().GetMerge().GetResult())
	}
	if merge.GetOrder().GetPinned() != nil {
		t.Fatal("the async merge's head was pinned, want it placed")
	}
	mergeWS := mergeWorkspaceOf(t, s.d, s.f.ws)
	awaitRoster(t, s.d, roster, "the merge workspace under Recently Merged", func(r *frontendv1.WorkspaceRoster) bool {
		for _, row := range rosterwalk.MergedRows(r) {
			if row.GetWorkspace().GetWorkspace().GetId() == mergeWS.GetId() {
				return row.GetName().GetText() == "async-fix"
			}
		}
		return false
	})
	if row := liveRow(rosterNow(t, s.d), mergeWS.GetId()); row != nil {
		t.Fatalf("the landed merge workspace has a live row %v, want it under Recently Merged alone", row)
	}
}

func TestAnAsyncMergeWorkspaceStaysHiddenEvenAfterItsMergeFails(t *testing.T) {
	t.Parallel()
	// Owner ruling, 2026-10-20: a merge workspace is never auto-revealed on
	// any outcome; the bubble's own "open merge workspace" button is the one
	// way in.
	// Arrange: replaying the merge workspace's branch conflicts.
	s := newSourcedRepo(t)
	root := s.f.watchRootFeed()
	s.repo.AddBranch("agent-fix")
	s.repo.CommitIn(s.repo.Dir, "main.txt", "moved\n")
	s.repo.ScriptRebaseConflict("async-fix", 1, "branch.txt")
	// The sweep covers every test; the declared records are evidence of the
	// merge conflict the test stages.
	s.d.ExpectWarnings("daemon.gitclient.merge_no_ff")

	// Act: the merge workspace's agent is briefed on the conflict, and gives up.
	s.asyncMerge(t, "async-fix", "agent-fix")
	mergeWS := mergeWorkspaceOf(t, s.d, s.f.ws)
	shim := s.d.Shim(mergeWS)
	shim.ExpectStartTurn()
	s.d.AwaitWorkspaceLogOperationCount(mergeWS.GetDir(), harness.OpTurnOpened, 1)
	hidden := rosterNow(t, s.d)
	pushConcludedTurn(shim, mainAgent, "gave-up")
	// The agent gives up on every further attempt the commit's conflict gets.
	for attempt := 2; attempt <= merge.MaxConflictAttempts; attempt++ {
		shim.ExpectStartTurn()
		s.d.AwaitWorkspaceLogOperationCount(mergeWS.GetDir(), harness.OpTurnOpened, attempt)
		pushConcludedTurn(shim, mainAgent, "gave-up")
	}

	// Assert: hidden while the agent worked, and still hidden once the
	// requester's bubble has settled on the merge's failure.
	if row := liveRow(hidden, mergeWS.GetId()); row != nil {
		t.Fatalf("the merge workspace had a row %v while hidden, want none", row)
	}
	for _, open := range rosterwalk.OpenRows(hidden) {
		if open.GetRow().GetWorkspace().GetWorkspace().GetId() == mergeWS.GetId() {
			t.Fatalf("the merge workspace had a tab %v while hidden, want none", open.GetTab())
		}
	}
	awaitRow(t, s.f, root, "the requester's bubble settled on the merge's failure", func(row *frontendv1.FeedRow) bool {
		return row.GetActivity().GetMerge().GetError().GetFailed() != nil
	})
	settled := rosterNow(t, s.d)
	if row := liveRow(settled, mergeWS.GetId()); row != nil {
		t.Fatalf("the failed merge workspace has a row %v, want it left hidden", row)
	}
	for _, open := range rosterwalk.OpenRows(settled) {
		if open.GetRow().GetWorkspace().GetWorkspace().GetId() == mergeWS.GetId() {
			t.Fatalf("the failed merge workspace has a tab %v, want it left hidden", open.GetTab())
		}
	}
}

func TestAnAsyncInstructionStreamsItsAgentIntoTheResolvingTabAndThenLands(t *testing.T) {
	t.Parallel()
	// Arrange: a branch carrying the work to resolve.
	s := newSourcedRepo(t)
	root := s.f.watchRootFeed()
	s.repo.AddBranch("agent-fix")

	// Act: the merge workspace's agent is briefed with the instruction,
	// resolves (a commit lands in its worktree), and ends its turn.
	s.asyncMerge(t, "pick-fix", "agent-fix", harness.WithInstruction("the clean work"))
	mergeWS := mergeWorkspaceOf(t, s.d, s.f.ws)
	shim := s.d.Shim(mergeWS)
	brief := shim.ExpectStartTurn()
	s.d.AwaitWorkspaceLogOperationCount(mergeWS.GetDir(), harness.OpTurnOpened, 1)
	head := s.f.awaitRowInFeed(nil, "the async merge's head", func(row *frontendv1.FeedRow) bool {
		return row.GetActivity().GetMerge() != nil
	})
	pickTab := s.f.awaitRowInFeed(head.GetId(), "the resolving tab, live", func(row *frontendv1.FeedRow) bool {
		return row.GetMergeTab().GetResolving().GetLive() != nil
	})
	harness.CommitWork(t, mergeWS.GetDir())
	pushConcludedTurn(shim, mainAgent, "picked")

	// Assert: the brief carried the instruction; the pick turn's concluded row
	// stands in the requester's bubble under the pick tab; the merge lands.
	if !strings.Contains(text(brief.GetSaid()), "the clean work") {
		t.Fatalf("the pick brief = %q, want the instruction in it", text(brief.GetSaid()))
	}
	mirrored := s.f.awaitRowInFeed(head.GetId(), "the pick turn's row mirrored under the pick tab", func(row *frontendv1.FeedRow) bool {
		return row.GetTurnEnded().GetConcluded() != nil && row.GetParent().GetRow().GetValue() == pickTab.GetId().GetValue()
	})
	if mirrored.GetTurn() != nil {
		t.Fatalf("the mirrored row carries turn %v, want none of the requester's", mirrored.GetTurn())
	}
	s.f.awaitRowInFeed(head.GetId(), "the resolving tab, settled succeeded", func(row *frontendv1.FeedRow) bool {
		return row.GetMergeTab().GetResolving().GetSettled().GetSucceeded() != nil
	})
	merge := awaitRow(t, s.f, root, "the async merge's terminal", isMergeTerminal)
	s.d.AwaitLandingDeployed()
	if merge.GetActivity().GetMerge().GetSuccess() == nil {
		t.Fatalf("the picked merge ended %v, want landed", merge.GetActivity().GetMerge().GetResult())
	}
}

// TestAnAsyncLandingIsReportedOnlyOnceItsDeployHasRun pins that a landing in the
// daemon's own checkout reaches its requester only after the deploy that
// carries it has decided every component: the report is owed at the landing,
// covered by the deploy as it commits, and handed back once it has run.
func TestAnAsyncLandingIsReportedOnlyOnceItsDeployHasRun(t *testing.T) {
	t.Parallel()
	// Arrange
	s := newSourcedRepo(t)
	s.repo.AddBranch("agent-fix")

	// Act
	s.asyncMerge(t, "async-fix", "agent-fix")
	s.d.AwaitLandingDeployed()

	// Assert: owed, then covered, then deployed, then handed back.
	at := func(what string, pred func(harness.LogRecord) bool) int {
		t.Helper()
		for i, r := range s.d.RunLog() {
			if pred(r) {
				return i
			}
		}
		t.Fatalf("no record of %s in the run log", what)
		return -1
	}
	owed := at("the owed report", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.mergeworkspace" && r.Message == "a landed merge's report is owed until the deploy that carries it has run"
	})
	covered := at("the deploy's cover", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.mergeworkspace" && r.Message == "the deploy covers the landing reports owed before it"
	})
	deployed := at("the deploy deciding every component", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.deploy.run" && r.Message == "deployed: every component decided"
	})
	handedBack := at("the handback", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.mergeworkspace" && r.Message == "handed a detached merge's outcome back to its requester"
	})
	if !(owed < covered && covered < deployed && deployed < handedBack) {
		t.Fatalf("records at owed %d, covered %d, deployed %d, handed back %d; want them in that order", owed, covered, deployed, handedBack)
	}
}

// mainCheckoutRequester starts a daemon whose requester is the repository's
// MAIN CHECKOUT: registered with RegisterWorkspace alone, so it has no
// creation job, then opened and given one concluded turn so it has a
// conversation to fork.
func mainCheckoutRequester(t *testing.T) *sourcedRepo {
	t.Helper()
	repo := harness.NewRepo(t)
	script := harness.NewTestAllScript(t, repo.Dir)
	script.SetExitCode(0)
	script.SetStdout("daemon: starting\ndaemon: passed in 1s\n")
	repo.DeploysAsAgentRepl()
	d := harness.StartDaemon(t, harness.Opts{DurableState: true, Timeout: harness.MergeChainTimeout, ExtraEnv: []string{harness.TestAllScriptEnv(script.Path)}})
	f := &fixture{d: d, repo: repo, ws: harness.Register(t, d, repo.Dir), t: t}
	f.open()
	f.host = d.WatchHost(f.ws)
	f.web = d.WatchWeb(f.ws)
	f.shim.ExpectStartSession()
	f.submit("do the main checkout's work", "main-initial", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	f.shim.ExpectStartTurn()
	d.AwaitWorkspaceLogOperationCount(f.ws.GetDir(), harness.OpTurnOpened, 1)
	pushConcludedTurn(f.shim, mainAgent, "main-initial")
	return &sourcedRepo{d: d, repo: repo, script: script, f: f}
}

// mergeWorkspaceJob answers the merge workspace's own creation job: where it
// lands and its before-merge actions, as stored.
func mergeWorkspaceJob(t *testing.T, d *harness.Daemon, mergeWS *workspacev1.WorkspaceRef) (target, before string) {
	t.Helper()
	d.WithDB(func(db *sql.DB) {
		if err := db.QueryRow(`SELECT target_dir, actions_before FROM creation_jobs WHERE workspace_id = ?`, mergeWS.GetId()).Scan(&target, &before); err != nil {
			t.Fatalf("read the creation job of merge workspace %s: %v", mergeWS.GetId(), err)
		}
	})
	return target, before
}

// sameDir reports whether two spellings name one directory.
func sameDir(t *testing.T, a, b string) bool {
	t.Helper()
	ra, errA := filepath.EvalSymlinks(a)
	rb, errB := filepath.EvalSymlinks(b)
	if errA != nil || errB != nil {
		t.Fatalf("resolve %q and %q: %v, %v", a, b, errA, errB)
	}
	return ra == rb
}

func TestAnAsyncMergeFromTheMainCheckoutLandsInItsMainWorktree(t *testing.T) {
	t.Parallel()
	// Arrange: the main checkout, which the daemon registered and never created.
	s := mainCheckoutRequester(t)
	root := s.f.watchRootFeed()
	s.repo.AddBranch("agent-fix")

	// Act
	s.asyncMerge(t, "main-fix", "agent-fix")
	mergeWS := mergeWorkspaceOf(t, s.d, s.f.ws)
	merge := awaitRow(t, s.f, root, "the async merge's terminal", isMergeTerminal)
	s.d.AwaitLandingDeployed()

	// Assert: forked, landed, and landed in the repository's main worktree.
	if merge.GetActivity().GetMerge().GetSuccess() == nil {
		t.Fatalf("the main checkout's async merge ended %v, want landed", merge.GetActivity().GetMerge().GetResult())
	}
	if target, _ := mergeWorkspaceJob(t, s.d, mergeWS); !sameDir(t, target, s.repo.Dir) {
		t.Fatalf("the merge workspace lands in %q, want the main worktree %q", target, s.repo.Dir)
	}
}

func TestAnAsyncRequestsBeforeMergeActionRunsInItsMergeWorkspace(t *testing.T) {
	t.Parallel()
	// Arrange
	s := newSourcedRepo(t)
	root := s.f.watchRootFeed()
	s.repo.AddBranch("agent-fix")
	withBefore := func(async *agentreplv1.CreateWorkspaceAsyncMerge) {
		async.DeployHooks = preDeployPrompts("run the request's linters")
	}

	// Act: the merge workspace is handed the request's before-merge prompt,
	// and concludes it.
	s.asyncMerge(t, "async-fix", "agent-fix", withBefore)
	mergeWS := mergeWorkspaceOf(t, s.d, s.f.ws)
	shim := s.d.Shim(mergeWS)
	req := shim.ExpectStartTurn()
	s.d.AwaitWorkspaceLogOperationCount(mergeWS.GetDir(), harness.OpTurnOpened, 1)
	pushConcludedTurn(shim, mainAgent, "before-done")
	merge := awaitRow(t, s.f, root, "the async merge's terminal", isMergeTerminal)
	s.d.AwaitLandingDeployed()

	// Assert: the prompt ran in the merge workspace as its before action, and
	// the merge landed.
	if req.GetOrigin() != mergeBeforeActionOrigin || !strings.Contains(text(req.GetSaid()), "run the request's linters") {
		t.Fatalf("the merge workspace's turn = (%v, %q), want the request's before-merge action", req.GetOrigin(), text(req.GetSaid()))
	}
	if merge.GetActivity().GetMerge().GetSuccess() == nil {
		t.Fatalf("the async merge ended %v, want landed", merge.GetActivity().GetMerge().GetResult())
	}
}
