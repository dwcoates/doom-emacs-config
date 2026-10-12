//go:build integration

package integration

import (
	"agentrepl/protohelpers/rosterwalk"
	"database/sql"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/integration/harness"
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
	harness.CommitWork(t, s.f.ws.GetDir())

	// Act.
	s.mergeAs(t, harness.OwnBranchAsync("async-fix"))
	merge := awaitRow(t, s.f, root, "the async merge's terminal", func(row *frontendv1.FeedRow) bool {
		m := row.GetActivity().GetMerge()
		return m.GetSuccess() != nil || m.GetError() != nil
	})
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

func TestAnAsyncMergeWorkspaceIsHiddenUntilItsMergeFailsAndThenRevealed(t *testing.T) {
	t.Parallel()
	// Arrange: replaying the merge workspace's branch conflicts.
	s := newSourcedRepo(t)
	harness.CommitWork(t, s.f.ws.GetDir())
	s.repo.CommitIn(s.repo.Dir, "main.txt", "moved\n")
	s.repo.ScriptRebaseConflict("async-fix", 1, "work.txt")
	// The sweep covers every test; the declared records are evidence of the
	// merge conflict the test stages.
	s.d.ExpectWarnings("daemon.gitclient.merge_no_ff")

	// Act: the merge workspace's agent is briefed on the conflict, and gives up.
	s.mergeAs(t, harness.OwnBranchAsync("async-fix"))
	mergeWS := mergeWorkspaceOf(t, s.d, s.f.ws)
	shim := s.d.Shim(mergeWS)
	shim.ExpectStartTurn()
	s.d.AwaitWorkspaceLogOperationCount(mergeWS.GetDir(), harness.OpTurnOpened, 1)
	hidden := rosterNow(t, s.d)
	pushConcludedTurn(shim, mainAgent, "gave-up")

	// Assert: hidden while the agent worked, revealed once the merge failed,
	// with a row and a tab.
	if row := liveRow(hidden, mergeWS.GetId()); row != nil {
		t.Fatalf("the merge workspace had a row %v while hidden, want none", row)
	}
	for _, tab := range hidden.GetTabBar().GetTabs() {
		if tab.GetWorkspace().GetId() == mergeWS.GetId() {
			t.Fatalf("the merge workspace had a tab %v while hidden, want none", tab)
		}
	}
	revealed := awaitRoster(t, s.d, s.d.WatchRoster(), "the merge workspace revealed", func(r *frontendv1.WorkspaceRoster) bool {
		return liveRow(r, mergeWS.GetId()) != nil
	})
	for _, tab := range revealed.GetTabBar().GetTabs() {
		if tab.GetWorkspace().GetId() == mergeWS.GetId() {
			return
		}
	}
	t.Fatal("the revealed merge workspace has no tab")
}

func TestAnAsyncPickStreamsItsAgentIntoThePickTabAndThenLands(t *testing.T) {
	t.Parallel()
	// Arrange: the requester's branch carries the work to pick.
	s := newSourcedRepo(t)
	root := s.f.watchRootFeed()
	harness.CommitWork(t, s.f.ws.GetDir())

	// Act: the merge workspace's agent is briefed with the instruction, picks
	// (a commit lands in its worktree), and ends its turn.
	s.mergeAs(t, harness.OwnBranchPick("pick-fix", "the clean work"))
	mergeWS := mergeWorkspaceOf(t, s.d, s.f.ws)
	shim := s.d.Shim(mergeWS)
	brief := shim.ExpectStartTurn()
	s.d.AwaitWorkspaceLogOperationCount(mergeWS.GetDir(), harness.OpTurnOpened, 1)
	head := s.f.awaitRowInFeed(nil, "the async merge's head", func(row *frontendv1.FeedRow) bool {
		return row.GetActivity().GetMerge() != nil
	})
	pickTab := s.f.awaitRowInFeed(head.GetId(), "the pick tab, live", func(row *frontendv1.FeedRow) bool {
		return row.GetMergeTab().GetPick().GetLive() != nil
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
	s.f.awaitRowInFeed(head.GetId(), "the pick tab, settled succeeded", func(row *frontendv1.FeedRow) bool {
		return row.GetMergeTab().GetPick().GetSettled().GetSucceeded() != nil
	})
	merge := awaitRow(t, s.f, root, "the async merge's terminal", func(row *frontendv1.FeedRow) bool {
		m := row.GetActivity().GetMerge()
		return m.GetSuccess() != nil || m.GetError() != nil
	})
	s.d.AwaitLandingDeployed()
	if merge.GetActivity().GetMerge().GetSuccess() == nil {
		t.Fatalf("the picked merge ended %v, want landed", merge.GetActivity().GetMerge().GetResult())
	}
}
