//go:build integration

package integration

import (
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
func rosterNow(t *testing.T, d *harness.Daemon) *frontendv1.RosterPush {
	t.Helper()
	return awaitRoster(t, d, d.WatchRoster(), "the roster as it stands", func(*frontendv1.RosterPush) bool { return true })
}

// liveRow answers WS's row in the roster's streams, nil when it has none. A
// merged workspace is in no stream.
func liveRow(roster *frontendv1.RosterPush, ws string) *frontendv1.RosterRow {
	return rosterRow(roster, ws)
}

// leftForMerged reports whether PUSH has taken WS out of its streams into
// the Recently Merged summary, and its merged page draws it as OK says.
func leftForMerged(t *testing.T, d *harness.Daemon, push *frontendv1.RosterPush, ws string, ok func(*frontendv1.RosterRow) bool) bool {
	t.Helper()
	if rosterRow(push, ws) != nil || push.GetRecentlyMerged().GetHeader().GetCount().GetWorkspaces() == 0 {
		return false
	}
	row := mergedRowOf(t, d, ws)
	return row != nil && ok(row)
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
	awaitRoster(t, s.d, roster, "the merge workspace under Recently Merged", func(r *frontendv1.RosterPush) bool {
		return leftForMerged(t, s.d, r, mergeWS.GetId(), func(row *frontendv1.RosterRow) bool { return row.GetName().GetText() == "async-fix" })
	})
	if row := liveRow(rosterNow(t, s.d), mergeWS.GetId()); row != nil {
		t.Fatalf("the landed merge workspace has a live row %v, want it under Recently Merged alone", row)
	}
}
