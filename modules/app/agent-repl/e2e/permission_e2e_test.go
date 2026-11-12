// permission_e2e_test.go — the feed-watch helpers the permission area left behind.
//
// The permission step (an ask answered allow-standing) is retired: the shim's frames
// are asserted in agent-shim/claude/shim/test/integration/gate.test.ts and
// scenario-frames.test.ts, the daemon's draw of the answer in
// daemon/integration/feed_permission_standing_test.go.
package e2e

import (
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"connectrpc.com/connect"

	"agent-repld/integration/harness"
)

// pmNewPermissionWorld builds one World, registers a fresh (scripted fake-git)
// repository as a workspace, and opens it.
func pmNewPermissionWorld(t *testing.T) (*World, *workspacev1.WorkspaceRef) {
	t.Helper()
	w := NewWorld(t, WorldOpts{})
	ws := w.NewWorkspace(t)
	w.RequireOpen(t, ws)
	return w, ws
}

// pmOpenFeedWatch opens the workspace's root feed and answers both the current
// page and a live tail from exactly where the page ends — the same pattern
// world_test.go's AwaitTurnEnded uses, generalized so this file's own
// predicates are not limited to "turn ended".
func pmOpenFeedWatch(t *testing.T, w *World, ws *workspacev1.WorkspaceRef) (*frontendv1.FeedPageSuccess, *harness.Stream[*frontendv1.FeedRow]) {
	t.Helper()
	opened, err := w.Client().OpenFeed(w.Ctx(), connect.NewRequest(&agentreplv1.OpenFeedRequest{Workspace: ws}))
	if err != nil {
		t.Fatalf("OpenFeed: %v", err)
	}
	success := opened.Msg.GetSuccess()
	if success == nil {
		t.Fatalf("OpenFeed = %v, want success", opened.Msg)
	}
	return success.GetPage().GetSuccess(), w.WatchFeedOn(w.Client(), success.GetWatch())
}

// pmAwaitFeedRow opens the feed and answers the first row (already on the
// current page, or arriving on the tail) satisfying pred. Used whenever a
// permission test needs a row whose CURRENT state is stable until the test
// itself changes it (an open ask stays open until answered; a settled row
// stays settled) — see pmWatchFeedFromNow for the case where the sequence of
// pushes, not merely the current value, is the fact under test.
func pmAwaitFeedRow(t *testing.T, w *World, ws *workspacev1.WorkspaceRef, what string, pred func(*frontendv1.FeedRow) bool) *frontendv1.FeedRow {
	t.Helper()
	page, stream := pmOpenFeedWatch(t, w, ws)
	defer stream.Close()
	for _, row := range page.GetRows() {
		if pred(row) {
			return row
		}
	}
	return harness.AwaitView(t, w.Ctx(), stream, what, pred)
}
