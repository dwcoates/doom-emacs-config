// hooks_e2e_test.go — feed helpers the hook cards' shim and daemon tiers left behind.
//
// The hook steps (blocked, failed) are retired: the shim's frames are asserted in
// agent-shim/claude/shim/test/integration/scenario-frames.test.ts and the cards the
// daemon draws from them in daemon/integration/feed_hooks_test.go.
package e2e

import (
	"testing"

	"agentrepl/e2e/kit/stack"
	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"connectrpc.com/connect"
)

// openFeedPage answers a fresh snapshot of the workspace's root feed. Called
// AFTER driveScenarioToCompletion/driveDocumentedPrompt has already
// synchronized on the turn's end and the sidecar's durable cursor advance,
// so this snapshot is a stable read of history, never a race against the
// turn still running.
func openFeedPage(t *testing.T, w *stack.World, ws *workspacev1.WorkspaceRef) []*frontendv1.FeedRow {
	t.Helper()
	d := w.Daemon
	resp, err := d.Client().OpenFeed(d.Ctx(), connect.NewRequest(&agentreplv1.OpenFeedRequest{Workspace: ws}))
	if err != nil {
		t.Fatalf("OpenFeed: %v", err)
	}
	success := resp.Msg.GetSuccess()
	if success == nil {
		t.Fatalf("OpenFeed = %v, want success", resp.Msg)
	}
	return success.GetPage().GetSuccess().GetRows()
}
