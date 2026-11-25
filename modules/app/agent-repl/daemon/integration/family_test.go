//go:build integration

package integration

import (
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/integration/harness"
)

// family_test.go exercises the family rules (agentrepl.v1 family.proto) end to
// end: a parent with an open child is not closed, killed or backlogged, a child is
// not made under a parent that is not open, and two workspaces that are not
// siblings do not swap places. Each refusal names the blocking workspace and
// changes nothing the roster shows.

// requireOpenChildRefusal fails unless ARM names F's parent and its one child.
func requireOpenChildRefusal(t *testing.T, f *retargetFamily, arm *agentreplv1.WorkspaceHasOpenChildren) {
	t.Helper()
	if arm.GetParent().GetId() != f.parent.GetId() || len(arm.GetChildren()) != 1 || arm.GetChildren()[0].GetId() != f.child.GetId() {
		t.Fatalf("has_open_children = %v, want the parent naming its child", arm)
	}
}

// requireStillNested fails unless the roster still draws F's child under its
// open parent. A refusal pushes nothing, so the roster is read from a fresh
// subscription, whose first frame is the roster as it stands.
func requireStillNested(t *testing.T, f *retargetFamily) {
	t.Helper()
	awaitRoster(t, f.d, f.d.WatchRoster(), "the family still open and nested", func(r *frontendv1.RosterPush) bool {
		return f.nested(r)
	})
}

// settleParentNotice ends the Nth notice F's parent receives about its child
// (the child's creation is the first, its close the second) and waits for the
// turn's durable close: each notice opens a turn on the parent, and a close is
// refused while a turn runs.
func settleParentNotice(t *testing.T, f *retargetFamily, unit string, n int) {
	t.Helper()
	shim := f.d.Shim(f.parent)
	shim.ExpectStartTurn()
	pushConcludedTurn(shim, mainAgent, unit)
	f.d.AwaitTurnsClosed(f.parent.GetDir(), n)
}

func TestAParentWithAnOpenChildIsNotClosed(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRetargetFamily(t, harness.Opts{}, nil)
	settleParentNotice(t, f, "created-notice", 1)

	// Act
	resp, err := f.d.Client().CloseWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.CloseWorkspaceRequest{Requester: harness.User(), Workspace: f.parent}))

	// Assert
	if err != nil {
		t.Fatalf("CloseWorkspace = error %v", err)
	}
	requireOpenChildRefusal(t, f, resp.Msg.GetError().GetHasOpenChildren())
	requireStillNested(t, f)
}
