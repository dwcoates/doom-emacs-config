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
// end: a parent with an open child is not closed, killed or hidden, a child is
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
	awaitRoster(t, f.d, f.d.WatchRoster(), "the family still open and nested", func(r *frontendv1.WorkspaceRoster) bool {
		return f.nested(r)
	})
}

func TestAParentWithAnOpenChildIsNotClosed(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRetargetFamily(t, harness.Opts{}, harness.NewRepo(t))

	// Act
	resp, err := f.d.Client().CloseWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.CloseWorkspaceRequest{Workspace: f.parent}))

	// Assert
	if err != nil {
		t.Fatalf("CloseWorkspace = error %v", err)
	}
	requireOpenChildRefusal(t, f, resp.Msg.GetError().GetHasOpenChildren())
	requireStillNested(t, f)
}

func TestAParentWithAnOpenChildIsNotKilled(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRetargetFamily(t, harness.Opts{}, harness.NewRepo(t))

	// Act
	resp, err := f.d.Client().KillWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.KillWorkspaceRequest{Workspace: f.parent}))

	// Assert
	if err != nil {
		t.Fatalf("KillWorkspace = error %v", err)
	}
	requireOpenChildRefusal(t, f, resp.Msg.GetError().GetHasOpenChildren())
	requireStillNested(t, f)
}

func TestAParentWithAnOpenChildIsNotHidden(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRetargetFamily(t, harness.Opts{}, harness.NewRepo(t))

	// Act
	resp, err := f.d.Client().SetWorkspaceHidden(f.d.Ctx(), connect.NewRequest(&agentreplv1.SetWorkspaceHiddenRequest{
		Workspace: f.parent, Visibility: &agentreplv1.SetWorkspaceHiddenRequest_Hide{Hide: &agentreplv1.SetWorkspaceHiddenHide{}},
	}))

	// Assert
	if err != nil {
		t.Fatalf("SetWorkspaceHidden = error %v", err)
	}
	requireOpenChildRefusal(t, f, resp.Msg.GetError().GetHasOpenChildren())
	requireStillNested(t, f)
}

func TestAChildOfAHiddenParentIsNotCreated(t *testing.T) {
	t.Parallel()
	// Arrange: a parent with no children, hidden.
	d := newDaemon(t, harness.Opts{})
	repository := createRepositoryRef(t, d, harness.NewRepo(t))
	created, err := d.Client().CreateWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository: repository,
		Form:       &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{}},
	}))
	if err != nil || created.Msg.GetSuccess() == nil {
		t.Fatalf("CreateWorkspace(parent) = (%v, %v), want a success", created, err)
	}
	parent := created.Msg.GetSuccess().GetWorkspace()
	hide(t, d, parent)

	// Act
	resp, err := d.Client().CreateWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository: repository,
		Parent:     &agentreplv1.CreateWorkspaceParent{Workspace: parent},
		Form:       &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{}},
	}))

	// Assert
	if err != nil {
		t.Fatalf("CreateWorkspace(child) = error %v", err)
	}
	refused := resp.Msg.GetError().GetParentNotOpen()
	if refused.GetParent().GetId() != parent.GetId() || refused.GetHidden() == nil {
		t.Fatalf("result = %v, want parent_not_open naming the hidden parent", resp.Msg.GetResult())
	}
}

func TestAChildDoesNotSwapPlacesWithItsParent(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRetargetFamily(t, harness.Opts{}, harness.NewRepo(t))

	// Act
	resp, err := f.d.Client().SwapWorkspaces(f.d.Ctx(), connect.NewRequest(&agentreplv1.SwapWorkspacesRequest{Workspace: f.child, Target: f.parent}))

	// Assert: a child's tab follows its parent's, so the swap is refused.
	if err != nil || resp.Msg.GetError().GetNotSiblings() == nil {
		t.Fatalf("SwapWorkspaces = (%v, %v), want not_siblings", resp, err)
	}
	requireStillNested(t, f)
}

func TestAParentWhoseChildIsClosedIsClosed(t *testing.T) {
	t.Parallel()
	// Arrange: the child closed first, as the rules ask.
	f := newRetargetFamily(t, harness.Opts{}, harness.NewRepo(t))
	if resp, err := f.d.Client().CloseWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.CloseWorkspaceRequest{Workspace: f.child})); err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("CloseWorkspace(child) = (%v, %v), want a success", resp, err)
	}

	// Act
	resp, err := f.d.Client().CloseWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.CloseWorkspaceRequest{Workspace: f.parent}))

	// Assert
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("CloseWorkspace(parent) = (%v, %v), want a success", resp, err)
	}
}
