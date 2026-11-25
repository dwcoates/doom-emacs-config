//go:build integration

package integration

import (
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/integration/harness"
)

// standingclose_test.go exercises the standing close request end to end
// (agentrepl.v1.CloseWorkspaceRequest.stand): a parent whose close its open
// child blocks stands, the workspace status reports it, and the daemon closes
// the parent on the edges that clear the child, with no further ask; a
// withdrawn request is never carried out.

// opStandingClose and opClose are the daemon's records of a standing close
// and of every close (internal/workspace).
const (
	opStandingClose  = "daemon.workspace.standing_close"
	opWorkspaceClose = "daemon.workspace.close"
)

// standParentClose asks F's parent's close to stand, as the user, and fails
// unless it stands on the parent's one child.
func standParentClose(t *testing.T, f *retargetFamily) {
	t.Helper()
	resp, err := f.d.Client().CloseWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.CloseWorkspaceRequest{Requester: harness.User(), Workspace: f.parent, Stand: true}))
	if err != nil {
		t.Fatalf("CloseWorkspace = error %v", err)
	}
	requireOpenChildRefusal(t, f, resp.Msg.GetSuccess().GetStands().GetWaits().GetOpenChildren())
}

// closeChild closes F's child as the user, and settles the notice of that
// close its parent receives (the parent's second).
func closeChild(t *testing.T, f *retargetFamily) {
	t.Helper()
	resp, err := f.d.Client().CloseWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.CloseWorkspaceRequest{Requester: harness.User(), Workspace: f.child}))
	if err != nil || resp.Msg.GetSuccess().GetClosed() == nil {
		t.Fatalf("CloseWorkspace(child) = (%v, %v), want closed", resp, err)
	}
	settleParentNotice(t, f, "child-closed-notice", 2)
}

// parentStatus reads F's parent's status.
func parentStatus(t *testing.T, f *retargetFamily) *agentreplv1.WorkspaceStatus {
	t.Helper()
	resp, err := f.d.Client().GetWorkspaceStatus(f.d.Ctx(), connect.NewRequest(&agentreplv1.GetWorkspaceStatusRequest{Workspace: f.parent}))
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("GetWorkspaceStatus = (%v, %v), want a status", resp, err)
	}
	return resp.Msg.GetSuccess().GetStatus()
}

func TestAStandingCloseIsReportedByTheWorkspaceStatus(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRetargetFamily(t, harness.Opts{}, nil)
	settleParentNotice(t, f, "created-notice", 1)

	// Act
	standParentClose(t, f)

	// Assert
	request := parentStatus(t, f).GetCloseRequest()
	children := request.GetWaits().GetOpenChildren().GetChildren()
	if request.GetUser() == nil || len(children) != 1 || children[0].GetId() != f.child.GetId() {
		t.Fatalf("close request = %v, want the user's, waiting on the child", request)
	}
	requireStillNested(t, f)
}

func TestAStandingCloseIsCarriedOutOnceItsChildCloses(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRetargetFamily(t, harness.Opts{}, nil)
	settleParentNotice(t, f, "created-notice", 1)
	standParentClose(t, f)

	// Act
	closeChild(t, f)

	// Assert: the parent's notice turn ended, and nothing blocks it any more.
	f.d.AwaitWorkspaceLogRecord(f.parent.GetDir(), "the parent closed by its standing request", func(r harness.LogRecord) bool {
		return r.Operation == opWorkspaceClose && r.Message == "closed the workspace"
	})
	if standing := parentStatus(t, f).GetStanding(); standing.GetClosed() == nil || parentStatus(t, f).GetCloseRequest() != nil {
		t.Fatalf("parent standing = %v, want closed with no request left", standing)
	}
}

func TestAWithdrawnStandingCloseIsNotCarriedOut(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRetargetFamily(t, harness.Opts{}, nil)
	settleParentNotice(t, f, "created-notice", 1)
	standParentClose(t, f)
	withdrawn, err := f.d.Client().WithdrawWorkspaceClose(f.d.Ctx(), connect.NewRequest(&agentreplv1.WithdrawWorkspaceCloseRequest{Requester: harness.User(), Workspace: f.parent}))
	if err != nil || withdrawn.Msg.GetSuccess().GetWithdrawn() == nil {
		t.Fatalf("WithdrawWorkspaceClose = (%v, %v), want withdrawn", withdrawn, err)
	}

	// Act
	closeChild(t, f)

	// Assert: the parent's notice turn ended and was judged with no request
	// standing, and it is open.
	f.d.AwaitWorkspaceLogRecord(f.parent.GetDir(), "the parent judged with no request standing", func(r harness.LogRecord) bool {
		return r.Operation == opStandingClose && r.Message == "no close request stands, so there is nothing to judge"
	})
	status := parentStatus(t, f)
	if status.GetStanding().GetOpen() == nil || status.GetCloseRequest() != nil {
		t.Fatalf("parent status = %v, want open with no close request", status)
	}
}
