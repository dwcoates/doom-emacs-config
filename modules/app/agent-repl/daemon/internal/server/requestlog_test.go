package server

import (
	"context"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
)

// TestRequestBoundaryRecordsAnEndedReadAtInfo: a caller that leaves while the
// boundary reads its workspace is recorded at INFO, never as a failure.
func TestRequestBoundaryRecordsAnEndedReadAtInfo(t *testing.T) {
	// Arrange.
	log := &recordingLogger{}
	h := newHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log, workspace: log} })
	h.DB.workspaceErr = context.Canceled

	// Act.
	_ = clientLogOnce(h)

	// Assert.
	info := false
	for _, rec := range log.at("INFO") {
		if rec.Operation == "daemon.server.request_boundary" {
			info = true
		}
	}
	if !info {
		t.Fatal("the boundary did not state the ended read at INFO")
	}
	for _, rec := range log.at("ERROR") {
		if rec.Operation == "daemon.server.request_boundary" {
			t.Fatalf("the boundary recorded an ended read at ERROR: %+v", rec)
		}
	}
}

// TestRequestBoundaryAfterCloseReadsNoRegistry: a request reaching a closed
// surface binds its scope without reading the state client being torn down.
func TestRequestBoundaryAfterCloseReadsNoRegistry(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	s := h.Server.(*server)
	if err := s.Close(); err != nil {
		t.Fatalf("close the surface: %v", err)
	}
	h.DB.workspaceErr = context.DeadlineExceeded

	// Act.
	_, err := s.beginRequest(context.Background(), "ClientLog", "", clientLogRequestMessage())

	// Assert.
	if err != nil {
		t.Fatalf("beginRequest after Close = %v, want the scope bound without a registry read", err)
	}
}

// clientLogRequestMessage is a ClientLog request naming the harness workspace.
func clientLogRequestMessage() *agentreplv1.ClientLogRequest {
	return &agentreplv1.ClientLogRequest{Source: &agentreplv1.ClientLogRequest_Workspace{Workspace: ref()}}
}

func TestRequestWorkspaceIDReadsTheRequestsOwnWorkspace(t *testing.T) {
	// Arrange
	req := &agentreplv1.SubmitPromptRequest{Workspace: &workspacev1.WorkspaceRef{Id: "ws-own"}}

	// Act
	got := requestWorkspaceID(req)

	// Assert
	if got != "ws-own" {
		t.Fatalf("workspace = %q, want ws-own", got)
	}
}

func TestRequestWorkspaceIDReadsTheChosenArmsWorkspace(t *testing.T) {
	// Arrange: an eviction names its workspace inside its action arm.
	req := &agentreplv1.UpdateMergeQueueRequest{Action: &agentreplv1.UpdateMergeQueueRequest_Evict{
		Evict: &agentreplv1.UpdateMergeQueueEvict{Workspace: &workspacev1.WorkspaceRef{Id: "ws-evicted"}},
	}}

	// Act
	got := requestWorkspaceID(req)

	// Assert
	if got != "ws-evicted" {
		t.Fatalf("workspace = %q, want ws-evicted", got)
	}
}

func TestRequestWorkspaceIDAnswersNothingForARequestNamingNoWorkspace(t *testing.T) {
	// Arrange
	req := &agentreplv1.UpdateMergeQueueRequest{}

	// Act
	got := requestWorkspaceID(req)

	// Assert
	if got != "" {
		t.Fatalf("workspace = %q, want none", got)
	}
}
