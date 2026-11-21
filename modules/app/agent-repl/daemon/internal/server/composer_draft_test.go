package server

import (
	"context"
	"errors"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/ids"
)

// keptDraft is a ReportComposerDraft keeping TEXT for the harness workspace.
func keptDraft(text string) *agentreplv1.ReportComposerDraftRequest {
	return &agentreplv1.ReportComposerDraftRequest{Workspace: ref(), Draft: &agentreplv1.ReportComposerDraftRequest_Kept{
		Kept: &agentreplv1.ReportComposerDraftKept{Text: text}}}
}

// clearedDraft is a ReportComposerDraft clearing the harness workspace's draft.
func clearedDraft() *agentreplv1.ReportComposerDraftRequest {
	return &agentreplv1.ReportComposerDraftRequest{Workspace: ref(), Draft: &agentreplv1.ReportComposerDraftRequest_Cleared{
		Cleared: &agentreplv1.ReportComposerDraftCleared{}}}
}

// reportDraft sends REQ, failing the test on a transport error.
func reportDraft(t *testing.T, h *harness, req *agentreplv1.ReportComposerDraftRequest) *agentreplv1.ReportComposerDraftResponse {
	t.Helper()
	resp, err := h.Client.ReportComposerDraft(context.Background(), connect.NewRequest(req))
	if err != nil {
		t.Fatalf("ReportComposerDraft: %v", err)
	}
	return resp.Msg
}

func TestAKeptComposerDraftIsStoredAndRidesTheHostView(t *testing.T) {
	// Arrange
	h := newHarness(t)
	stream := holdHostStream(t, h)

	// Act
	resp := reportDraft(t, h, keptDraft("half a thought"))

	// Assert
	if resp.GetSuccess() == nil {
		t.Fatalf("result = %v, want success", resp.GetResult())
	}
	if got := h.DB.composerDrafts[testWorkspaceID]; got != "half a thought" {
		t.Fatalf("stored draft = %q, want the reported one", got)
	}
	draft := receivePush(t, stream, "the host view carrying the draft", func(r *agentreplv1.WatchHostWorkspaceResponse) *agentreplv1.HostComposerDraft {
		return r.GetHost().GetComposerDraft()
	})
	if draft.GetText() != "half a thought" {
		t.Fatalf("host view draft = %q, want the reported one", draft.GetText())
	}
}

func TestAClearedComposerDraftIsForgottenAndLeavesTheHostView(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.DB.composerDrafts = map[ids.WorkspaceID]string{testWorkspaceID: "restored already"}
	stream := holdHostStream(t, h)

	// Act
	resp := reportDraft(t, h, clearedDraft())

	// Assert
	if resp.GetSuccess() == nil {
		t.Fatalf("result = %v, want success", resp.GetResult())
	}
	if _, kept := h.DB.composerDrafts[testWorkspaceID]; kept {
		t.Fatal("the draft is still stored after it was cleared")
	}
	host := receivePush(t, stream, "the republished host view", (*agentreplv1.WatchHostWorkspaceResponse).GetHost)
	if host.GetComposerDraft() != nil {
		t.Fatalf("host view draft = %v, want none", host.GetComposerDraft())
	}
}

func TestAComposerDraftTheStoreRefusesIsAnsweredStoreFailed(t *testing.T) {
	// Arrange
	h, log := newLoggedHarness(t)
	h.DB.composerDrafts = map[ids.WorkspaceID]string{testWorkspaceID: "the earlier one"}
	h.DB.draftWriteErr = errors.New("disk I/O error")

	// Act
	resp := reportDraft(t, h, keptDraft("a newer one"))

	// Assert
	if got := resp.GetError().GetStoreFailed().GetReason(); got != "disk I/O error" {
		t.Fatalf("result = %v, want store_failed naming the store's error", resp.GetResult())
	}
	if got := h.DB.composerDrafts[testWorkspaceID]; got != "the earlier one" {
		t.Fatalf("stored draft = %q, want the earlier one kept", got)
	}
	for _, record := range log.at("ERROR") {
		if record.Operation == opComposerDraft && record.Context["draft"] == "kept" && record.Context["cause"] == "disk I/O error" {
			return
		}
	}
	t.Fatalf("no error record named the refused write: %+v", log.at("ERROR"))
}

func TestReportComposerDraftRefusesAMalformedReport(t *testing.T) {
	tests := []struct {
		name string
		req  *agentreplv1.ReportComposerDraftRequest
	}{
		{"an empty kept draft", keptDraft("")},
		{"no draft arm", &agentreplv1.ReportComposerDraftRequest{Workspace: ref()}},
		{"no workspace", &agentreplv1.ReportComposerDraftRequest{Draft: clearedDraft().GetDraft()}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			_, err := h.Client.ReportComposerDraft(context.Background(), connect.NewRequest(tc.req))

			// Assert
			var cerr *connect.Error
			if !errors.As(err, &cerr) || cerr.Code() != connect.CodeInvalidArgument {
				t.Fatalf("ReportComposerDraft = %v, want InvalidArgument", err)
			}
			if len(h.DB.composerDrafts) != 0 {
				t.Fatalf("stored drafts = %v, want none", h.DB.composerDrafts)
			}
		})
	}
}

func TestReportComposerDraftOnAnUnknownWorkspaceIsRefused(t *testing.T) {
	// Arrange
	h := newHarness(t)
	req := keptDraft("lost")
	req.Workspace = &workspacev1.WorkspaceRef{Id: "ws-nope"}

	// Act
	resp := reportDraft(t, h, req)

	// Assert
	if resp.GetError().GetUnknownWorkspace() == nil {
		t.Fatalf("result = %v, want unknown_workspace", resp.GetResult())
	}
}

func TestAHostViewWhoseDraftCannotBeReadIsNotPublished(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.DB.draftReadErr = errors.New("database is locked")
	s := h.Server.(*server)

	// Act
	_, ok := s.composeHostWorkspace(context.Background(), s.log, testWorkspaceID)

	// Assert
	if ok {
		t.Fatal("a host view was composed without its composer draft")
	}
}
