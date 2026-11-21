package server

import (
	"context"
	"errors"
	"slices"
	"strings"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

func TestKeepLiveWorkspaceAnswersTheKeepersResponse(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.KeepLives.response = &agentreplv1.KeepLiveWorkspaceResponse{Result: &agentreplv1.KeepLiveWorkspaceResponse_Success{
		Success: &agentreplv1.KeepLiveWorkspaceSuccess{Outcome: &agentreplv1.KeepLiveWorkspaceSuccess_Busy{Busy: &agentreplv1.KeepLiveWorkspaceBusy{}}},
	}}

	// Act.
	resp, err := h.Client.KeepLiveWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.KeepLiveWorkspaceRequest{Workspace: ref()}))

	// Assert.
	if err != nil || resp.Msg.GetSuccess().GetBusy() == nil {
		t.Fatalf("KeepLiveWorkspace = %v, %v; want the keeper's answer", resp, err)
	}
	if !slices.Equal(h.KeepLives.asked, []ids.WorkspaceID{testWorkspaceID}) {
		t.Fatalf("asked = %v, want the workspace kept live", h.KeepLives.asked)
	}
}

func TestKeepLiveWorkspaceRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	unknown := ref()
	unknown.Id = "ws-unknown"

	// Act.
	resp, err := h.Client.KeepLiveWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.KeepLiveWorkspaceRequest{Workspace: unknown}))

	// Assert.
	if err != nil || resp.Msg.GetError().GetUnknownWorkspace() == nil || len(h.KeepLives.asked) != 0 {
		t.Fatalf("KeepLiveWorkspace = %v, %v with asked %v; want unknown_workspace and nothing kept live", resp, err, h.KeepLives.asked)
	}
}

func TestKeepLiveWorkspaceAnswersCanceledWhenTheCallEndsFirst(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	h := newHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log, workspace: log} })
	h.KeepLives.err = errors.New("cachelapse: the keep-live call ended before its run did: context canceled")

	// Act.
	_, err := h.Client.KeepLiveWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.KeepLiveWorkspaceRequest{Workspace: ref()}))

	// Assert.
	if connect.CodeOf(err) != connect.CodeCanceled {
		t.Fatalf("KeepLiveWorkspace error = %v, want canceled", err)
	}
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Operation: "daemon.server.keep_live_workspace", Message: "the keep-live call ended before its run did"}); !ok {
		t.Fatalf("records = %+v, want the abandoned call recorded", log.Records())
	}
}

func TestNewRefusesAMissingKeepLive(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	deps := h.withoutDep(func(d *Deps) { d.KeepLives = nil })

	// Act.
	_, err := New(deps)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "the cache keeper's keep-live") {
		t.Fatalf("New = %v, want the refusal naming the keep-live", err)
	}
}
