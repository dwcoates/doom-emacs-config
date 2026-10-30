package server

import (
	"context"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
)

// loadPageInEditor calls LoadPageInEditor for the harness's workspace,
// failing the test on a transport error.
func loadPageInEditor(t *testing.T, h *harness) *agentreplv1.LoadPageInEditorResponse {
	t.Helper()
	resp, err := h.Client.LoadPageInEditor(context.Background(),
		connect.NewRequest(&agentreplv1.LoadPageInEditorRequest{Workspace: ref()}))
	if err != nil {
		t.Fatalf("LoadPageInEditor: %v", err)
	}
	return resp.Msg
}

func TestLoadPageInEditorWithAHostStreamAnswersSuccess(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	holdHostStream(t, h)

	// Act.
	resp := loadPageInEditor(t, h)

	// Assert.
	if resp.GetSuccess() == nil {
		t.Fatalf("result = %v, want success", resp.GetResult())
	}
}

func TestLoadPageInEditorWithAHostStreamPushesLoadPageRequested(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	stream := holdHostStream(t, h)

	// Act.
	loadPageInEditor(t, h)
	push := receiveHostEvent(t, stream)

	// Assert.
	if push.GetLoadPageRequested() == nil {
		t.Fatalf("push = %v, want load_page_requested", push)
	}
}

func TestLoadPageInEditorRecordsTheRelayAtInfo(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	log := dlog.NewTestLogger()
	h.Surfaces.workspace = log
	holdHostStream(t, h)

	// Act.
	loadPageInEditor(t, h)

	// Assert.
	for _, rec := range log.Records() {
		if rec.Operation == "LoadPageInEditor" && rec.Level == "info" &&
			rec.Message == "relayed a load button's click to the editor, which loads the page" &&
			rec.Context["host_streams"] == 1 {
			return
		}
	}
	t.Fatalf("records = %+v, want the relay at info naming one host stream", log.Records())
}

func TestLoadPageInEditorWithNoHostStreamIsRefusedNoEditor(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp := loadPageInEditor(t, h)

	// Assert.
	if resp.GetError().GetNoEditor() == nil {
		t.Fatalf("result = %v, want no_editor", resp.GetResult())
	}
}

func TestLoadPageInEditorWithNoHostStreamRecordsTheRefusalAtWarn(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	log := dlog.NewTestLogger()
	h.Surfaces.workspace = log

	// Act.
	loadPageInEditor(t, h)

	// Assert.
	for _, rec := range log.Records() {
		if rec.Operation == "LoadPageInEditor" && rec.Level == "warn" &&
			rec.Message == "no host stream holds the workspace, so no editor loads its page" {
			return
		}
	}
	t.Fatalf("records = %+v, want the refusal at warn", log.Records())
}

func TestLoadPageInEditorOfAnUnknownWorkspaceIsRefusedByArm(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.LoadPageInEditor(context.Background(),
		connect.NewRequest(&agentreplv1.LoadPageInEditorRequest{
			Workspace: &workspacev1.WorkspaceRef{Id: "ws-nope"},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("LoadPageInEditor: %v", err)
	}
	if resp.Msg.GetError().GetUnknownWorkspace() == nil {
		t.Fatalf("result = %v, want unknown_workspace", resp.Msg.GetResult())
	}
}

func TestLoadPageInEditorWithNoRefIsInvalidArgument(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.Client.LoadPageInEditor(context.Background(),
		connect.NewRequest(&agentreplv1.LoadPageInEditorRequest{}))

	// Assert.
	if got := connectCode(t, err); got != connect.CodeInvalidArgument {
		t.Fatalf("code = %v, want invalid_argument", got)
	}
}
