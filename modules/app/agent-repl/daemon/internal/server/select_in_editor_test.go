package server

import (
	"context"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/workspace"
	"agent-repld/internal/wsm"
)

// holdHostStream opens the workspace's host stream and waits for its opening
// `host` push, which the server sends only after it has subscribed the stream's
// events: from then on an event emitted for the workspace reaches it.
func holdHostStream(t *testing.T, h *harness) *connect.ServerStreamForClient[agentreplv1.WatchHostWorkspaceResponse] {
	t.Helper()
	ctx, cancel := context.WithCancel(context.Background())
	t.Cleanup(cancel)
	stream := dialHostStream(t, h, ctx)
	receivePush(t, stream, "the opening host push", func(r *agentreplv1.WatchHostWorkspaceResponse) *agentreplv1.HostWorkspace {
		return r.GetHost()
	})
	return stream
}

// selectInEditor calls SelectInEditor for the harness's workspace, failing the
// test on a transport error.
func selectInEditor(t *testing.T, h *harness) *agentreplv1.SelectInEditorResponse {
	t.Helper()
	resp, err := h.Client.SelectInEditor(context.Background(),
		connect.NewRequest(&agentreplv1.SelectInEditorRequest{Workspace: ref()}))
	if err != nil {
		t.Fatalf("SelectInEditor: %v", err)
	}
	return resp.Msg
}

func TestSelectInEditorWithAHostStreamAnswersRelayed(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	holdHostStream(t, h)

	// Act.
	resp := selectInEditor(t, h)

	// Assert.
	if resp.GetSuccess().GetRelayed() == nil {
		t.Fatalf("result = %v, want success relayed", resp.GetResult())
	}
}

func TestSelectInEditorWithAHostStreamPushesSelectRequested(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	stream := holdHostStream(t, h)

	// Act.
	selectInEditor(t, h)
	push := receiveHostEvent(t, stream)

	// Assert.
	if push.GetSelectRequested() == nil {
		t.Fatalf("push = %v, want select_requested", push)
	}
}

func TestSelectInEditorWithAHostStreamSelectsNothingItself(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	holdHostStream(t, h)

	// Act.
	selectInEditor(t, h)

	// Assert: the editor makes the switch, so the daemon stamps nothing.
	if got := h.Verbs.selects.Load(); got != 0 {
		t.Fatalf("selects = %d, want none on the relayed route", got)
	}
}

func TestSelectInEditorWithAHostStreamRevealsNoMergeWorkspace(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	record := h.DB.workspaces[testWorkspaceID]
	record.MergeOf = &wsm.MergeOrigin{Requester: "ws-requester", Request: "r-1"}
	h.DB.workspaces[testWorkspaceID] = record
	holdHostStream(t, h)

	// Act.
	selectInEditor(t, h)

	// Assert: the reveal belongs to the select the editor then makes.
	if len(h.Merge.reveals) != 0 {
		t.Fatalf("reveals = %v, want none on the relayed route", h.Merge.reveals)
	}
}

func TestSelectInEditorRecordsTheRelayAtInfo(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	log := dlog.NewTestLogger()
	h.Surfaces.workspace = log
	holdHostStream(t, h)

	// Act.
	selectInEditor(t, h)

	// Assert.
	for _, rec := range log.Records() {
		if rec.Operation == "SelectInEditor" && rec.Level == "info" &&
			rec.Message == "relayed a row click to the editor, which makes the switch" &&
			rec.Context["host_streams"] == 1 {
			return
		}
	}
	t.Fatalf("records = %+v, want the relay at info naming one host stream", log.Records())
}

func TestSelectInEditorWithNoHostStreamSelectsTheWorkspace(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Deps.Sidebar.(*fakeSidebar).revision = 42

	// Act.
	resp := selectInEditor(t, h)

	// Assert.
	if got := resp.GetSuccess().GetSelected().GetRosterRevision(); got != 42 {
		t.Fatalf("result = %v, want success selected at the sidebar's revision 42", resp.GetResult())
	}
	if got := h.Verbs.selects.Load(); got != 1 {
		t.Fatalf("selects = %d, want the daemon's one select", got)
	}
	if got := workspace.SelectionHistory(h.Verbs.lastSelectHistory.Load()); got != workspace.RecordSelection {
		t.Fatalf("history = %v, want RecordSelection: a click is a choice", got)
	}
}

func TestSelectInEditorWithNoHostStreamRecordsTheDaemonsRoute(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	log := dlog.NewTestLogger()
	h.Surfaces.workspace = log

	// Act.
	selectInEditor(t, h)

	// Assert.
	for _, rec := range log.Records() {
		if rec.Operation == "SelectInEditor" && rec.Level == "info" &&
			rec.Message == "no host stream holds the workspace; the daemon selects it" {
			return
		}
	}
	t.Fatalf("records = %+v, want the daemon's route at info", log.Records())
}

func TestSelectInEditorWithNoHostStreamAnswersTheSelectsRefusal(t *testing.T) {
	// Arrange.
	h := standingDownSelect(t)

	// Act.
	resp := selectInEditor(t, h)

	// Assert.
	if resp.GetError().GetStandingDown() == nil {
		t.Fatalf("result = %v, want standing_down", resp.GetResult())
	}
}

func TestSelectInEditorOfAnUnknownWorkspaceIsRefusedByArm(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.SelectInEditor(context.Background(),
		connect.NewRequest(&agentreplv1.SelectInEditorRequest{
			Workspace: &workspacev1.WorkspaceRef{Id: "ws-nope"},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("SelectInEditor: %v", err)
	}
	if resp.Msg.GetError().GetUnknownWorkspace() == nil {
		t.Fatalf("result = %v, want unknown_workspace", resp.Msg.GetResult())
	}
	if got := h.Verbs.selects.Load(); got != 0 {
		t.Fatalf("selects = %d, want none for an unknown workspace", got)
	}
}

func TestSelectInEditorWithNoRefIsInvalidArgument(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.Client.SelectInEditor(context.Background(),
		connect.NewRequest(&agentreplv1.SelectInEditorRequest{}))

	// Assert.
	if got := connectCode(t, err); got != connect.CodeInvalidArgument {
		t.Fatalf("code = %v, want invalid_argument", got)
	}
}

// TestTheDaemonsSelectIsOneSelectForBothRpcs pins selectSubject as the ONE
// select a client's request makes: SelectWorkspace and SelectInEditor's daemon
// route reveal a hidden merge workspace, let every held banner go and answer
// the sidebar's revision alike, so a select spelled by hand in either handler
// fails here rather than drifting from the other.
func TestTheDaemonsSelectIsOneSelectForBothRpcs(t *testing.T) {
	tests := []struct {
		name string
		call func(h *harness) (uint64, error)
	}{
		{
			name: "SelectWorkspace",
			call: func(h *harness) (uint64, error) {
				resp, err := h.Client.SelectWorkspace(context.Background(),
					connect.NewRequest(&agentreplv1.SelectWorkspaceRequest{Workspace: ref()}))
				if err != nil {
					return 0, err
				}
				return resp.Msg.GetSuccess().GetRosterRevision(), nil
			},
		},
		{
			name: "SelectInEditor with no host stream",
			call: func(h *harness) (uint64, error) {
				resp, err := h.Client.SelectInEditor(context.Background(),
					connect.NewRequest(&agentreplv1.SelectInEditorRequest{Workspace: ref()}))
				if err != nil {
					return 0, err
				}
				return resp.Msg.GetSuccess().GetSelected().GetRosterRevision(), nil
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Deps.Sidebar.(*fakeSidebar).revision = 7
			record := h.DB.workspaces[testWorkspaceID]
			record.MergeOf = &wsm.MergeOrigin{Requester: "ws-requester", Request: "r-1"}
			h.DB.workspaces[testWorkspaceID] = record

			// Act.
			revision, err := tt.call(h)

			// Assert.
			if err != nil {
				t.Fatalf("%s: %v", tt.name, err)
			}
			if revision != 7 {
				t.Fatalf("roster_revision = %d, want the sidebar's 7", revision)
			}
			if len(h.Merge.reveals) != 1 || h.Merge.reveals[0] != testWorkspaceID {
				t.Fatalf("reveals = %v, want the merge workspace revealed", h.Merge.reveals)
			}
			if got := h.Banners.forgotten(); len(got) != 1 || got[0] != "a workspace was selected" {
				t.Fatalf("forgets = %q, want the selection to let every banner go", got)
			}
		})
	}
}
