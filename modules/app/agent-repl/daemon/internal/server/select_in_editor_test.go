package server

import (
	"context"
	"slices"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
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

// holdEditor stands one Emacs daemon stream, which a relayed click reaches.
func holdEditor(h *harness) *daemonWatcher {
	return emacsWatcher(h.Server.(*server))
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

func TestSelectInEditorWithAnEditorAnswersRelayed(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	holdEditor(h)

	// Act.
	resp := selectInEditor(t, h)

	// Assert.
	if resp.GetSuccess().GetRelayed() == nil {
		t.Fatalf("result = %v, want success relayed", resp.GetResult())
	}
}

func TestSelectInEditorWithAnEditorPushesSelectRequestedNamingTheWorkspace(t *testing.T) {
	// Arrange: no host stream holds the workspace, as for one the editor
	// does not hold.
	h := newHarness(t)
	w := holdEditor(h)

	// Act.
	selectInEditor(t, h)
	push := <-w.elisp
	push.sent()

	// Assert.
	if got := push.push.GetSelectRequested().GetWorkspace(); got.GetId() != string(testWorkspaceID) {
		t.Fatalf("push = %v, want select_requested naming the workspace", push.push)
	}
}

func TestSelectInEditorWithAnEditorSelectsNothingItself(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	holdEditor(h)

	// Act.
	selectInEditor(t, h)

	// Assert: the editor makes the switch, so the daemon stamps nothing.
	if got := h.Verbs.selects.Load(); got != 0 {
		t.Fatalf("selects = %d, want none on the relayed route", got)
	}
}

func TestSelectInEditorWithAnEditorRevealsNoMergeWorkspace(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	record := h.DB.workspaces[testWorkspaceID]
	record.MergeOf = &wsm.MergeOrigin{Requester: "ws-requester", Request: "r-1"}
	h.DB.workspaces[testWorkspaceID] = record
	holdEditor(h)

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
	holdEditor(h)

	// Act.
	selectInEditor(t, h)

	// Assert.
	for _, rec := range log.Records() {
		if rec.Operation == "SelectInEditor" && rec.Level == "info" &&
			rec.Message == "relayed a row click to the editor, which makes the switch" &&
			rec.Context["editors"] == 1 {
			return
		}
	}
	t.Fatalf("records = %+v, want the relay at info naming one editor", log.Records())
}

func TestSelectInEditorWithNoEditorSelectsTheWorkspace(t *testing.T) {
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

func TestSelectInEditorWithNoEditorRecordsTheDaemonsRoute(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	log := dlog.NewTestLogger()
	h.Surfaces.workspace = log

	// Act.
	selectInEditor(t, h)

	// Assert.
	for _, rec := range log.Records() {
		if rec.Operation == "SelectInEditor" && rec.Level == "info" &&
			rec.Message == "no Emacs daemon stream stands; the daemon selects the workspace" {
			return
		}
	}
	t.Fatalf("records = %+v, want the daemon's route at info", log.Records())
}

func TestSelectInEditorWithNoEditorAnswersTheSelectsRefusal(t *testing.T) {
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
			name: "SelectInEditor with no editor",
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

// selectInEditorRevealing calls SelectInEditor for the harness's workspace,
// naming ROW to reveal ("" names none).
func selectInEditorRevealing(t *testing.T, h *harness, row string) *agentreplv1.SelectInEditorResponse {
	t.Helper()
	req := &agentreplv1.SelectInEditorRequest{Workspace: ref()}
	if row != "" {
		req.Reveal = &frontendv1.FeedId{Value: row}
	}
	resp, err := h.Client.SelectInEditor(context.Background(), connect.NewRequest(req))
	if err != nil {
		t.Fatalf("SelectInEditor: %v", err)
	}
	return resp.Msg
}

func TestSelectInEditorNamingARowRevealsItOnThePage(t *testing.T) {
	tests := []struct {
		name          string
		editor        bool
		attachedFirst bool
		wantRelayed   bool
	}{
		{name: "relayed, to a page that listens", editor: true, attachedFirst: true, wantRelayed: true},
		{name: "relayed, to a page that attaches after the click", editor: true, attachedFirst: false, wantRelayed: true},
		{name: "the daemon's route, to a page that listens", editor: false, attachedFirst: true},
		{name: "the daemon's route, to a page that attaches after the click", editor: false, attachedFirst: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			if tt.editor {
				holdEditor(h)
			}
			var stream *connect.ServerStreamForClient[agentreplv1.WatchWebWorkspaceResponse]
			if tt.attachedFirst {
				stream = openWeb(t, h)
			}

			// Act.
			resp := selectInEditorRevealing(t, h, "row-merge-head")
			if !tt.attachedFirst {
				stream = openWeb(t, h)
			}

			// Assert.
			if got := resp.GetSuccess().GetRelayed() != nil; got != tt.wantRelayed {
				t.Fatalf("result = %v, want relayed %v", resp.GetResult(), tt.wantRelayed)
			}
			if got := receiveWebEvent(t, stream).GetRevealRow().GetRow().GetValue(); got != "row-merge-head" {
				t.Fatalf("revealed %q, want the click's row", got)
			}
		})
	}
}

func TestSelectInEditorRelayedNamingNoRowRevealsNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	holdEditor(h)
	selectInEditorRevealing(t, h, "")
	stream := openWeb(t, h)

	// Act.
	h.Server.PushTransferred(testWorkspaceID, "127.0.0.1:4242")

	// Assert.
	if push := receiveWebEvent(t, stream); push.GetTransferred() == nil {
		t.Fatalf("the page was handed %v, want no reveal", push)
	}
}

func TestSelectInEditorRecordsTheRelaysRevealAtInfo(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	log := dlog.NewTestLogger()
	h.Surfaces.workspace = log
	holdEditor(h)

	// Act.
	selectInEditorRevealing(t, h, "row-merge-head")

	// Assert.
	for _, rec := range log.Records() {
		if rec.Operation == "SelectInEditor" && rec.Level == "info" &&
			rec.Message == "relayed a row click to the editor, which makes the switch" &&
			rec.Context["reveal"] == "row-merge-head" {
			return
		}
	}
	t.Fatalf("records = %+v, want the relay at info naming the reveal", log.Records())
}

func TestSelectInEditorFollowedFromAFeedRecordsTheSourceAndRelaysTheSwitch(t *testing.T) {
	// Arrange: the page's own workspace is the source and the target alike.
	h := newHarness(t)
	w := holdEditor(h)

	// Act.
	_, err := h.Client.SelectInEditor(context.Background(),
		connect.NewRequest(&agentreplv1.SelectInEditorRequest{Workspace: ref(), FollowedFrom: ref()}))
	relayed := <-w.elisp
	relayed.sent()

	// Assert.
	if err != nil {
		t.Fatalf("SelectInEditor: %v", err)
	}
	want := []interactionCall{{ws: testWorkspaceID, kind: workspace.InteractionLinkFollowed}}
	if !slices.Equal(h.Verbs.interactions, want) || relayed.push.GetSelectRequested() == nil {
		t.Fatalf("interactions = %v, push = %v, want the source's followed link recorded and select_requested to the editor", h.Verbs.interactions, relayed.push)
	}
}

func TestSelectInEditorFollowedFromAnUnknownWorkspaceIsRefusedByArm(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	holdEditor(h)

	// Act.
	resp, err := h.Client.SelectInEditor(context.Background(),
		connect.NewRequest(&agentreplv1.SelectInEditorRequest{
			Workspace:    ref(),
			FollowedFrom: &workspacev1.WorkspaceRef{Id: "ws-nope"},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("SelectInEditor: %v", err)
	}
	if resp.Msg.GetError().GetUnknownWorkspace() == nil {
		t.Fatalf("result = %v, want unknown_workspace", resp.Msg.GetResult())
	}
	if got := h.Verbs.selects.Load(); got != 0 {
		t.Fatalf("selects = %d, want none for an unknown source", got)
	}
}

func TestSelectInEditorFollowedFromABlankRefIsInvalidArgument(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.Client.SelectInEditor(context.Background(),
		connect.NewRequest(&agentreplv1.SelectInEditorRequest{Workspace: ref(), FollowedFrom: &workspacev1.WorkspaceRef{}}))

	// Assert.
	if got := connectCode(t, err); got != connect.CodeInvalidArgument {
		t.Fatalf("code = %v, want invalid_argument", got)
	}
}
