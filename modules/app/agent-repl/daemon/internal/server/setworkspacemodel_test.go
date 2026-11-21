package server

import (
	"context"
	"errors"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/workspace"
	"agent-repld/internal/wsm"
)

const setModelOp = "daemon.server.set_workspace_model"

// setModelHarness is a harness whose workspace belongs to a registered
// repository whose login serves the catalog opus and sonnet, and whose
// compaction submits as turn "t-compact".
func setModelHarness(t *testing.T) (*harness, *dlog.TestLogger) {
	t.Helper()
	h := newHarness(t)
	repo := registeredRepoRef(h)
	record := h.DB.workspaces[testWorkspaceID]
	record.Repo = wsm.RepoID(repo.GetId())
	h.DB.workspaces[testWorkspaceID] = record
	h.Verbs.creationCatalog = workspace.CreationCatalog{ConfigDir: "/home/.claude", Known: true,
		Options: []*conversationv1.ModelOption{modelOption("opus"), modelOption("sonnet")}}
	h.Prompts.outcome.Turn = "t-compact"
	log := dlog.NewTestLogger()
	h.Surfaces.workspace = log
	return h, log
}

func setWorkspaceModel(h *harness, model string) (*connect.Response[agentreplv1.SetWorkspaceModelResponse], error) {
	return h.Client.SetWorkspaceModel(context.Background(), connect.NewRequest(&agentreplv1.SetWorkspaceModelRequest{
		Workspace: ref(), Requester: workspaceRequester(ref()), Model: &conversationv1.AgentModel{Name: model}}))
}

func TestSetWorkspaceModelSetsTheModelOnlyAfterTheCompactionFinished(t *testing.T) {
	// Arrange
	h, log := setModelHarness(t)
	h.Turns.entered = make(chan struct{})
	h.Turns.release = make(chan struct{})
	type answer struct {
		resp *connect.Response[agentreplv1.SetWorkspaceModelResponse]
		err  error
	}
	answered := make(chan answer, 1)

	// Act
	go func() {
		resp, err := setWorkspaceModel(h, "sonnet")
		answered <- answer{resp, err}
	}()
	<-h.Turns.entered

	// Assert: while the compaction's end is awaited, it is submitted and the model is not yet set.
	text := h.Prompts.lastSaid.GetContent().GetBlocks()[0].GetText().GetText()
	if text != "/compact" || h.Prompts.lastOrigin != conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_COMPACT || h.Prompts.lastDelivery != wsm.DeliveryDeferred {
		t.Fatalf("submitted %q as %s %v; want a deferred workspace compaction", text, h.Prompts.lastOrigin, h.Prompts.lastDelivery)
	}
	if h.Verbs.setModel != "" {
		t.Fatalf("the model was set to %q while the compaction was still running", h.Verbs.setModel)
	}
	close(h.Turns.release)
	got := <-answered
	if got.err != nil || got.resp.Msg.GetSuccess() == nil {
		t.Fatalf("SetWorkspaceModel = %v, %v; want success", got.resp, got.err)
	}
	if h.Verbs.setModel != "sonnet" || len(h.Turns.awaited) != 1 || h.Turns.awaited[0] != "t-compact" {
		t.Fatalf("set %q after awaiting %v; want sonnet after the compaction turn", h.Verbs.setModel, h.Turns.awaited)
	}
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Operation: setModelOp}); !ok {
		t.Fatalf("records = %+v, want the set recorded", log.Records())
	}
}

func TestSetWorkspaceModelIsRefused(t *testing.T) {
	tests := []struct {
		name      string
		model     string
		arrange   func(h *harness)
		got       func(*agentreplv1.SetWorkspaceModelError) bool
		wantLevel string
		wantArm   string
		// untouched says nothing was compacted.
		untouched bool
	}{
		{name: "a model outside the catalog", model: "gpt", untouched: true, wantLevel: "info", wantArm: "model_not_in_catalog",
			got: func(e *agentreplv1.SetWorkspaceModelError) bool {
				n := e.GetModelNotInCatalog()
				return n.GetModel() == "gpt" && n.GetConfigDir() == "/home/.claude" && len(n.GetOptions()) == 2
			}},
		{name: "a login that served no catalog", model: "opus", untouched: true, wantLevel: "info", wantArm: "model_catalog_unknown",
			arrange: func(h *harness) { h.Verbs.creationCatalog = workspace.CreationCatalog{ConfigDir: "/home/.claude"} },
			got:     func(e *agentreplv1.SetWorkspaceModelError) bool { return e.GetModelCatalogUnknown() != nil }},
		{name: "a closed workspace", model: "opus", untouched: true, wantLevel: "info", wantArm: "closed",
			arrange: func(h *harness) {
				ws := h.DB.workspaces[testWorkspaceID]
				ws.Closed = true
				h.DB.workspaces[testWorkspaceID] = ws
			},
			got: func(e *agentreplv1.SetWorkspaceModelError) bool { return e.GetClosed() != nil }},
		{name: "a session at its cold gate", model: "opus", wantLevel: "debug", wantArm: "cold_gate",
			arrange: func(h *harness) { h.Prompts.err = &workspace.Refusal{Arm: "cold_gate", Reason: "cold"} },
			got:     func(e *agentreplv1.SetWorkspaceModelError) bool { return e.GetColdGate() != nil }},
		{name: "a compaction that failed", model: "opus", wantLevel: "error", wantArm: "compaction_failed",
			arrange: func(h *harness) { h.Turns.closes = wsm.CloseFailed },
			got:     func(e *agentreplv1.SetWorkspaceModelError) bool { return e.GetCompactionFailed().GetHow() == "failed" }},
		{name: "a compaction the user interrupted", model: "opus", wantLevel: "error", wantArm: "compaction_failed",
			arrange: func(h *harness) { h.Turns.closes = wsm.CloseKilled },
			got:     func(e *agentreplv1.SetWorkspaceModelError) bool { return e.GetCompactionFailed().GetHow() == "killed" }},
		{name: "a vendor refusing the change", model: "opus", wantLevel: "error", wantArm: "vendor_refused",
			arrange: func(h *harness) {
				h.Verbs.setModelErr = &workspace.ShimRefusal{Arm: workspace.ArmShimVendorRefused, Detail: "no"}
			},
			got: func(e *agentreplv1.SetWorkspaceModelError) bool { return e.GetVendorRefused().GetDetail() == "no" }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h, log := setModelHarness(t)
			if tt.arrange != nil {
				tt.arrange(h)
			}

			// Act
			resp, err := setWorkspaceModel(h, tt.model)

			// Assert
			if err != nil || !tt.got(resp.Msg.GetError()) {
				t.Fatalf("SetWorkspaceModel = %v, %v; want the %s refusal", resp, err, tt.wantArm)
			}
			if tt.untouched && h.Prompts.submits != 0 {
				t.Fatalf("%d compactions were submitted, want none", h.Prompts.submits)
			}
			if tt.wantArm != "vendor_refused" && h.Verbs.setModel != "" {
				t.Fatalf("the model was set to %q, want it unchanged", h.Verbs.setModel)
			}
			query := dlog.RecordQuery{Level: tt.wantLevel, Operation: setModelOp}
			if tt.wantLevel != "error" {
				query.Message = "answered a typed refusal"
			}
			record, ok := dlog.FindRecord(log.Records(), query)
			if !ok || record.Context["model"] != tt.model || record.Context["workspace"] != string(testWorkspaceID) {
				t.Fatalf("records = %+v, want a %s record under %s naming the workspace and the model", log.Records(), tt.wantLevel, setModelOp)
			}
		})
	}
}

func TestSetWorkspaceModelFailsLoudlyWhenItCannotAwaitTheCompaction(t *testing.T) {
	// Arrange
	h, log := setModelHarness(t)
	h.Turns.err = errors.New("no live session")

	// Act
	_, err := setWorkspaceModel(h, "opus")

	// Assert
	if connectCode(t, err) != connect.CodeInternal || h.Verbs.setModel != "" {
		t.Fatalf("err = %v, set %q; want an internal error and the model unchanged", err, h.Verbs.setModel)
	}
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: setModelOp}); !ok {
		t.Fatalf("records = %+v, want the failure recorded", log.Records())
	}
}

func TestSetWorkspaceModelRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange
	h, _ := setModelHarness(t)
	missing := ref()
	missing.Id = "ws-missing"

	// Act
	resp, err := h.Client.SetWorkspaceModel(context.Background(), connect.NewRequest(&agentreplv1.SetWorkspaceModelRequest{
		Workspace: missing, Requester: userRequester(), Model: &conversationv1.AgentModel{Name: "opus"}}))

	// Assert
	if err != nil || resp.Msg.GetError().GetUnknownWorkspace() == nil || h.Prompts.submits != 0 {
		t.Fatalf("SetWorkspaceModel = %v, %v, submits %d; want unknown_workspace and no compaction", resp, err, h.Prompts.submits)
	}
}

func TestSetWorkspaceModelRejectsAMalformedRequest(t *testing.T) {
	tests := []struct {
		name string
		edit func(*agentreplv1.SetWorkspaceModelRequest)
	}{
		{name: "no model", edit: func(r *agentreplv1.SetWorkspaceModelRequest) { r.Model = nil }},
		{name: "a blank model name", edit: func(r *agentreplv1.SetWorkspaceModelRequest) { r.Model = &conversationv1.AgentModel{} }},
		{name: "no requester", edit: func(r *agentreplv1.SetWorkspaceModelRequest) { r.Requester = nil }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h, _ := setModelHarness(t)
			req := &agentreplv1.SetWorkspaceModelRequest{Workspace: ref(), Requester: userRequester(), Model: &conversationv1.AgentModel{Name: "opus"}}
			tt.edit(req)

			// Act
			_, err := h.Client.SetWorkspaceModel(context.Background(), connect.NewRequest(req))

			// Assert
			if connectCode(t, err) != connect.CodeInvalidArgument || h.Prompts.submits != 0 {
				t.Fatalf("err = %v, submits %d; want invalid_argument and no compaction", err, h.Prompts.submits)
			}
		})
	}
}
