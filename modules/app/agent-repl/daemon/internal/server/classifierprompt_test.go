package server

import (
	"context"
	"errors"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/classifier"
	"claude-repld/internal/classifierupdate"
	"claude-repld/internal/dlog"
)

func classifierPromptRequest() *agentreplv1.UpdateClassifierPromptRequest {
	return &agentreplv1.UpdateClassifierPromptRequest{
		Instruction: "always interrupt for an 'after' ordering",
		Example: &agentreplv1.ClassifiedPromptExample{
			Text:  "after the tests pass, bump the version",
			Route: agentreplv1.ClassifierRoute_CLASSIFIER_ROUTE_HOLD_FOR_TURN_END,
		},
	}
}

func TestUpdateClassifierPromptHandsTheRequestToTheUpdater(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	if _, err := h.Client.UpdateClassifierPrompt(context.Background(), connect.NewRequest(classifierPromptRequest())); err != nil {
		t.Fatalf("UpdateClassifierPrompt: %v", err)
	}

	// Assert.
	want := classifierupdate.Request{
		Instruction: "always interrupt for an 'after' ordering",
		Example:     classifierupdate.Example{Text: "after the tests pass, bump the version", Route: classifier.RouteQueue},
	}
	if len(h.ClassifierPrompt.updates) != 1 || h.ClassifierPrompt.updates[0] != want {
		t.Fatalf("updates = %+v, want exactly %+v", h.ClassifierPrompt.updates, want)
	}
}

func TestUpdateClassifierPromptAnswersTheCommit(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ClassifierPrompt.result = classifierupdate.Result{Commit: "aaaa", Path: "/p/queue-routing-classifier.md"}

	// Act.
	resp, err := h.Client.UpdateClassifierPrompt(context.Background(), connect.NewRequest(classifierPromptRequest()))

	// Assert.
	if err != nil {
		t.Fatalf("UpdateClassifierPrompt: %v", err)
	}
	success := resp.Msg.GetSuccess()
	if success.GetCommit() != "aaaa" || success.GetPath() != "/p/queue-routing-classifier.md" {
		t.Fatalf("UpdateClassifierPrompt = %v, want success naming the commit and the path", resp.Msg)
	}
}

func TestUpdateClassifierPromptMapsEveryRefusal(t *testing.T) {
	// Arrange.
	tests := []struct {
		arm   string
		armOf func(*agentreplv1.UpdateClassifierPromptError) bool
	}{
		{classifierupdate.ArmInProgress, func(e *agentreplv1.UpdateClassifierPromptError) bool { return e.GetInProgress() != nil }},
		{classifierupdate.ArmRewriteFailed, func(e *agentreplv1.UpdateClassifierPromptError) bool {
			return e.GetRewriteFailed().GetDetail() == "why"
		}},
		{classifierupdate.ArmRewriteRejected, func(e *agentreplv1.UpdateClassifierPromptError) bool {
			return e.GetRewriteRejected().GetDetail() == "why"
		}},
		{classifierupdate.ArmUnchanged, func(e *agentreplv1.UpdateClassifierPromptError) bool { return e.GetUnchanged() != nil }},
		{classifierupdate.ArmChangedDuringRewrite, func(e *agentreplv1.UpdateClassifierPromptError) bool {
			return e.GetChangedDuringRewrite() != nil
		}},
		{classifierupdate.ArmCommitFailed, func(e *agentreplv1.UpdateClassifierPromptError) bool {
			return e.GetCommitFailed().GetDetail() == "why"
		}},
	}
	for _, tt := range tests {
		t.Run(tt.arm, func(t *testing.T) {
			h := newHarness(t)
			h.ClassifierPrompt.err = &classifierupdate.Refusal{Arm: tt.arm, Reason: "why"}

			// Act.
			resp, err := h.Client.UpdateClassifierPrompt(context.Background(), connect.NewRequest(classifierPromptRequest()))

			// Assert.
			if err != nil {
				t.Fatalf("UpdateClassifierPrompt: %v", err)
			}
			if !tt.armOf(resp.Msg.GetError()) {
				t.Fatalf("UpdateClassifierPrompt = %v, want the %s arm", resp.Msg, tt.arm)
			}
		})
	}
}

func TestUpdateClassifierPromptNamesTheDirtyBrief(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ClassifierPrompt.err = &classifierupdate.Refusal{
		Arm: classifierupdate.ArmUncommittedChanges, Reason: "dirty",
		Fields: map[string]any{"path": "/p/queue-routing-classifier.md"},
	}

	// Act.
	resp, err := h.Client.UpdateClassifierPrompt(context.Background(), connect.NewRequest(classifierPromptRequest()))

	// Assert.
	if err != nil {
		t.Fatalf("UpdateClassifierPrompt: %v", err)
	}
	if got := resp.Msg.GetError().GetUncommittedChanges().GetPath(); got != "/p/queue-routing-classifier.md" {
		t.Fatalf("uncommitted_changes.path = %q, want the brief's path", got)
	}
}

func TestUpdateClassifierPromptSurfacesAnOrdinaryFailureAsAnError(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	h := newHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log} })
	h.ClassifierPrompt.err = errors.New("read the brief: permission denied")

	// Act.
	_, err := h.Client.UpdateClassifierPrompt(context.Background(), connect.NewRequest(classifierPromptRequest()))

	// Assert.
	if code := connectCode(t, err); code != connect.CodeInternal {
		t.Fatalf("code = %v, want Internal", code)
	}
	for _, record := range log.Records() {
		if record.Level == "error" && record.Operation == "UpdateClassifierPrompt" && record.Context["cause"] == "read the brief: permission denied" {
			return
		}
	}
	t.Fatalf("no error record named the failure: %+v", log.Records())
}

func TestRouteOfReadsEveryWireRoute(t *testing.T) {
	// Arrange.
	tests := []struct {
		wire agentreplv1.ClassifierRoute
		want classifier.Route
	}{
		{agentreplv1.ClassifierRoute_CLASSIFIER_ROUTE_INTERRUPT, classifier.RouteInterrupt},
		{agentreplv1.ClassifierRoute_CLASSIFIER_ROUTE_AFTER_TOOL_CALL, classifier.RouteAfterToolCall},
		{agentreplv1.ClassifierRoute_CLASSIFIER_ROUTE_HOLD_FOR_TURN_END, classifier.RouteQueue},
	}
	for _, tt := range tests {
		t.Run(tt.wire.String(), func(t *testing.T) {
			// Act, Assert.
			if got := routeOf(tt.wire); got != tt.want {
				t.Fatalf("routeOf(%v) = %v, want %v", tt.wire, got, tt.want)
			}
		})
	}
}

func TestRouteOfPanicsOnUnspecified(t *testing.T) {
	// Arrange.
	defer func() {
		if recover() == nil {
			t.Fatalf("routeOf(UNSPECIFIED) did not panic")
		}
	}()

	// Act.
	routeOf(agentreplv1.ClassifierRoute_CLASSIFIER_ROUTE_UNSPECIFIED)
}
