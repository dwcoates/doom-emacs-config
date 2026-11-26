package server

import (
	"context"
	"errors"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/merge"
)

// productionPauseRequest asks UpdateProductionPause to pause, or (resume) to
// resume, theHeldRepository.
func productionPauseRequest(resume bool) *connect.Request[agentreplv1.UpdateProductionPauseRequest] {
	req := &agentreplv1.UpdateProductionPauseRequest{Repository: theHeldRepository,
		Action: &agentreplv1.UpdateProductionPauseRequest_Pause{Pause: &agentreplv1.UpdateProductionPausePause{}}}
	if resume {
		req.Action = &agentreplv1.UpdateProductionPauseRequest_Resume{Resume: &agentreplv1.UpdateProductionPauseResume{}}
	}
	return connect.NewRequest(req)
}

// theProductionScope is theHeldRepository as the orchestrator is asked it.
var theProductionScope = merge.RepositoryScope{ID: "repo-1", Dir: "/repos/one/.git"}

func TestAProductionPauseAnswersWhatItStoppedAndRestatesTheChip(t *testing.T) {
	cases := []struct {
		name   string
		paused merge.ProductionPaused
		check  func(*agentreplv1.UpdateProductionPauseSuccessPaused) bool
	}{
		{name: "a pause that interrupted the mediator names it", paused: merge.ProductionPaused{Interrupted: "ws-mediator", Dropped: 2},
			check: func(p *agentreplv1.UpdateProductionPauseSuccessPaused) bool {
				return p.GetInterrupted().GetId() == "ws-mediator" && p.GetDroppedSpans() == 2
			}},
		{name: "a pause that interrupted nothing leaves it unset", paused: merge.ProductionPaused{},
			check: func(p *agentreplv1.UpdateProductionPauseSuccessPaused) bool {
				return p.Interrupted == nil && p.GetDroppedSpans() == 0
			}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.Merge.productionPaused = tc.paused

			// Act
			resp, err := h.Client.UpdateProductionPause(context.Background(), productionPauseRequest(false))

			// Assert
			if err != nil || !tc.check(resp.Msg.GetSuccess().GetPaused()) {
				t.Fatalf("UpdateProductionPause = %v, %v; want the pause's own account", resp, err)
			}
			if len(h.Merge.productionPauseScopes) != 1 || h.Merge.productionPauseScopes[0] != theProductionScope {
				t.Fatalf("paused %v, want one pause of %v", h.Merge.productionPauseScopes, theProductionScope)
			}
			if len(h.Verbs.restatedProduction) != 1 || h.Verbs.restatedProduction[0] != theProductionScope {
				t.Fatalf("restated %v, want the chip restated for %v", h.Verbs.restatedProduction, theProductionScope)
			}
		})
	}
}

func TestAProductionResumeAnswersResumedAndRestatesTheChip(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	resp, err := h.Client.UpdateProductionPause(context.Background(), productionPauseRequest(true))

	// Assert
	if err != nil || resp.Msg.GetSuccess().GetResumed() == nil || len(h.Merge.productionResumeScopes) != 1 || len(h.Verbs.restatedProduction) != 1 {
		t.Fatalf("UpdateProductionPause = %v, %v, resumes %v, restates %v; want one resume and one restate", resp, err, h.Merge.productionResumeScopes, h.Verbs.restatedProduction)
	}
}

func TestTheProductionPauseRefusalsAreAnswersAndRestateNothing(t *testing.T) {
	cases := []struct {
		name   string
		resume bool
		arm    string
		got    func(*agentreplv1.UpdateProductionPauseError) bool
	}{
		{name: "a pause of paused checks", arm: merge.ArmAlreadyPaused,
			got: func(e *agentreplv1.UpdateProductionPauseError) bool { return e.GetAlreadyPaused() != nil }},
		{name: "a resume of running checks", resume: true, arm: merge.ArmNotPaused,
			got: func(e *agentreplv1.UpdateProductionPauseError) bool { return e.GetNotPaused() != nil }},
		{name: "an unknown repository", arm: merge.ArmUnknownRepository,
			got: func(e *agentreplv1.UpdateProductionPauseError) bool { return e.GetUnknownRepository() != nil }},
		{name: "a repository that checks no production", resume: true, arm: merge.ArmProductionChecksOff,
			got: func(e *agentreplv1.UpdateProductionPauseError) bool { return e.GetChecksOff() != nil }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.Merge.productionPauseErr = &merge.RefusalError{Arm: tc.arm, Reason: "nothing to change"}

			// Act
			resp, err := h.Client.UpdateProductionPause(context.Background(), productionPauseRequest(tc.resume))

			// Assert
			if err != nil || !tc.got(resp.Msg.GetError()) || len(h.Verbs.restatedProduction) != 0 {
				t.Fatalf("UpdateProductionPause = %v, %v, restated %v; want the %s arm and no restate", resp, err, h.Verbs.restatedProduction, tc.arm)
			}
		})
	}
}

func TestAProductionPauseThatFailsIsATransportErrorAndStillRestatesTheChip(t *testing.T) {
	// Arrange: the pause stood, but its interrupt failed.
	h := newHarness(t)
	h.Merge.productionPauseErr = errors.New("the interrupt failed")

	// Act
	_, err := h.Client.UpdateProductionPause(context.Background(), productionPauseRequest(false))

	// Assert
	if err == nil || len(h.Verbs.restatedProduction) != 1 {
		t.Fatalf("UpdateProductionPause = %v, restated %v; want an error and the chip restated", err, h.Verbs.restatedProduction)
	}
}

func TestUpdateProductionPauseRefusesAnIncompleteRequest(t *testing.T) {
	cases := []struct {
		name string
		req  *agentreplv1.UpdateProductionPauseRequest
	}{
		{name: "no repository", req: &agentreplv1.UpdateProductionPauseRequest{Action: &agentreplv1.UpdateProductionPauseRequest_Pause{Pause: &agentreplv1.UpdateProductionPausePause{}}}},
		{name: "no action", req: &agentreplv1.UpdateProductionPauseRequest{Repository: theHeldRepository}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			_, err := h.Client.UpdateProductionPause(context.Background(), connect.NewRequest(tc.req))

			// Assert
			if connect.CodeOf(err) != connect.CodeInvalidArgument || len(h.Merge.productionPauseScopes)+len(h.Merge.productionResumeScopes) != 0 {
				t.Fatalf("UpdateProductionPause = %v, want InvalidArgument and nothing asked", err)
			}
		})
	}
}
