package server

import (
	"context"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"connectrpc.com/connect"

	"agent-repld/internal/merge"
	"agent-repld/internal/wsm"
)

// productionDispatch is a decision dispatching one remediation child.
func productionDispatch() *agentreplv1.UpdateProductionMediationRequest {
	return &agentreplv1.UpdateProductionMediationRequest{Mediator: ref(), Action: &agentreplv1.UpdateProductionMediationRequest_Decide{Decide: &agentreplv1.ProductionMediationDecision{
		Summary: "one problem",
		Decision: &agentreplv1.ProductionMediationDecision_Dispatch{Dispatch: &agentreplv1.ProductionMediationDispatch{Children: []*agentreplv1.ProductionChild{{
			Work: &agentreplv1.ProductionChild_Remediation{Remediation: &agentreplv1.ProductionChildRemediation{}},
			Findings: []*agentreplv1.ProductionFinding{{
				Statement: "the tail lags", Sites: []*agentreplv1.LogSiteKey{{Hex: "0123456789ab"}}, Evidence: []string{"tail lag 9s"},
			}},
		}}}},
	}}}
}

func TestUpdateProductionMediationAnswersEachDecision(t *testing.T) {
	nothing := &agentreplv1.UpdateProductionMediationRequest{Mediator: ref(), Action: &agentreplv1.UpdateProductionMediationRequest_Decide{Decide: &agentreplv1.ProductionMediationDecision{
		Summary: "quiet", Decision: &agentreplv1.ProductionMediationDecision_NothingToDo{NothingToDo: &agentreplv1.ProductionMediationNothingToDo{}},
	}}}
	tests := []struct {
		name string
		req  *agentreplv1.UpdateProductionMediationRequest
		want func(*agentreplv1.UpdateProductionMediationSuccess) bool
		// wantChildren is how many children the orchestrator was asked for.
		wantChildren int
	}{
		{name: "nothing to do", req: nothing, want: func(s *agentreplv1.UpdateProductionMediationSuccess) bool { return s.GetDecidedNothing() != nil }},
		{name: "a dispatch answers its children", req: productionDispatch(), wantChildren: 1,
			want: func(s *agentreplv1.UpdateProductionMediationSuccess) bool {
				return len(s.GetDispatched().GetChildren()) == 1 && s.GetDispatched().GetChildren()[0].GetId() == "child-1"
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.Merge.productionMade = []wsm.Workspace{{ID: "child-1", Dir: "/w/child-1"}}

			// Act
			resp, err := h.Client.UpdateProductionMediation(context.Background(), connect.NewRequest(tt.req))

			// Assert
			if err != nil || !tt.want(resp.Msg.GetSuccess()) {
				t.Fatalf("UpdateProductionMediation = (%v, %v)", resp, err)
			}
			if len(h.Merge.productionDecisions) != 1 || len(h.Merge.productionDecisions[0].Children) != tt.wantChildren {
				t.Fatalf("decisions = %+v, want one with %d children", h.Merge.productionDecisions, tt.wantChildren)
			}
		})
	}
}

func TestUpdateProductionMediationCarriesTheDispatchToTheOrchestrator(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	if _, err := h.Client.UpdateProductionMediation(context.Background(), connect.NewRequest(productionDispatch())); err != nil {
		t.Fatal(err)
	}

	// Assert
	child := h.Merge.productionDecisions[0].Children[0]
	if child.Kind != wsm.ProductionRemediation || len(child.Findings) != 1 || child.Findings[0].Sites[0] != "0123456789ab" || child.Findings[0].Evidence[0] != "tail lag 9s" {
		t.Fatalf("child = %+v, want the remediation with its finding, site and evidence", child)
	}
}

func TestUpdateProductionMediationAnswersTheOrchestratorsRefusalsOnTheirArms(t *testing.T) {
	tests := []struct {
		name string
		err  error
		got  func(*agentreplv1.UpdateProductionMediationError) bool
	}{
		{name: "not the production mediator", err: &merge.RefusalError{Arm: merge.ArmNotProductionMediator, Reason: "no"},
			got: func(e *agentreplv1.UpdateProductionMediationError) bool { return e.GetNotProductionMediator() != nil }},
		{name: "no check owed", err: &merge.RefusalError{Arm: merge.ArmNoProductionCheckOwed, Reason: "no"},
			got: func(e *agentreplv1.UpdateProductionMediationError) bool { return e.GetNoCheckOwed() != nil }},
		{name: "a site an open child owns", err: &merge.RefusalError{Arm: merge.ArmSiteOwned, Reason: "owned", Fields: map[string]any{"site": "0123456789ab", "holder": "child-9"}},
			got: func(e *agentreplv1.UpdateProductionMediationError) bool {
				return e.GetSiteOwned().GetSite().GetHex() == "0123456789ab" && e.GetSiteOwned().GetOpenChild().GetId() == "child-9"
			}},
		{name: "a site two children name", err: &merge.RefusalError{Arm: merge.ArmSiteOwned, Reason: "twice", Fields: map[string]any{"site": "0123456789ab", "index": 1}},
			got: func(e *agentreplv1.UpdateProductionMediationError) bool {
				return e.GetSiteOwned().GetRepeatedInChild() == 1
			}},
		{name: "a child refused", err: &merge.RefusalError{Arm: merge.ArmChildRefused, Reason: "the name is taken", Fields: map[string]any{"child": 0}},
			got: func(e *agentreplv1.UpdateProductionMediationError) bool {
				return e.GetChildRefused().GetDetail() == "the name is taken" && e.GetChildRefused().GetChild() == 0
			}},
		{name: "paused production checks", err: &merge.RefusalError{Arm: merge.ArmProductionPaused, Reason: "paused"},
			got: func(e *agentreplv1.UpdateProductionMediationError) bool { return e.GetPaused() != nil }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.Merge.productionErr = tt.err

			// Act
			resp, err := h.Client.UpdateProductionMediation(context.Background(), connect.NewRequest(productionDispatch()))

			// Assert
			if err != nil || !tt.got(resp.Msg.GetError()) {
				t.Fatalf("UpdateProductionMediation = (%v, %v), want the arm", resp, err)
			}
		})
	}
}

func TestUpdateProductionMediationFoldsOneChildIntoAnother(t *testing.T) {
	// Arrange
	h := newHarness(t)
	req := &agentreplv1.UpdateProductionMediationRequest{Mediator: ref(), Action: &agentreplv1.UpdateProductionMediationRequest_Fold{Fold: &agentreplv1.ProductionMediationFold{Folded: ref(), Surviving: ref()}}}

	// Act
	resp, err := h.Client.UpdateProductionMediation(context.Background(), connect.NewRequest(req))

	// Assert
	if err != nil || resp.Msg.GetSuccess().GetFolded() == nil || len(h.Merge.folds) != 1 {
		t.Fatalf("UpdateProductionMediation = (%v, %v), folds %v; want the fold done", resp, err, h.Merge.folds)
	}
}

func TestUpdateProductionMediationRefusesAMalformedRequestAsInvalid(t *testing.T) {
	blank := productionDispatch()
	blank.GetDecide().Summary = " "
	noEvidence := productionDispatch()
	noEvidence.GetDecide().GetDispatch().GetChildren()[0].GetFindings()[0].Evidence = nil
	badSite := productionDispatch()
	badSite.GetDecide().GetDispatch().GetChildren()[0].GetFindings()[0].Sites = []*agentreplv1.LogSiteKey{{Hex: "XYZ"}}
	noKind := productionDispatch()
	noKind.GetDecide().GetDispatch().GetChildren()[0].Work = nil
	noChildren := productionDispatch()
	noChildren.GetDecide().GetDispatch().Children = nil
	noAction := &agentreplv1.UpdateProductionMediationRequest{Mediator: ref()}
	noMediator := productionDispatch()
	noMediator.Mediator = &workspacev1.WorkspaceRef{}
	tests := []struct {
		name string
		req  *agentreplv1.UpdateProductionMediationRequest
	}{
		{name: "a blank summary", req: blank},
		{name: "a finding with no evidence", req: noEvidence},
		{name: "a malformed site key", req: badSite},
		{name: "a child with no kind", req: noKind},
		{name: "a dispatch with no child", req: noChildren},
		{name: "no action", req: noAction},
		{name: "no mediator", req: noMediator},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			_, err := h.Client.UpdateProductionMediation(context.Background(), connect.NewRequest(tt.req))

			// Assert
			if connect.CodeOf(err) != connect.CodeInvalidArgument || len(h.Merge.productionDecisions) != 0 {
				t.Fatalf("err = %v, decisions %d; want InvalidArgument and nothing asked", err, len(h.Merge.productionDecisions))
			}
		})
	}
}
