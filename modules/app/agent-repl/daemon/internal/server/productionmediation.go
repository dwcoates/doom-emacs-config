package server

import (
	"context"
	"fmt"
	"strings"

	"agentrepl/logging/logsite"
	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"connectrpc.com/connect"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/merge"
	"agent-repld/internal/wsm"
)

func (s *requestLoggingServer) UpdateProductionMediation(
	ctx context.Context,
	req *connect.Request[agentreplv1.UpdateProductionMediationRequest],
) (*connect.Response[agentreplv1.UpdateProductionMediationResponse], error) {
	return serveUnary(s, ctx, "UpdateProductionMediation", req, s.server.UpdateProductionMediation)
}

// UpdateProductionMediation is the production mediator's decisions
// (endpoint_update_production_mediation.proto): a decision on the span it was
// told, or a fold of one child into another (merge.DecideProduction,
// merge.FoldProduction).
func (s *server) UpdateProductionMediation(
	ctx context.Context,
	req *connect.Request[agentreplv1.UpdateProductionMediationRequest],
) (*connect.Response[agentreplv1.UpdateProductionMediationResponse], error) {
	const rpc = "UpdateProductionMediation"
	if err := validateUpdateProductionMediationRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.UpdateProductionMediationResponse{}
	mediator, cerr, done := s.subjectForField(ctx, rpc, "mediator", req.Msg.GetMediator(), resp)
	if done {
		return answer(resp, cerr)
	}
	success := &agentreplv1.UpdateProductionMediationSuccess{}
	switch action := req.Msg.GetAction().(type) {
	case *agentreplv1.UpdateProductionMediationRequest_Decide:
		made, err := s.deps.Merge.DecideProduction(ctx, mediator.Record.ID, productionDecisionOf(action.Decide))
		if err != nil {
			return s.productionRefusal(mediator.Log, rpc, resp, err)
		}
		if action.Decide.GetDispatch() == nil {
			success.Outcome = &agentreplv1.UpdateProductionMediationSuccess_DecidedNothing{DecidedNothing: &agentreplv1.ProductionMediationDecidedNothing{}}
			break
		}
		dispatched := &agentreplv1.ProductionMediationDispatched{}
		for _, child := range made {
			dispatched.Children = append(dispatched.Children, child.Ref())
		}
		success.Outcome = &agentreplv1.UpdateProductionMediationSuccess_Dispatched{Dispatched: dispatched}
	case *agentreplv1.UpdateProductionMediationRequest_Fold:
		folded, cerr, done := s.subjectForField(ctx, rpc, "fold.folded", action.Fold.GetFolded(), resp)
		if done {
			return answer(resp, cerr)
		}
		surviving, cerr, done := s.subjectForField(ctx, rpc, "fold.surviving", action.Fold.GetSurviving(), resp)
		if done {
			return answer(resp, cerr)
		}
		if err := s.deps.Merge.FoldProduction(ctx, mediator.Record.ID, folded.Record.ID, surviving.Record.ID); err != nil {
			return s.productionRefusal(mediator.Log, rpc, resp, err)
		}
		success.Outcome = &agentreplv1.UpdateProductionMediationSuccess_Folded{Folded: &agentreplv1.ProductionMediationFolded{}}
	default:
		panic(fmt.Sprintf("server: UpdateProductionMediation action %T past validation", action))
	}
	resp.Result = &agentreplv1.UpdateProductionMediationResponse_Success{Success: success}
	return connect.NewResponse(resp), nil
}

// productionDecisionOf reads a decision into the orchestrator's request.
func productionDecisionOf(d *agentreplv1.ProductionMediationDecision) merge.ProductionDecisionRequest {
	out := merge.ProductionDecisionRequest{Summary: d.GetSummary()}
	for _, child := range d.GetDispatch().GetChildren() {
		kind := wsm.ProductionRemediation
		if child.GetInvestigation() != nil {
			kind = wsm.ProductionInvestigation
		}
		req := merge.ProductionChildRequest{Name: child.GetName(), Kind: kind}
		for _, f := range child.GetFindings() {
			finding := merge.ProductionFinding{Statement: f.GetStatement(), Evidence: f.GetEvidence()}
			for _, site := range f.GetSites() {
				finding.Sites = append(finding.Sites, site.GetHex())
			}
			req.Findings = append(req.Findings, finding)
		}
		out.Children = append(out.Children, req)
	}
	return out
}

// productionRefusal answers ERR: a refusal the orchestrator named, onto its
// arm; any other failure as Internal, recorded at ERROR.
func (s *server) productionRefusal(log dlog.Logger, rpc string, resp *agentreplv1.UpdateProductionMediationResponse, err error) (*connect.Response[agentreplv1.UpdateProductionMediationResponse], error) {
	refused, ok := merge.Refused(err)
	if !ok {
		return nil, fail(log, rpc, err)
	}
	e := &agentreplv1.UpdateProductionMediationError{}
	switch refused.Arm {
	case merge.ArmNotProductionMediator:
		e.Cause = &agentreplv1.UpdateProductionMediationError_NotProductionMediator{NotProductionMediator: &agentreplv1.UpdateProductionMediationNotProductionMediator{}}
	case merge.ArmNoProductionCheckOwed:
		e.Cause = &agentreplv1.UpdateProductionMediationError_NoCheckOwed{NoCheckOwed: &agentreplv1.UpdateProductionMediationNoCheckOwed{}}
	case merge.ArmSiteOwned:
		owned := &agentreplv1.UpdateProductionMediationSiteOwned{Site: &agentreplv1.LogSiteKey{Hex: fmt.Sprint(refused.Fields["site"])}}
		if holder, held := refused.Fields["holder"]; held {
			owned.Holder = &agentreplv1.UpdateProductionMediationSiteOwned_OpenChild{OpenChild: s.workspaceRef(fmt.Sprint(holder))}
		} else {
			index, _ := refused.Fields["index"].(int)
			owned.Holder = &agentreplv1.UpdateProductionMediationSiteOwned_RepeatedInChild{RepeatedInChild: uint32(index)}
		}
		e.Cause = &agentreplv1.UpdateProductionMediationError_SiteOwned{SiteOwned: owned}
	case merge.ArmNotFoldable:
		e.Cause = &agentreplv1.UpdateProductionMediationError_NotFoldable{NotFoldable: &agentreplv1.UpdateProductionMediationNotFoldable{
			Workspace: s.workspaceRef(fmt.Sprint(refused.Fields["workspace"])), Detail: refused.Reason}}
	case merge.ArmChildRefused:
		index, _ := refused.Fields["child"].(int)
		e.Cause = &agentreplv1.UpdateProductionMediationError_ChildRefused{ChildRefused: &agentreplv1.UpdateProductionMediationChildRefused{Child: uint32(index), Detail: refused.Reason}}
	default:
		return nil, fail(log, rpc, fmt.Errorf("an orchestrator refusal %q this rpc declares no arm for: %w", refused.Arm, err))
	}
	log.Log(dlog.LevelInfo, opProductionMediation, logsite.Msg("refused a production mediation under {arm}"), dlog.Context{"rpc": rpc, "arm": refused.Arm, "reason": refused.Reason})
	resp.Result = &agentreplv1.UpdateProductionMediationResponse_Error{Error: e}
	return connect.NewResponse(resp), nil
}

// opProductionMediation is the operation the production mediation rpc's
// refusals are recorded under.
const opProductionMediation = "daemon.server.production_mediation"

// workspaceRef is the roster's ref of workspace ID: its id, and its dir when
// the registry still holds it.
func (s *server) workspaceRef(id string) *workspacev1.WorkspaceRef {
	ref := &workspacev1.WorkspaceRef{Id: id}
	if record, _, err := s.registryWorkspace(context.Background(), ids.WorkspaceID(id)); err == nil {
		ref.Dir = record.Dir
	}
	return ref
}

// validateUpdateProductionMediationRequest refuses an unset action, a blank
// summary, a dispatch naming no child, a child naming no kind or no finding,
// a finding with no statement or no evidence, and a malformed site key.
func validateUpdateProductionMediationRequest(req *agentreplv1.UpdateProductionMediationRequest) *connect.Error {
	if err := validateWorkspaceRef("mediator", req.GetMediator()); err != nil {
		return err
	}
	switch action := req.GetAction().(type) {
	case *agentreplv1.UpdateProductionMediationRequest_Decide:
		d := action.Decide
		if strings.TrimSpace(d.GetSummary()) == "" {
			return invalid("decide.summary", "a decision says what the mediator found")
		}
		switch d.GetDecision().(type) {
		case *agentreplv1.ProductionMediationDecision_NothingToDo:
			return nil
		case *agentreplv1.ProductionMediationDecision_Dispatch:
			return validateProductionDispatch(d.GetDispatch())
		}
		return invalid("decide.decision", "a decision is nothing_to_do or dispatch")
	case *agentreplv1.UpdateProductionMediationRequest_Fold:
		if err := validateWorkspaceRef("fold.folded", action.Fold.GetFolded()); err != nil {
			return err
		}
		return validateWorkspaceRef("fold.surviving", action.Fold.GetSurviving())
	}
	return invalid("action", "a production mediation names its action: decide or fold")
}

// validateProductionDispatch refuses a dispatch D that names no child, or a
// child with no kind, no finding, a blank statement, no evidence, or a
// malformed site key.
func validateProductionDispatch(d *agentreplv1.ProductionMediationDispatch) *connect.Error {
	if len(d.GetChildren()) == 0 {
		return invalid("decide.dispatch.children", "a dispatch names at least one child")
	}
	for i, child := range d.GetChildren() {
		field := fmt.Sprintf("decide.dispatch.children[%d]", i)
		if child.GetWork() == nil {
			return invalid(field+".work", "a child is a remediation or an investigation")
		}
		if child.Name != nil && strings.TrimSpace(child.GetName()) == "" {
			return invalid(field+".name", "a child's name is left unset to mint one, never set blank")
		}
		if len(child.GetFindings()) == 0 {
			return invalid(field+".findings", "a child takes on at least one finding")
		}
		for j, f := range child.GetFindings() {
			at := fmt.Sprintf("%s.findings[%d]", field, j)
			if strings.TrimSpace(f.GetStatement()) == "" {
				return invalid(at+".statement", "a finding states what is wrong")
			}
			if len(f.GetEvidence()) == 0 {
				return invalid(at+".evidence", "a finding quotes at least one record that shows it")
			}
			for _, site := range f.GetSites() {
				if !logsite.ValidKey(site.GetHex()) {
					return invalid(at+".sites", "a site key is 12 lowercase hex characters")
				}
			}
		}
	}
	return nil
}
