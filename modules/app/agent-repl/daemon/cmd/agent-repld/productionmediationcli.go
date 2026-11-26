package main

import (
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"strings"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
)

// productionMediationNoun is the agent API's noun over the production
// mediator's decisions (agentrepl.v1.UpdateProductionMediation). Only the
// production mediator's own agent calls it, from its own worktree.
const productionMediationNoun = "production-mediation"

// productionDecide ends the span the production mediator was told with
// nothing to do.
func productionDecide(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	summary := fs.String("summary", "", "what the span held, in a sentence (required)")
	positionals, ok := c.parse(fs, args)
	if !ok {
		return exitFailure
	}
	if len(positionals) != 0 {
		return c.usageFailed("nothing is positional")
	}
	if strings.TrimSpace(*summary) == "" {
		return c.usageFailed("-summary is required: what the span held")
	}
	if !c.dial(*stateDir) {
		return exitFailure
	}
	return c.updateProductionMediation(func(mediator *workspacev1.WorkspaceRef) *agentreplv1.UpdateProductionMediationRequest {
		return &agentreplv1.UpdateProductionMediationRequest{Mediator: mediator, Action: &agentreplv1.UpdateProductionMediationRequest_Decide{Decide: &agentreplv1.ProductionMediationDecision{
			Summary: *summary, Decision: &agentreplv1.ProductionMediationDecision_NothingToDo{NothingToDo: &agentreplv1.ProductionMediationNothingToDo{}},
		}}}
	})
}

// productionDispatch ends the span with the children read from standing
// input (readProductionChildren), all made in one call.
func productionDispatch(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	summary := fs.String("summary", "", "what the span held, in a sentence (required)")
	positionals, ok := c.parse(fs, args)
	if !ok {
		return exitFailure
	}
	if len(positionals) != 0 {
		return c.usageFailed("nothing is positional; the children are JSON on standard input")
	}
	if strings.TrimSpace(*summary) == "" {
		return c.usageFailed("-summary is required: what the span held")
	}
	children, err := readProductionChildren(c.deps.stdin)
	if err != nil {
		return c.usageFailed("%v", err)
	}
	if !c.dial(*stateDir) {
		return exitFailure
	}
	return c.updateProductionMediation(func(mediator *workspacev1.WorkspaceRef) *agentreplv1.UpdateProductionMediationRequest {
		return &agentreplv1.UpdateProductionMediationRequest{Mediator: mediator, Action: &agentreplv1.UpdateProductionMediationRequest_Decide{Decide: &agentreplv1.ProductionMediationDecision{
			Summary: *summary, Decision: &agentreplv1.ProductionMediationDecision_Dispatch{Dispatch: &agentreplv1.ProductionMediationDispatch{Children: children}},
		}}}
	})
}

// productionFold folds one child into another.
func productionFold(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	folded := fs.String("folded", "", "the child folded away (required)")
	surviving := fs.String("surviving", "", "the child that takes over its work (required)")
	positionals, ok := c.parse(fs, args)
	if !ok {
		return exitFailure
	}
	if len(positionals) != 0 {
		return c.usageFailed("nothing is positional")
	}
	if *folded == "" || *surviving == "" {
		return c.usageFailed("-folded and -surviving are both required")
	}
	if !c.dial(*stateDir) {
		return exitFailure
	}
	foldedRef, ok := c.named(*folded)
	if !ok {
		return exitFailure
	}
	survivingRef, ok := c.named(*surviving)
	if !ok {
		return exitFailure
	}
	return c.updateProductionMediation(func(mediator *workspacev1.WorkspaceRef) *agentreplv1.UpdateProductionMediationRequest {
		return &agentreplv1.UpdateProductionMediationRequest{Mediator: mediator, Action: &agentreplv1.UpdateProductionMediationRequest_Fold{Fold: &agentreplv1.ProductionMediationFold{
			Folded: foldedRef, Surviving: survivingRef,
		}}}
	})
}

// updateProductionMediation asks BUILD's request of the dialed daemon as the
// calling workspace, and prints the answer.
func (c *cliCall) updateProductionMediation(build func(*workspacev1.WorkspaceRef) *agentreplv1.UpdateProductionMediationRequest) int {
	mediator, ok := c.calling()
	if !ok {
		return exitFailure
	}
	resp, err := c.client.UpdateProductionMediation(c.ctx, connect.NewRequest(build(mediator)))
	return answerDescribed(c, resp, err, func(msg *agentreplv1.UpdateProductionMediationResponse) (string, error) {
		return describeProductionRefusal(msg.GetError())
	})
}

// productionChildInput is one child as a dispatch's standard input spells it.
type productionChildInput struct {
	Name     string                   `json:"name"`
	Kind     string                   `json:"kind"`
	Findings []productionFindingInput `json:"findings"`
}

// productionFindingInput is one finding as standard input spells it.
type productionFindingInput struct {
	Statement string   `json:"statement"`
	Sites     []string `json:"sites"`
	Evidence  []string `json:"evidence"`
}

// readProductionChildren reads a dispatch's children from IN: a JSON list of
// {"name"?, "kind": "remediation"|"investigation", "findings": [{"statement",
// "sites"?, "evidence"}]}, at least one.
func readProductionChildren(in io.Reader) ([]*agentreplv1.ProductionChild, error) {
	var children []productionChildInput
	decoder := json.NewDecoder(in)
	decoder.DisallowUnknownFields()
	if err := decoder.Decode(&children); err != nil {
		return nil, fmt.Errorf(`read the children from standard input as a JSON list of {"name", "kind", "findings": [{"statement", "sites", "evidence"}]}: %w`, err)
	}
	if len(children) == 0 {
		return nil, errors.New("a dispatch names at least one child")
	}
	out := make([]*agentreplv1.ProductionChild, 0, len(children))
	for i, child := range children {
		c := &agentreplv1.ProductionChild{}
		if child.Name != "" {
			c.Name = &child.Name
		}
		switch child.Kind {
		case "remediation":
			c.Work = &agentreplv1.ProductionChild_Remediation{Remediation: &agentreplv1.ProductionChildRemediation{}}
		case "investigation":
			c.Work = &agentreplv1.ProductionChild_Investigation{Investigation: &agentreplv1.ProductionChildInvestigation{}}
		default:
			return nil, fmt.Errorf("child %d's kind is remediation or investigation, not %q", i, child.Kind)
		}
		for _, f := range child.Findings {
			finding := &agentreplv1.ProductionFinding{Statement: f.Statement, Evidence: f.Evidence}
			for _, site := range f.Sites {
				finding.Sites = append(finding.Sites, &agentreplv1.LogSiteKey{Hex: site})
			}
			c.Findings = append(c.Findings, finding)
		}
		out = append(out, c)
	}
	return out, nil
}

// describeProductionRefusal is E's refusal as one sentence.
func describeProductionRefusal(e *agentreplv1.UpdateProductionMediationError) (string, error) {
	switch r := e.GetCause().(type) {
	case *agentreplv1.UpdateProductionMediationError_UnknownWorkspace:
		return "the daemon serves no such workspace", nil
	case *agentreplv1.UpdateProductionMediationError_WorkspaceRefMismatch:
		return fmt.Sprintf("the workspace lives at %s, not where the request said", r.WorkspaceRefMismatch.GetRegistryDir()), nil
	case *agentreplv1.UpdateProductionMediationError_TransferringAway:
		return fmt.Sprintf("this daemon handed the workspace to %s; ask there", r.TransferringAway.GetAddress()), nil
	case *agentreplv1.UpdateProductionMediationError_NotProductionMediator:
		return "this workspace is no repository's production mediator", nil
	case *agentreplv1.UpdateProductionMediationError_NoCheckOwed:
		return "no production span you were told is owed", nil
	case *agentreplv1.UpdateProductionMediationError_SiteOwned:
		owned := r.SiteOwned
		if child := owned.GetOpenChild(); child != nil {
			return fmt.Sprintf("the log site %s is owned by the open child at %s; leave it to that child, and dispatch nothing", owned.GetSite().GetHex(), child.GetDir()), nil
		}
		return fmt.Sprintf("the log site %s is named by two children (the second is child %d); give each site to one child, and dispatch nothing", owned.GetSite().GetHex(), owned.GetRepeatedInChild()), nil
	case *agentreplv1.UpdateProductionMediationError_NotFoldable:
		return fmt.Sprintf("the workspace at %s cannot be folded: %s", r.NotFoldable.GetWorkspace().GetDir(), r.NotFoldable.GetDetail()), nil
	case *agentreplv1.UpdateProductionMediationError_ChildRefused:
		return fmt.Sprintf("child %d could not be made (%s); no child of the dispatch was kept", r.ChildRefused.GetChild(), r.ChildRefused.GetDetail()), nil
	case *agentreplv1.UpdateProductionMediationError_Paused:
		return "the repository's production checks are paused: decide, dispatch and fold nothing, and do not work around the pause", nil
	}
	return "", undescribed("deciding a production span", e)
}

// productionPause pauses or resumes one repository's production checks
// (agentrepl.v1.UpdateProductionPause). Any agent may ask it, not only the
// production mediator.
func productionPause(action string) func(c *cliCall, args []string) int {
	return func(c *cliCall, args []string) int {
		fs, stateDir := c.flags()
		repository := fs.String("repository", "", "here, or a main checkout (required)")
		positionals, ok := c.parse(fs, args)
		if !ok {
			return exitFailure
		}
		if len(positionals) != 0 {
			return c.usageFailed("nothing is positional")
		}
		if *repository == "" {
			return c.usageFailed("-repository is required: here, or a main checkout")
		}
		req := &agentreplv1.UpdateProductionPauseRequest{}
		switch action {
		case "pause":
			req.Action = &agentreplv1.UpdateProductionPauseRequest_Pause{Pause: &agentreplv1.UpdateProductionPausePause{}}
		case "resume":
			req.Action = &agentreplv1.UpdateProductionPauseRequest_Resume{Resume: &agentreplv1.UpdateProductionPauseResume{}}
		default:
			panic(fmt.Sprintf("agent-repld: production-mediation names the undeclared pause action %q", action))
		}
		if !c.dial(*stateDir) {
			return exitFailure
		}
		scope, ok := c.scopeFlag(*repository)
		if !ok {
			return exitFailure
		}
		req.Repository = scope
		resp, err := c.client.UpdateProductionPause(c.ctx, connect.NewRequest(req))
		return answerDescribed(c, resp, err, func(msg *agentreplv1.UpdateProductionPauseResponse) (string, error) {
			return describeProductionPauseRefusal(msg.GetError())
		})
	}
}

// describeProductionPauseRefusal says in one plain sentence why the
// repository's production checks were not paused or resumed. Every
// UpdateProductionPauseError arm has its own sentence.
func describeProductionPauseRefusal(e *agentreplv1.UpdateProductionPauseError) (string, error) {
	switch e.GetCause().(type) {
	case *agentreplv1.UpdateProductionPauseError_AlreadyPaused:
		return "the repository's production checks are already paused", nil
	case *agentreplv1.UpdateProductionPauseError_NotPaused:
		return "the repository's production checks are not paused", nil
	case *agentreplv1.UpdateProductionPauseError_UnknownRepository:
		return noRepositoryFlag, nil
	case *agentreplv1.UpdateProductionPauseError_ChecksOff:
		return "the repository's lifecycle config turns no production checks on, so there is nothing to pause or resume", nil
	}
	return "", undescribed("pausing production checks", e)
}
