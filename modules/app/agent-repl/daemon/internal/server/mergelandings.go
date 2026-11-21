package server

import (
	"context"
	"strings"
	"time"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/merge"
	"agent-repld/internal/workspace"
	"agent-repld/internal/wsm"
)

// The landings on a repository's main branch: the read of a window of them,
// the merges through the queue apart from every other, and land-direct.sh's
// report of a landing it made (merge landings.go records every one).

// callerRenames spells an unknown caller as RecordDirectLanding's own arm.
var callerRenames = map[string]string{workspace.ArmUnknownWorkspace: "unknown_caller"}

// ListMergeLandings answers a repository's landings in a window.
func (s *server) ListMergeLandings(
	ctx context.Context,
	req *connect.Request[agentreplv1.ListMergeLandingsRequest],
) (*connect.Response[agentreplv1.ListMergeLandingsResponse], error) {
	const rpc = "ListMergeLandings"
	if err := validateListMergeLandingsRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.ListMergeLandingsResponse{}
	since := time.UnixMilli(req.Msg.GetSinceMs())
	until := s.now()
	if req.Msg.UntilMs != nil {
		until = time.UnixMilli(req.Msg.GetUntilMs())
	}
	if until.Before(since) {
		// AN UNSET END IS NOW, so a start in the future reverses the window
		// only once it is resolved.
		return nil, invalid("since_ms", "the window starts after it ends")
	}
	window, err := s.deps.Merge.Landings(ctx, *scopeOf(req.Msg.GetRepository()), since, until)
	if err != nil {
		return answer(resp, s.answerRefusal(s.log, rpc, resp, err, nil))
	}
	success := mergeLandingsOf(window, since, until)
	s.log.Debug("daemon.server.merge_landings", "answered a repository's landings in a window", dlog.Context{
		"repository": string(window.Repository.ID), "through_queue": success.GetThroughQueue().GetCount(), "otherwise": success.GetOtherwise().GetCount(),
		"partial": success.GetCoverage().GetPartial() != nil})
	resp.Result = &agentreplv1.ListMergeLandingsResponse_Success{Success: success}
	return connect.NewResponse(resp), nil
}

// RecordDirectLanding records land-direct.sh's report of a landing it made.
func (s *server) RecordDirectLanding(
	ctx context.Context,
	req *connect.Request[agentreplv1.RecordDirectLandingRequest],
) (*connect.Response[agentreplv1.RecordDirectLandingResponse], error) {
	const rpc = "RecordDirectLanding"
	if err := validateRecordDirectLandingRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.RecordDirectLandingResponse{}
	log := s.log
	report := merge.DirectLanding{Repository: *scopeOf(req.Msg.GetRepository()), Branch: req.Msg.GetBranch(), Head: req.Msg.GetLandedHead()}
	if req.Msg.Caller != nil {
		caller, refused, done := s.resolveNaming(ctx, rpc, resp, req.Msg.GetCaller(), callerRenames)
		if done {
			return answer(resp, refused)
		}
		report.Caller, log = caller.Record.ID, caller.Log
	}
	path, err := s.deps.Merge.RecordDirectLanding(ctx, report)
	if err != nil {
		return answer(resp, s.answerRefusal(log, rpc, resp, err, nil))
	}
	resp.Result = &agentreplv1.RecordDirectLandingResponse_Success{Success: &agentreplv1.RecordDirectLandingSuccess{Path: otherwisePathOf(path)}}
	return connect.NewResponse(resp), nil
}

// mergeLandingsOf is THE ONE MAPPING from the store's landings to the
// wire's: the queue's into their list, every other into the other, and the
// window's coverage from when recording began.
func mergeLandingsOf(window merge.LandingsWindow, since, until time.Time) *agentreplv1.ListMergeLandingsSuccess {
	coverage := &agentreplv1.ListMergeLandingsCoverage{Coverage: &agentreplv1.ListMergeLandingsCoverage_Complete{Complete: &agentreplv1.ListMergeLandingsCoverageComplete{}}}
	if window.RecordingBegan.After(since) {
		coverage.Coverage = &agentreplv1.ListMergeLandingsCoverage_Partial{Partial: &agentreplv1.ListMergeLandingsCoveragePartial{
			RecordedSinceMs: window.RecordingBegan.UnixMilli()}}
	}
	queue := &agentreplv1.ListMergeLandingsThroughQueue{Count: uint32(len(window.ThroughQueue))}
	for _, l := range window.ThroughQueue {
		if l.Path != wsm.LandingThroughQueue || l.Workspace == "" {
			panic("server: a landing through the queue is the queue's and names its workspace (" + l.Head + ")")
		}
		queue.Landings = append(queue.Landings, &agentreplv1.MergeLandingThroughQueue{
			Workspace: landingWorkspaceOf(l), Branch: l.Branch, LandedHead: l.Head, LandedAtMs: l.LandedAt.UnixMilli()})
	}
	otherwise := &agentreplv1.ListMergeLandingsOtherwise{Count: uint32(len(window.Otherwise))}
	for _, l := range window.Otherwise {
		entry := &agentreplv1.MergeLandingOtherwise{Branch: l.Branch, LandedHead: l.Head, LandedAtMs: l.LandedAt.UnixMilli(), Path: otherwisePathOf(l.Path)}
		if l.Workspace != "" {
			entry.Workspace = landingWorkspaceOf(l)
		}
		otherwise.Landings = append(otherwise.Landings, entry)
	}
	return &agentreplv1.ListMergeLandingsSuccess{
		Repository:   &workspacev1.RepositoryRef{Id: string(window.Repository.ID), Dir: window.Repository.Dir},
		Window:       &agentreplv1.ListMergeLandingsWindow{SinceMs: since.UnixMilli(), UntilMs: until.UnixMilli()},
		Coverage:     coverage,
		ThroughQueue: queue,
		Otherwise:    otherwise,
	}
}

// landingWorkspaceOf renders the workspace a landing names.
func landingWorkspaceOf(l wsm.MergeLanding) *agentreplv1.MergeLandingWorkspace {
	return &agentreplv1.MergeLandingWorkspace{Workspace: &workspacev1.WorkspaceRef{Id: string(l.Workspace)}, Name: l.WorkspaceName}
}

// otherwisePathOf renders a landing path other than the queue's. The queue's
// path, or one the store does not name, reaching it is a defect, and panics.
func otherwisePathOf(path wsm.LandingPath) *agentreplv1.MergeLandingOtherwisePath {
	switch path {
	case wsm.LandingQueueBypass:
		return &agentreplv1.MergeLandingOtherwisePath{Path: &agentreplv1.MergeLandingOtherwisePath_QueueBypass{QueueBypass: &agentreplv1.MergeLandingOtherwisePathQueueBypass{}}}
	case wsm.LandingDeployMediator:
		return &agentreplv1.MergeLandingOtherwisePath{Path: &agentreplv1.MergeLandingOtherwisePath_DeployMediator{DeployMediator: &agentreplv1.MergeLandingOtherwisePathDeployMediator{}}}
	case wsm.LandingDirect:
		return &agentreplv1.MergeLandingOtherwisePath{Path: &agentreplv1.MergeLandingOtherwisePath_Direct{Direct: &agentreplv1.MergeLandingOtherwisePathDirect{}}}
	}
	panic("server: a landing outside the queue took no path it can name (" + string(path) + ")")
}

// validateListMergeLandingsRequest is ListMergeLandingsRequest's base
// function.
func validateListMergeLandingsRequest(req *agentreplv1.ListMergeLandingsRequest) *connect.Error {
	if err := validateRepositoryRef("repository", req.GetRepository()); err != nil {
		return err
	}
	if req.GetSinceMs() <= 0 {
		return invalid("since_ms", "name the window's start")
	}
	if req.UntilMs != nil && req.GetUntilMs() < req.GetSinceMs() {
		return invalid("until_ms", "the window ends before it starts")
	}
	return nil
}

// validateRecordDirectLandingRequest is RecordDirectLandingRequest's base
// function.
func validateRecordDirectLandingRequest(req *agentreplv1.RecordDirectLandingRequest) *connect.Error {
	if err := validateRepositoryRef("repository", req.GetRepository()); err != nil {
		return err
	}
	if strings.TrimSpace(req.GetBranch()) == "" {
		return invalid("branch", "name the branch that landed")
	}
	if strings.TrimSpace(req.GetLandedHead()) == "" {
		return invalid("landed_head", "name the main branch's commit once it landed")
	}
	if req.Caller != nil {
		return validateWorkspaceRef("caller", req.GetCaller())
	}
	return nil
}
