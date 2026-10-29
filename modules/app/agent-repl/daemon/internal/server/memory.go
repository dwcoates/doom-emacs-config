package server

import (
	"context"
	"errors"
	"fmt"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/memorypopup"
)

// MemoryPopups is what the rail's memory popups read and Emacs's residency
// reports write (memorypopup.Popups).
type MemoryPopups interface {
	Report(ws ids.WorkspaceID, residency *agentreplv1.PageResidency) error
	Overview(open func(ids.WorkspaceID) bool) (*frontendv1.SidebarMemoryOverview, error)
	Workspace(ws ids.WorkspaceID) *frontendv1.SidebarWorkspaceMemory
}

const opMemory = "daemon.server.memory"

// validateReportPageResidencyRequest is ReportPageResidencyRequest's base
// function.
func validateReportPageResidencyRequest(req *agentreplv1.ReportPageResidencyRequest) *connect.Error {
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	return validatePageResidency("residency", req.GetResidency())
}

// validatePageResidency is PageResidency's base function: its state is set,
// and a loaded page names its release rule.
func validatePageResidency(field string, r *agentreplv1.PageResidency) *connect.Error {
	switch state := r.GetState().(type) {
	case *agentreplv1.PageResidency_NotLoaded:
		return nil
	case *agentreplv1.PageResidency_Loaded:
		return validatePageResidencyLoaded(field+".loaded", state.Loaded)
	default:
		return invalid(field+".state", "a page residency names its state")
	}
}

// validatePageResidencyLoaded is PageResidencyLoaded's base function.
func validatePageResidencyLoaded(field string, loaded *agentreplv1.PageResidencyLoaded) *connect.Error {
	if loaded.GetRelease().GetWhen() == nil {
		return invalid(field+".release.when", "a loaded page names when it is released")
	}
	return nil
}

// ReportPageResidency records what Emacs holds of a workspace's page, for its
// row's memory hover.
func (s *server) ReportPageResidency(
	ctx context.Context,
	req *connect.Request[agentreplv1.ReportPageResidencyRequest],
) (*connect.Response[agentreplv1.ReportPageResidencyResponse], error) {
	const rpc = "ReportPageResidency"
	if err := validateReportPageResidencyRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.ReportPageResidencyResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	if err := s.deps.Memory.Report(subject.Record.ID, req.Msg.GetResidency()); err != nil {
		subject.Log.Error(opMemory, "a validated page residency was refused by the memory popups", dlog.Context{"cause": err.Error()})
		return nil, connect.NewError(connect.CodeInternal, fmt.Errorf("%s: %w", rpc, err))
	}
	subject.Log.Debug(opMemory, "recorded the page's residency", dlog.Context{"loaded": req.Msg.GetResidency().GetLoaded() != nil})
	resp.Result = &agentreplv1.ReportPageResidencyResponse_Success{Success: &agentreplv1.ReportPageResidencySuccess{}}
	return connect.NewResponse(resp), nil
}

// GetMemoryOverview answers the version hover's popup.
func (s *server) GetMemoryOverview(
	ctx context.Context,
	req *connect.Request[agentreplv1.GetMemoryOverviewRequest],
) (*connect.Response[agentreplv1.GetMemoryOverviewResponse], error) {
	records, err := s.deps.DB.ListWorkspaces(ctx)
	if err != nil {
		s.log.Error(opMemory, "could not read the registry for the memory overview", dlog.Context{"cause": err.Error()})
		return nil, connect.NewError(connect.CodeInternal, fmt.Errorf("GetMemoryOverview: read the registry: %w", err))
	}
	open := make(map[ids.WorkspaceID]bool, len(records))
	for _, record := range records {
		if !record.Closed {
			open[record.ID] = true
		}
	}
	resp := &agentreplv1.GetMemoryOverviewResponse{}
	overview, err := s.deps.Memory.Overview(func(ws ids.WorkspaceID) bool { return open[ws] })
	switch {
	case errors.Is(err, memorypopup.ErrMeasurementFailed):
		resp.Result = &agentreplv1.GetMemoryOverviewResponse_Error{Error: &agentreplv1.GetMemoryOverviewError{
			Cause: &agentreplv1.GetMemoryOverviewError_MeasurementFailed{MeasurementFailed: &agentreplv1.GetMemoryOverviewMeasurementFailed{
				Reason: "the latest sample could not list agent-repl's processes",
			}},
		}}
	case err != nil:
		s.log.Error(opMemory, "the memory overview failed", dlog.Context{"cause": err.Error()})
		return nil, connect.NewError(connect.CodeInternal, fmt.Errorf("GetMemoryOverview: %w", err))
	case overview == nil:
		resp.Result = &agentreplv1.GetMemoryOverviewResponse_Success{Success: &agentreplv1.GetMemoryOverviewSuccess{
			Outcome: &agentreplv1.GetMemoryOverviewSuccess_NotYetMeasured{NotYetMeasured: &agentreplv1.GetMemoryOverviewNotYetMeasured{}},
		}}
	default:
		resp.Result = &agentreplv1.GetMemoryOverviewResponse_Success{Success: &agentreplv1.GetMemoryOverviewSuccess{
			Outcome: &agentreplv1.GetMemoryOverviewSuccess_Measured{Measured: overview},
		}}
	}
	return connect.NewResponse(resp), nil
}

// GetWorkspaceMemory answers a workspace row's memory lines.
func (s *server) GetWorkspaceMemory(
	ctx context.Context,
	req *connect.Request[agentreplv1.GetWorkspaceMemoryRequest],
) (*connect.Response[agentreplv1.GetWorkspaceMemoryResponse], error) {
	const rpc = "GetWorkspaceMemory"
	if err := validateWorkspaceRef("workspace", req.Msg.GetWorkspace()); err != nil {
		return nil, err
	}
	resp := &agentreplv1.GetWorkspaceMemoryResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	resp.Result = &agentreplv1.GetWorkspaceMemoryResponse_Success{Success: &agentreplv1.GetWorkspaceMemorySuccess{
		Memory: s.deps.Memory.Workspace(subject.Record.ID),
	}}
	return connect.NewResponse(resp), nil
}
