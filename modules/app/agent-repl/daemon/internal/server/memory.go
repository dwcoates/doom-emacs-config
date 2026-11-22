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

// MemoryPopups is what the rail's memory popups read (memorypopup.Popups).
type MemoryPopups interface {
	Overview(open []ids.WorkspaceID) (*frontendv1.SidebarMemoryOverview, error)
	Workspace(ws ids.WorkspaceID) *frontendv1.SidebarWorkspaceMemory
}

const opMemory = "daemon.server.memory"

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
	open := make([]ids.WorkspaceID, 0, len(records))
	for _, record := range records {
		if !record.Closed {
			open = append(open, record.ID)
		}
	}
	resp := &agentreplv1.GetMemoryOverviewResponse{}
	overview, err := s.deps.Memory.Overview(open)
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
