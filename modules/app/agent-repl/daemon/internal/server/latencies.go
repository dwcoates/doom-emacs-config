package server

import (
	"context"
	"errors"
	"fmt"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/runtimemetrics"
	"agent-repld/internal/workspace"
	"agent-repld/internal/wsm"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

// LatencyRecorder is where ReportLatencies records what it accepts: the
// daemon's metrics surface (runtimemetrics.Surface), whose off value records
// nothing and says so.
type LatencyRecorder interface {
	// Enabled reports whether anything is recorded.
	Enabled() bool
	// ObserveLatency records one validated latency for ws.
	ObserveLatency(ws ids.WorkspaceID, kind runtimemetrics.LatencyKind, seconds float64)
}

// ReportLatencies records a client's finished interaction latencies in the
// always-on metrics (endpoint_report_latencies.proto).
//
// THE BATCH IS ALL OR NOTHING ON A DEFECT, PER WORKSPACE ON A SCHEDULE. A
// malformed latency is InvalidArgument and a ref whose dir disagrees with the
// registry refuses the whole batch, because either is a client defect and a
// half-recorded batch would hide it. A workspace the registry no longer holds
// is an ordinary schedule (it closed while the report was in flight): its
// latencies are dropped and counted, the rest recorded.
func (s *server) ReportLatencies(
	ctx context.Context,
	req *connect.Request[agentreplv1.ReportLatenciesRequest],
) (*connect.Response[agentreplv1.ReportLatenciesResponse], error) {
	const rpc = "ReportLatencies"
	kinds := make([]runtimemetrics.LatencyKind, len(req.Msg.GetLatencies()))
	for i, l := range req.Msg.GetLatencies() {
		field := fmt.Sprintf("latencies[%d]", i)
		if err := validateWorkspaceRef(field+".workspace", l.GetWorkspace()); err != nil {
			return nil, err
		}
		kind, err := runtimemetrics.ValidateReportedLatency(l)
		if err != nil {
			return nil, invalid(field, err.Error())
		}
		kinds[i] = kind
	}
	resp := &agentreplv1.ReportLatenciesResponse{}
	if !s.deps.Latencies.Enabled() {
		resp.Result = &agentreplv1.ReportLatenciesResponse_Success{Success: &agentreplv1.ReportLatenciesSuccess{
			Outcome: &agentreplv1.ReportLatenciesSuccess_MetricsOff{MetricsOff: &agentreplv1.ReportLatenciesMetricsOff{}},
		}}
		return connect.NewResponse(resp), nil
	}

	known := map[ids.WorkspaceID]bool{}
	for _, l := range req.Msg.GetLatencies() {
		id := ids.WorkspaceID(l.GetWorkspace().GetId())
		if _, decided := known[id]; decided {
			continue
		}
		record, ended, err := s.registryWorkspace(ctx, id)
		switch {
		case ended:
			s.log.Info(rpc, "refused a request that arrived while the daemon was shutting down", dlog.Context{"workspace": string(id)})
			return nil, connect.NewError(connect.CodeUnavailable, fmt.Errorf("%s: workspace %q: %w", rpc, id, errServingEnded))
		case errors.Is(err, wsm.ErrNotFound):
			known[id] = false
			continue
		case err != nil:
			return nil, fail(s.log, rpc, fmt.Errorf("read workspace %q: %w", id, err))
		}
		if dir := l.GetWorkspace().GetDir(); dir != "" && dir != record.Dir {
			if cerr := s.refuse(s.log, rpc, resp, s.fill(refusal{
				Arm:    workspace.ArmWorkspaceRefMismatch,
				Reason: fmt.Sprintf("the ref for workspace %q names dir %q; the registry holds %q", id, dir, record.Dir),
				Fields: map[string]any{"workspace_id": string(id), "reported_dir": dir, "registry_dir": record.Dir},
			})); cerr != nil {
				return nil, cerr
			}
			return connect.NewResponse(resp), nil
		}
		known[id] = true
	}

	var recorded, dropped uint32
	for i, l := range req.Msg.GetLatencies() {
		id := ids.WorkspaceID(l.GetWorkspace().GetId())
		if !known[id] {
			dropped++
			continue
		}
		s.deps.Latencies.ObserveLatency(id, kinds[i], l.GetSeconds())
		recorded++
	}
	success := &agentreplv1.ReportLatenciesSuccess{}
	if dropped == 0 {
		success.Outcome = &agentreplv1.ReportLatenciesSuccess_Recorded{Recorded: &agentreplv1.ReportLatenciesRecorded{Recorded: recorded}}
	} else {
		s.log.Debug(rpc, "dropped latencies of workspaces the registry no longer holds", dlog.Context{"recorded": recorded, "dropped": dropped})
		success.Outcome = &agentreplv1.ReportLatenciesSuccess_PartlyRecorded{PartlyRecorded: &agentreplv1.ReportLatenciesPartlyRecorded{
			Recorded: recorded, DroppedUnknownWorkspace: dropped,
		}}
	}
	resp.Result = &agentreplv1.ReportLatenciesResponse_Success{Success: success}
	return connect.NewResponse(resp), nil
}
