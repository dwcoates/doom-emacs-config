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
	"agent-repld/internal/resolve/sidebar"
)

// opRecencyReach is the operation ReportRecencyReach's records carry.
const opRecencyReach = "daemon.server.report_recency_reach"

// ReportRecencyReach installs Emacs's recency reach, which the roster draws
// as its open rows' gutters (endpoint_report_recency_reach.proto). The reach
// belongs to Emacs's WatchDaemon stream, so a report with none standing is
// refused, exactly as a focus report is (ReportEditorFocus).
func (s *server) ReportRecencyReach(
	_ context.Context,
	req *connect.Request[agentreplv1.ReportRecencyReachRequest],
) (*connect.Response[agentreplv1.ReportRecencyReachResponse], error) {
	reach, err := recencyReachOf(req.Msg.GetReaches())
	if err != nil {
		return nil, err
	}
	resp := &agentreplv1.ReportRecencyReachResponse{}
	if err := s.deps.Sidebar.ReportRecencyReach(reach); err != nil {
		if !errors.Is(err, sidebar.ErrNoEmacsStream) {
			// The roster refuses a report for exactly one reason; any other is
			// a broken invariant, never an arm.
			panic("server: ReportRecencyReach: an unrecognized refusal: " + err.Error())
		}
		s.log.Info(opRecencyReach, "a recency reach arrived with no Emacs stream standing; it was refused", dlog.Context{
			"reaches": len(reach), "cause": err.Error(),
		})
		resp.Result = &agentreplv1.ReportRecencyReachResponse_Error{Error: &agentreplv1.ReportRecencyReachError{
			Cause: &agentreplv1.ReportRecencyReachError_NoEmacsStream{
				NoEmacsStream: &agentreplv1.ReportRecencyReachNoEmacsStream{},
			},
		}}
		return connect.NewResponse(resp), nil
	}
	s.log.Debug(opRecencyReach, "the roster took Emacs's recency reach", dlog.Context{"reaches": len(reach)})
	resp.Result = &agentreplv1.ReportRecencyReachResponse_Success{Success: &agentreplv1.ReportRecencyReachSuccess{}}
	return connect.NewResponse(resp), nil
}

// recencyReachOf reads REACHES into the roster's gutters by workspace,
// refusing a malformed report as Connect InvalidArgument: a reach naming no
// workspace, one named twice, no direction, or zero presses.
func recencyReachOf(reaches []*agentreplv1.RecencyReach) (sidebar.RecencyReach, error) {
	out := sidebar.RecencyReach{}
	for i, reach := range reaches {
		field := fmt.Sprintf("reaches[%d]", i)
		id := ids.WorkspaceID(reach.GetWorkspace().GetId())
		if id == "" {
			return nil, invalid(field+".workspace", "the reached workspace is required")
		}
		if _, seen := out[id]; seen {
			return nil, invalid(field+".workspace", fmt.Sprintf("workspace %s is reached twice", id))
		}
		gutter, err := recencyGutterOf(reach, field)
		if err != nil {
			return nil, err
		}
		out[id] = gutter
	}
	return out, nil
}

// recencyGutterOf answers REACH's direction as the open row's gutter.
func recencyGutterOf(reach *agentreplv1.RecencyReach, field string) (*frontendv1.RosterOpenRowRecencyGutter, error) {
	switch d := reach.GetDirection().(type) {
	case *agentreplv1.RecencyReach_Older:
		if d.Older.GetPresses() == 0 {
			return nil, invalid(field+".older.presses", "a reach is at least one press")
		}
		return &frontendv1.RosterOpenRowRecencyGutter{Reach: &frontendv1.RosterOpenRowRecencyGutter_Older{
			Older: &frontendv1.RosterOpenRowRecencyGutterOlder{Presses: d.Older.GetPresses()},
		}}, nil
	case *agentreplv1.RecencyReach_Newer:
		if d.Newer.GetPresses() == 0 {
			return nil, invalid(field+".newer.presses", "a reach is at least one press")
		}
		return &frontendv1.RosterOpenRowRecencyGutter{Reach: &frontendv1.RosterOpenRowRecencyGutter_Newer{
			Newer: &frontendv1.RosterOpenRowRecencyGutterNewer{Presses: d.Newer.GetPresses()},
		}}, nil
	case nil:
		return nil, invalid(field+".direction", "which chord reaches the tab is required")
	default:
		panic(fmt.Sprintf("server: ReportRecencyReach: an unhandled direction arm %T", d))
	}
}
