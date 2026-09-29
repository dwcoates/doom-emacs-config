package server

import (
	"context"
	"errors"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/desktopnotify"
	"claude-repld/internal/dlog"
)

// opEditorFocus is the operation ReportEditorFocus's records carry.
const opEditorFocus = "daemon.server.report_editor_focus"

// ReportEditorFocus moves Emacs's desktop focus, which decides every later
// desktop banner. A report with no Emacs WatchDaemon stream standing is
// refused: the focus belongs to that stream, and a report with no stream has
// nothing to belong to.
func (s *server) ReportEditorFocus(
	_ context.Context,
	req *connect.Request[agentreplv1.ReportEditorFocusRequest],
) (*connect.Response[agentreplv1.ReportEditorFocusResponse], error) {
	if req.Msg.GetFocus().GetFocus() == nil {
		return nil, invalid("focus", "whether Emacs is focused is required")
	}
	focused := isFocused(req.Msg.GetFocus())
	resp := &agentreplv1.ReportEditorFocusResponse{}
	if err := s.deps.Focus.Report(focused); err != nil {
		if !errors.Is(err, desktopnotify.ErrNoEmacsStream) {
			// The focus holder refuses for exactly one reason; any other is a
			// broken invariant, never an arm.
			panic("server: ReportEditorFocus: an unrecognized focus refusal: " + err.Error())
		}
		s.log.Info(opEditorFocus, "a focus report arrived with no Emacs stream standing; it was refused", dlog.Context{
			"focused": focused, "cause": err.Error(),
		})
		resp.Result = &agentreplv1.ReportEditorFocusResponse_Error{Error: &agentreplv1.ReportEditorFocusError{
			Cause: &agentreplv1.ReportEditorFocusError_NoEmacsStream{
				NoEmacsStream: &agentreplv1.ReportEditorFocusNoEmacsStream{},
			},
		}}
		return connect.NewResponse(resp), nil
	}
	resp.Result = &agentreplv1.ReportEditorFocusResponse_Success{Success: &agentreplv1.ReportEditorFocusSuccess{}}
	return connect.NewResponse(resp), nil
}

// isFocused reads the focus arm. The caller has refused an unset arm.
func isFocused(focus *agentreplv1.EditorFocus) bool {
	return focus.GetFocused() != nil
}
