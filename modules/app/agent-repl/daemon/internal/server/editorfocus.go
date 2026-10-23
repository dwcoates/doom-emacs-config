package server

import (
	"context"
	"errors"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/desktopnotify"
	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// opEditorFocus is the operation ReportEditorFocus's records carry.
const opEditorFocus = "daemon.server.report_editor_focus"

// Banners is the desktop banners a focus gain claims among
// (desktopnotify.Notifier): the claim itself, and the selection that lets
// every held banner go.
type Banners interface {
	// Claim answers the newest held banner and lets go of every banner held.
	Claim() desktopnotify.Claim
	// Forget lets go of every held banner, for reason.
	Forget(reason string)
}

// ReportEditorFocus moves Emacs's desktop focus, which decides every later
// desktop banner. A report that is a FOCUS GAIN claims the newest held banner
// in the same call and answers its workspace, which Emacs selects: one round
// trip from Emacs coming forward to its switch, and no report can land
// between the gain and its claim. A report with no Emacs WatchDaemon stream
// standing is refused: the focus belongs to that stream, and a report with no
// stream has nothing to belong to.
func (s *server) ReportEditorFocus(
	_ context.Context,
	req *connect.Request[agentreplv1.ReportEditorFocusRequest],
) (*connect.Response[agentreplv1.ReportEditorFocusResponse], error) {
	if req.Msg.GetFocus().GetFocus() == nil {
		return nil, invalid("focus", "whether Emacs is focused is required")
	}
	focused := isFocused(req.Msg.GetFocus())
	resp := &agentreplv1.ReportEditorFocusResponse{}
	gained, err := s.deps.Focus.Report(focused)
	if err != nil {
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
	resp.Result = &agentreplv1.ReportEditorFocusResponse_Success{Success: s.focusClaim(gained)}
	return connect.NewResponse(resp), nil
}

// focusClaim answers what an accepted report claimed: nothing unless it is a
// gain, and for a gain the newest held banner's workspace, or that none was
// held.
func (s *server) focusClaim(gained bool) *agentreplv1.ReportEditorFocusSuccess {
	if !gained {
		return &agentreplv1.ReportEditorFocusSuccess{Claim: &agentreplv1.ReportEditorFocusSuccess_NoGain{
			NoGain: &agentreplv1.ReportEditorFocusNoGain{},
		}}
	}
	claim := s.deps.Banners.Claim()
	if !claim.Held {
		return &agentreplv1.ReportEditorFocusSuccess{Claim: &agentreplv1.ReportEditorFocusSuccess_NothingHeld{
			NothingHeld: &agentreplv1.ReportEditorFocusNothingHeld{},
		}}
	}
	return &agentreplv1.ReportEditorFocusSuccess{Claim: &agentreplv1.ReportEditorFocusSuccess_Banner{
		Banner: &agentreplv1.ReportEditorFocusBannerClaimed{Workspace: wsm.FamilyRef(claim.Target).Ref()},
	}}
}

// isFocused reads the focus arm. The caller has refused an unset arm.
func isFocused(focus *agentreplv1.EditorFocus) bool {
	return focus.GetFocused() != nil
}

// EmacsMeasure measures the Emacs whose WatchDaemon stream stands
// (runtimemetrics.Surface is the production one; its off surface measures
// nothing).
type EmacsMeasure interface {
	// AttachEmacs measures the Emacs of pid until the returned release runs.
	AttachEmacs(pid int) (release func())
}
