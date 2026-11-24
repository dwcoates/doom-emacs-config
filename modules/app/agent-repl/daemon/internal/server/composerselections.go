package server

import (
	"context"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/publish"
	"agent-repld/internal/wsm"
)

// ComposerSelections is the record of every candidate the reader took from a
// composer's completion (internal/composerselections). ReportComposerSelection
// records into it, and the host stream serves it to Emacs as its
// `composer_selections` arm so every composer orders its completions by it.
type ComposerSelections interface {
	// Selected records NAME of KIND as the newest selection, durably, then
	// publishes the record. A failed write changes nothing and is returned,
	// already logged.
	Selected(ctx context.Context, kind wsm.ComposerSelectionKind, name string) error
	// Topic answers the record, holding the record in force so a fresh
	// subscriber is handed it first.
	Topic() *publish.Topic[*agentreplv1.HostComposerSelections]
}

// composerSelectionOf answers the kind and name SEL names, or false when its
// kind is unset (or one this build does not know).
func composerSelectionOf(sel *agentreplv1.ComposerSelection) (wsm.ComposerSelectionKind, string, bool) {
	switch kind := sel.GetKind().(type) {
	case *agentreplv1.ComposerSelection_Command:
		return wsm.ComposerSelectionCommand, kind.Command.GetName(), true
	case *agentreplv1.ComposerSelection_Workspace:
		return wsm.ComposerSelectionWorkspace, kind.Workspace.GetName(), true
	case *agentreplv1.ComposerSelection_Channel:
		return wsm.ComposerSelectionChannel, kind.Channel.GetName(), true
	default:
		return "", "", false
	}
}

// ReportComposerSelection records the candidate the reader just took from a
// composer's completion. The record is the user's, so no workspace is
// resolved; its change reaches Emacs on every host stream, never in this
// response. A write the store refused is the `not_recorded` arm, logged by the
// keeper that owns the write.
func (s *server) ReportComposerSelection(
	ctx context.Context,
	req *connect.Request[agentreplv1.ReportComposerSelectionRequest],
) (*connect.Response[agentreplv1.ReportComposerSelectionResponse], error) {
	if err := validateReportComposerSelectionRequest(req.Msg); err != nil {
		return nil, err
	}
	kind, name, ok := composerSelectionOf(req.Msg.GetSelection())
	if !ok {
		panic("server: ReportComposerSelection passed validation with no selection kind")
	}
	if err := s.deps.Selections.Selected(ctx, kind, name); err != nil {
		return connect.NewResponse(&agentreplv1.ReportComposerSelectionResponse{
			Result: &agentreplv1.ReportComposerSelectionResponse_Error{Error: &agentreplv1.ReportComposerSelectionError{
				Cause: &agentreplv1.ReportComposerSelectionError_NotRecorded{NotRecorded: &agentreplv1.ReportComposerSelectionNotRecorded{Detail: err.Error()}},
			}},
		}), nil
	}
	s.log.Debug("daemon.server.report_composer_selection", "recorded a composer selection",
		dlog.Context{"kind": string(kind), "name": name})
	return connect.NewResponse(&agentreplv1.ReportComposerSelectionResponse{
		Result: &agentreplv1.ReportComposerSelectionResponse_Success{Success: &agentreplv1.ReportComposerSelectionSuccess{}},
	}), nil
}
