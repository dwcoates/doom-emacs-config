package server

import (
	"context"
	"errors"
	"fmt"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/bringup"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/opentabs"
	"agent-repld/internal/wsm"
)

// Opens is what OpenTabs and ReportPageDrawn drive (internal/opentabs): the
// opens themselves, and the page facts an open waits on.
type Opens interface {
	// Run serves one open until every workspace is answered, or ctx ends,
	// or emit fails.
	Run(ctx context.Context, workspaces []opentabs.Workspace, emit opentabs.Emit)
	// PageAttached and PageDetached bracket one page stream's life.
	PageAttached(page string)
	PageDetached(page string)
	// PageServes records a page's accepted subscription naming a workspace.
	PageServes(page string, ws ids.WorkspaceID)
	// PageDrawn records a page of a workspace drawn, answering
	// opentabs.ErrUnknownPage for a page no stream holds.
	PageDrawn(page string, ws ids.WorkspaceID) error
}

// OpenTabs opens the workspaces the editor names (endpoint_open_tabs.proto).
//
// EVERY WORKSPACE IS RESOLVED BEFORE ANYTHING IS SENT OR STARTED: a malformed
// request is InvalidArgument and a registry that cannot be read ends the
// stream with its error, so an open never half-starts. A workspace this daemon
// will not open is answered `refused` with the same checks every
// per-workspace rpc takes (resolveStreamRef); one that is not visible is
// answered `hidden` and nothing is started for it.
func (s *server) OpenTabs(
	ctx context.Context,
	req *connect.Request[agentreplv1.OpenTabsRequest],
	out *connect.ServerStream[agentreplv1.OpenTabsResponse],
) error {
	const rpc = "OpenTabs"
	if err := validateOpenTabsRequest(req.Msg); err != nil {
		return err
	}
	streamCtx, cancel := s.streamContext(ctx)
	defer cancel()

	workspaces, err := s.resolveOpens(streamCtx, rpc, req.Msg.GetWorkspaces())
	if err != nil {
		return err
	}
	s.acceptStream(ctx, rpc)
	s.deps.Opens.Run(streamCtx, workspaces, func(frame *agentreplv1.OpenTabsResponse) error {
		if err := out.Send(frame); err != nil {
			s.log.Debug(rpc, "the editor's open stream went away", dlog.Context{"cause": err.Error()})
			return err
		}
		return nil
	})
	return nil
}

// validateOpenTabsRequest is OpenTabsRequest's base function: at least one
// workspace, each named once, each with its ref and its page arm set.
func validateOpenTabsRequest(req *agentreplv1.OpenTabsRequest) *connect.Error {
	if len(req.GetWorkspaces()) == 0 {
		return invalid("workspaces", "an open names at least one workspace")
	}
	seen := make(map[string]bool, len(req.GetWorkspaces()))
	for i, w := range req.GetWorkspaces() {
		field := fmt.Sprintf("workspaces[%d]", i)
		if err := validateWorkspaceRef(field+".workspace", w.GetWorkspace()); err != nil {
			return err
		}
		if w.GetPage() == nil {
			return invalid(field+".page", "the page arm says whether the editor loads a web page for it")
		}
		id := w.GetWorkspace().GetId()
		if seen[id] {
			return invalid(field+".workspace.id", fmt.Sprintf("workspace %q is named twice", id))
		}
		seen[id] = true
	}
	return nil
}

// resolveOpens resolves every workspace of an open: its refusal, or its
// visibility, name and page.
func (s *server) resolveOpens(ctx context.Context, rpc string, asked []*agentreplv1.OpenTabsWorkspace) ([]opentabs.Workspace, error) {
	out := make([]opentabs.Workspace, len(asked))
	var records []wsm.Workspace
	at := map[ids.WorkspaceID]int{}
	for i, w := range asked {
		ref := w.GetWorkspace()
		out[i] = opentabs.Workspace{Ref: ref, Name: ref.GetId(), Page: w.GetLoading() != nil}
		subject, r, err := s.resolveStreamRef(ctx, rpc, ref)
		if err != nil {
			return nil, failResolution(s.log, rpc, err)
		}
		if r != nil {
			refused := &agentreplv1.OpenTabsOpenedRefused{}
			if !setArm(refused.ProtoReflect(), r.Arm, r.Fields) {
				s.log.Error(rpc, "a workspace was refused with an arm OpenTabs does not carry", dlog.Context{
					"workspace": ref.GetId(), "arm": r.Arm, "reason": r.Reason,
					"invariant_violation": "every refusal resolveStreamRef answers is an OpenTabsOpenedRefused arm",
				})
				return nil, connect.NewError(connect.CodeInternal,
					fmt.Errorf("%s: workspace %q: refusal arm %q has no OpenTabs arm", rpc, ref.GetId(), r.Arm))
			}
			out[i].Refused = refused
			continue
		}
		out[i].Name = subject.Record.Name
		at[subject.Record.ID] = i
		records = append(records, subject.Record)
	}
	// ONE READ OF THE FOLDS, through the one filter every bring-up shares.
	_, withheld, err := bringup.OnlyVisible(ctx, s.deps.DB, s.log, records)
	if err != nil {
		return nil, fail(s.log, rpc, err)
	}
	for _, ws := range withheld {
		out[at[ws.ID]].Hidden = true
	}
	return out, nil
}

// ReportPageDrawn records that a workspace's page drew its conversation
// (endpoint_report_page_drawn.proto). The fact lives with the page's stream.
func (s *server) ReportPageDrawn(
	ctx context.Context,
	req *connect.Request[agentreplv1.ReportPageDrawnRequest],
) (*connect.Response[agentreplv1.ReportPageDrawnResponse], error) {
	const rpc = "ReportPageDrawn"
	if err := validateWorkspaceRef("workspace", req.Msg.GetWorkspace()); err != nil {
		return nil, err
	}
	if req.Msg.GetPage() == "" {
		return nil, invalid("page", "the page's connection id is required")
	}
	resp := &agentreplv1.ReportPageDrawnResponse{}
	subject, r, err := s.resolveRegistered(ctx, rpc, req.Msg.GetWorkspace())
	if err != nil {
		return nil, failResolution(s.log, rpc, err)
	}
	if r != nil {
		if cerr := s.refuse(s.log, rpc, resp, *r); cerr != nil {
			return nil, cerr
		}
		return connect.NewResponse(resp), nil
	}
	if err := s.deps.Opens.PageDrawn(req.Msg.GetPage(), subject.Record.ID); err != nil {
		if !errors.Is(err, opentabs.ErrUnknownPage) {
			return nil, fail(s.log, rpc, err)
		}
		if cerr := s.refuse(s.log, rpc, resp, refusal{
			Arm: "unknown_page", Reason: err.Error(), Info: true,
		}); cerr != nil {
			return nil, cerr
		}
		return connect.NewResponse(resp), nil
	}
	s.log.Debug(rpc, "a page reported its workspace drawn", dlog.Context{
		"page": req.Msg.GetPage(), "workspace": string(subject.Record.ID),
	})
	resp.Result = &agentreplv1.ReportPageDrawnResponse_Success{Success: &agentreplv1.ReportPageDrawnSuccess{}}
	return connect.NewResponse(resp), nil
}

// pageSubscriptionWorkspace names the workspace a page subscription serves:
// the arms that watch one workspace's conversation surfaces. A feed's token
// is opaque here and the roster, daemon and login watches name no
// conversation, so they serve none.
func pageSubscriptionWorkspace(req *agentreplv1.SubscribePageRequest) (ids.WorkspaceID, bool) {
	var id string
	switch watch := req.GetRequest().(type) {
	case *agentreplv1.SubscribePageRequest_WebWorkspace:
		id = watch.WebWorkspace.GetWorkspace().GetId()
	case *agentreplv1.SubscribePageRequest_Topbar:
		id = watch.Topbar.GetWorkspace().GetId()
	case *agentreplv1.SubscribePageRequest_Footer:
		id = watch.Footer.GetWorkspace().GetId()
	}
	return ids.WorkspaceID(id), id != ""
}
