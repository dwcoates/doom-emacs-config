package server

import (
	"context"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	"agentrepl/protohelpers/rosterwalk"

	"agent-repld/internal/dlog"
)

// WatchWorkspaceMentions serves one page's workspace mention targets: the
// names its response trees link to the workspaces they switch to.
func (s *server) WatchWorkspaceMentions(
	ctx context.Context,
	req *connect.Request[agentreplv1.WatchWorkspaceMentionsRequest],
	out *connect.ServerStream[agentreplv1.WatchWorkspaceMentionsResponse],
) error {
	const rpc = "WatchWorkspaceMentions"
	if err := s.watchWorkspaceMentions(ctx, req.Msg, out); err != nil {
		return err
	}
	endStandingStream(s, rpc, s.log, out, workspaceMentionsEnding())
	return nil
}

// workspaceMentionsEnding is the mention stream's planned ending frame, the
// dedicated rpc's and a page subscription's alike.
func workspaceMentionsEnding() *agentreplv1.WatchWorkspaceMentionsResponse {
	return &agentreplv1.WatchWorkspaceMentionsResponse{
		Push: &agentreplv1.WatchWorkspaceMentionsResponse_Ending{Ending: &agentreplv1.DaemonStreamEnding{}},
	}
}

// watchWorkspaceMentions is the body, written to whatever sink carries it.
//
// THE SET IS THE ROSTER'S, RESOLVED FOR ONE VIEWER. Every roster the sidebar
// publishes is turned into the viewer's targets (`mentionTargets`), and a set
// equal to the last one this stream sent is not sent again: the roster
// publishes on every row change, and nearly none of those change a link.
func (s *server) watchWorkspaceMentions(
	ctx context.Context,
	msg *agentreplv1.WatchWorkspaceMentionsRequest,
	out streamSink[agentreplv1.WatchWorkspaceMentionsResponse],
) error {
	const rpc = "WatchWorkspaceMentions"
	if err := validateWorkspaceRef("viewer", msg.GetViewer()); err != nil {
		return err
	}
	viewer := msg.GetViewer().GetId()
	log := s.log.With(dlog.Context{"viewer": viewer})
	return serveTopic(s, ctx, rpc, log, s.deps.Sidebar.Topic(), &changedOnlySink[agentreplv1.WatchWorkspaceMentionsResponse, *agentreplv1.WatchWorkspaceMentionsResponse]{
		out: out,
		sent: func(m *agentreplv1.WatchWorkspaceMentionsResponse) {
			log.Debug(rpc, "sent a changed set of workspace mention targets",
				dlog.Context{"targets": len(m.GetTargets().GetTargets())})
		},
	}, func(roster *frontendRoster) *agentreplv1.WatchWorkspaceMentionsResponse {
		return &agentreplv1.WatchWorkspaceMentionsResponse{
			Push: &agentreplv1.WatchWorkspaceMentionsResponse_Targets{Targets: mentionTargets(roster, viewer)},
		}
	})
}

// mentionTargets resolves VIEWER's workspace mention targets from ROSTER: the
// Open section's workspaces in its drawn order, less VIEWER's own and less
// every name two of them share, since a shared name names no one workspace.
// Hidden workspaces are not in the Open section and so are never targets.
func mentionTargets(roster *frontendRoster, viewer string) *frontendv1.WorkspaceMentionTargets {
	rows := rosterwalk.OpenRows(roster)
	named := make(map[string]int, len(rows))
	for _, open := range rows {
		named[open.GetRow().GetName().GetText()]++
	}
	targets := &frontendv1.WorkspaceMentionTargets{}
	for _, open := range rows {
		row := open.GetRow()
		name := row.GetName().GetText()
		ref := row.GetWorkspace().GetWorkspace()
		if name == "" || named[name] > 1 || ref.GetId() == viewer {
			continue
		}
		targets.Targets = append(targets.Targets, &frontendv1.WorkspaceMentionTarget{
			Name:      &frontendv1.WorkspaceMentionName{Text: name},
			Workspace: ref,
		})
	}
	return targets
}

// changedOnlySink sends a frame only when it differs from the last one it
// sent, so a stream whose view is DERIVED from a busier topic pushes only the
// derived view's changes. SENT, when set, is told of each frame that went out.
type changedOnlySink[R any, P interface {
	*R
	proto.Message
}] struct {
	out  streamSink[R]
	last P
	sent func(*R)
}

// Send implements streamSink.
func (c *changedOnlySink[R, P]) Send(m *R) error {
	if c.last != nil && proto.Equal(c.last, P(m)) {
		return nil
	}
	if err := c.out.Send(m); err != nil {
		return err
	}
	c.last = P(m)
	if c.sent != nil {
		c.sent(m)
	}
	return nil
}
