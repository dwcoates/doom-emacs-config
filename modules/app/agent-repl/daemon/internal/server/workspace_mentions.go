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
// names its bubbles link to the workspaces they switch to.
func (s *server) WatchWorkspaceMentions(
	ctx context.Context,
	req *connect.Request[agentreplv1.WatchWorkspaceMentionsRequest],
	out *connect.ServerStream[agentreplv1.WatchWorkspaceMentionsResponse],
) error {
	return serveThenEnd(s, "WatchWorkspaceMentions", out, workspaceMentionsEnding(), func() error {
		return s.watchWorkspaceMentions(ctx, req.Msg, out)
	})
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

// mentionTargets resolves VIEWER's workspace mention targets from ROSTER:
// the one set the viewer's page links and the viewer's composer completes
// (frontend.v1.WorkspaceMentionTargets), so the two cannot differ.
//
// Every workspace the roster carries that is not closed is a candidate: the
// Open section's in its drawn order, then the hidden section's. A name only
// one candidate carries names it. A name several carry names the one of them
// in the viewer's own repository when exactly one is, and otherwise no one.
// The viewer's own workspace is never a target, and a name it carries names
// no other workspace either, so a word the reader's own workspace is called
// never switches somewhere else.
func mentionTargets(roster *frontendRoster, viewer string) *frontendv1.WorkspaceMentionTargets {
	var rows []*frontendv1.RosterRow
	for _, open := range rosterwalk.OpenRows(roster) {
		rows = append(rows, open.GetRow())
	}
	rows = append(rows, rosterwalk.HiddenRows(roster)...)

	viewerRepository := ""
	byName := make(map[string][]*frontendv1.RosterRow, len(rows))
	for _, row := range rows {
		if row.GetWorkspace().GetWorkspace().GetId() == viewer {
			viewerRepository = row.GetRepository().GetName()
		}
		if name := row.GetName().GetText(); name != "" {
			byName[name] = append(byName[name], row)
		}
	}

	targets := &frontendv1.WorkspaceMentionTargets{}
	for _, row := range rows {
		name := row.GetName().GetText()
		if name == "" || mentionNamed(byName[name], viewerRepository) != row {
			continue
		}
		ref := row.GetWorkspace().GetWorkspace()
		if ref.GetId() == viewer {
			continue
		}
		target := &frontendv1.WorkspaceMentionTarget{
			Name:      &frontendv1.WorkspaceMentionName{Text: name},
			Workspace: ref,
		}
		if repository := row.GetRepository().GetName(); repository != "" {
			target.Repository = &frontendv1.WorkspaceMentionRepository{Name: repository}
		}
		targets.Targets = append(targets.Targets, target)
	}
	return targets
}

// mentionNamed answers the row a name CARRIERS share names, or nil: the one
// carrier, or the one of several in VIEWERREPOSITORY ("" for a viewer whose
// repository is unknown, which prefers no one).
func mentionNamed(carriers []*frontendv1.RosterRow, viewerRepository string) *frontendv1.RosterRow {
	if len(carriers) == 1 {
		return carriers[0]
	}
	var named *frontendv1.RosterRow
	for _, row := range carriers {
		if viewerRepository == "" || row.GetRepository().GetName() != viewerRepository {
			continue
		}
		if named != nil {
			return nil
		}
		named = row
	}
	return named
}

// mentionTargetsTracker resolves one viewer's mention targets from each
// roster in turn, answering whether the set differs from the last it
// answered: the host stream's change filter, since the roster publishes on
// every row change and nearly none of those change a target.
type mentionTargetsTracker struct {
	viewer string
	last   *frontendv1.WorkspaceMentionTargets
}

// next resolves ROSTER's targets, and whether they differ from the last set
// next answered (the first set always does).
func (t *mentionTargetsTracker) next(roster *frontendRoster) (*frontendv1.WorkspaceMentionTargets, bool) {
	set := mentionTargets(roster, t.viewer)
	if t.last != nil && proto.Equal(t.last, set) {
		return set, false
	}
	t.last = set
	return set, true
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
