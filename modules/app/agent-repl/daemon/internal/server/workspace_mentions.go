package server

import (
	"context"
	"time"

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
	targets := s.mentionTracker(viewer)
	return serveTopic(s, ctx, rpc, log, s.deps.Sidebar.Topic(), &changedOnlySink[agentreplv1.WatchWorkspaceMentionsResponse, *agentreplv1.WatchWorkspaceMentionsResponse]{
		out: out,
		sent: func(m *agentreplv1.WatchWorkspaceMentionsResponse) {
			log.Debug(rpc, "sent a changed set of workspace mention targets",
				dlog.Context{"targets": len(m.GetTargets().GetTargets())})
		},
	}, func(push *frontendRoster) *agentreplv1.WatchWorkspaceMentionsResponse {
		return &agentreplv1.WatchWorkspaceMentionsResponse{
			Push: &agentreplv1.WatchWorkspaceMentionsResponse_Targets{Targets: targets.targets(push)},
		}
	})
}

// RecentlyClosedMentionWindow is how long after a workspace closes its name
// still links (owner request, 2026-11-17): a workspace closed at or after
// NOW less this window is a mention target, and a click on it reopens it.
const RecentlyClosedMentionWindow = 5 * 24 * time.Hour

// mentionTargets resolves VIEWER's workspace mention targets from PUSH, a
// whole roster push, and MERGED, every merged workspace's row (which the
// roster no longer carries), at NOW: the one set the viewer's page links and
// the viewer's composer completes (frontend.v1.WorkspaceMentionTargets), so
// the two cannot differ.
//
// Every workspace the workspaces stream carries is a LIVE candidate: the Open
// section's in its drawn order, then the hidden section's. Every workspace
// closed within RecentlyClosedMentionWindow follows as a CLOSED candidate:
// the merged, then the closed stream's, in that order; a closed row without a
// close record is none, since nothing vouches for when it closed. A name only
// one candidate carries names it. A name several carry names the one of them
// in the viewer's own repository when exactly one is, and otherwise no one. A
// name a live candidate carries is settled among the live ones alone, so a
// closed workspace whose name was reused never costs the live one its link.
// The viewer's own workspace is never a target, and a name it carries names
// no other workspace either, so a word the reader's own workspace is called
// never switches somewhere else.
func mentionTargets(push *frontendRoster, merged []*frontendv1.RosterRow, viewer string, now time.Time) *frontendv1.WorkspaceMentionTargets {
	roster := push.GetWorkspaces()
	var live, closed []*frontendv1.RosterRow
	for _, open := range rosterwalk.OpenRows(roster) {
		live = append(live, open.GetRow())
	}
	live = append(live, rosterwalk.HiddenRows(roster)...)
	since := now.Add(-RecentlyClosedMentionWindow).UnixMilli()
	for _, row := range append(append([]*frontendv1.RosterRow(nil), merged...), rosterwalk.ClosedRows(push.GetClosed())...) {
		if row.Closure != nil && row.GetClosure().GetAtMs() >= since {
			closed = append(closed, row)
		}
	}

	viewerRepository := ""
	for _, row := range live {
		if row.GetWorkspace().GetWorkspace().GetId() == viewer {
			viewerRepository = row.GetRepository().GetName()
		}
	}
	liveByName := rowsByName(live)
	closedByName := rowsByName(closed)

	targets := &frontendv1.WorkspaceMentionTargets{}
	add := func(row *frontendv1.RosterRow, carriers []*frontendv1.RosterRow, standing func(*frontendv1.WorkspaceMentionTarget)) {
		name := row.GetName().GetText()
		ref := row.GetWorkspace().GetWorkspace()
		if name == "" || mentionNamed(carriers, viewerRepository) != row || ref.GetId() == viewer {
			return
		}
		target := &frontendv1.WorkspaceMentionTarget{
			Name:      &frontendv1.WorkspaceMentionName{Text: name},
			Workspace: ref,
		}
		standing(target)
		if repository := row.GetRepository().GetName(); repository != "" {
			target.Repository = &frontendv1.WorkspaceMentionRepository{Name: repository}
		}
		targets.Targets = append(targets.Targets, target)
	}
	for _, row := range live {
		add(row, liveByName[row.GetName().GetText()], func(t *frontendv1.WorkspaceMentionTarget) {
			t.Standing = &frontendv1.WorkspaceMentionTarget_Open{Open: &frontendv1.WorkspaceMentionTargetOpen{}}
		})
	}
	for _, row := range closed {
		if len(liveByName[row.GetName().GetText()]) > 0 {
			continue
		}
		add(row, closedByName[row.GetName().GetText()], func(t *frontendv1.WorkspaceMentionTarget) {
			t.Standing = &frontendv1.WorkspaceMentionTarget_Closed{Closed: &frontendv1.WorkspaceMentionTargetClosed{}}
		})
	}
	return targets
}

// rowsByName groups the named rows of ROWS by name, each group in ROWS's order.
func rowsByName(rows []*frontendv1.RosterRow) map[string][]*frontendv1.RosterRow {
	byName := make(map[string][]*frontendv1.RosterRow, len(rows))
	for _, row := range rows {
		if name := row.GetName().GetText(); name != "" {
			byName[name] = append(byName[name], row)
		}
	}
	return byName
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
	// now is the clock the recently closed window is measured against.
	now func() time.Time
	// mergedRows reads every merged workspace's row (sidebar.Resolver's
	// MergedRows), which the roster no longer carries. It is read again
	// only when a push's recently merged stream differs from the one merged
	// was read for: the resolver reuses an unchanged stream's pointer, and
	// a merged row read on every push would resolve the whole merged band
	// for each viewer on every row change.
	mergedRows func() ([]*frontendv1.RosterRow, *frontendv1.RosterPush, bool)
	mergedFor  *frontendv1.RosterMergedSection
	merged     []*frontendv1.RosterRow
	last       *frontendv1.WorkspaceMentionTargets
}

// targets resolves PUSH's targets, reading the merged rows again when PUSH's
// recently merged stream is not the one they were read for.
func (t *mentionTargetsTracker) targets(push *frontendRoster) *frontendv1.WorkspaceMentionTargets {
	if section := push.GetRecentlyMerged(); t.mergedFor == nil || section != t.mergedFor {
		rows, _, ok := t.mergedRows()
		if !ok {
			// The roster topic delivered PUSH, so the resolver has published.
			panic("mention targets: the roster topic delivered a push the resolver says it never published")
		}
		t.mergedFor, t.merged = section, rows
	}
	return mentionTargets(push, t.merged, t.viewer, t.now())
}

// mentionTracker answers VIEWER's tracker, reading merged rows from
// the sidebar and the window's clock from the server.
func (s *server) mentionTracker(viewer string) *mentionTargetsTracker {
	return &mentionTargetsTracker{viewer: viewer, now: s.now, mergedRows: s.deps.Sidebar.MergedRows}
}

// next resolves ROSTER's targets, and whether they differ from the last set
// next answered (the first set always does).
func (t *mentionTargetsTracker) next(roster *frontendRoster) (*frontendv1.WorkspaceMentionTargets, bool) {
	set := t.targets(roster)
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
