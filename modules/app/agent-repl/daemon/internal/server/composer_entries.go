package server

import (
	"context"
	"fmt"
	"slices"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/publish"
	"agent-repld/internal/resolve/feed"
)

const opComposerEntries = "daemon.server.composer_entries"

// THE COMPOSER'S ENTRIES. Emacs reports the entry labels its composer holds
// and the one point is on (ReportComposerEntries); the daemon holds the last
// report per workspace and RESTATES what follows from it whenever any input
// moves: a report, the selection, or a response's tree (the feed's
// WatchLabelChanges). One follower restates them, one workspace at a time, each
// after the change it answers, so the last change is always followed by a
// restatement that reads it (followComposerEntries).
//
// What a restatement publishes:
//   - the root feed's watch: the node point's entry answers
//     (frontend.v1.FeedComposerEntry), which the webapp highlights;
//   - the host view: the labels that answer nothing
//     (agentrepl.v1.HostComposerEntries), which Emacs marks.
//
// WHILE A PROMPT IS SELECTED the composer is editing a sent prompt's entry
// (selectEntry) or replying to a prompt: neither answers a response the
// person picked, so nothing is highlighted and no label is marked.

// composerReport is one workspace's composer as its last report stated it.
type composerReport struct {
	labels  []string
	atPoint string // "" when point is on no entry
	edits   uint64 // text edits the composer has had, relayed so the webapp can tell the person typed
}

// composerReply answers the reply row a composer's entries are read
// against: the selected row unless it is a prompt. The second result is
// false while a prompt is selected, when the composer answers no response.
// A prompt is told by the row itself, not the selection's kind: a prompt no
// rollback reaches is selected as a bubble.
func (s *server) composerReply(ws ids.WorkspaceID) (*frontendv1.FeedId, bool) {
	row, kind, held := s.currentSelection(ws)
	if !held {
		return nil, true
	}
	if kind == selectionPrompt {
		return nil, false
	}
	if text, ok := s.deps.Feed.SelectableText(ws, row); ok && text.Prompt {
		return nil, false
	}
	return row, true
}

// ReportComposerEntries holds the composer's reported entries and announces
// the workspace, so the follower restates what follows from them. See
// endpoint_report_composer_entries.proto.
func (s *server) ReportComposerEntries(
	ctx context.Context,
	req *connect.Request[agentreplv1.ReportComposerEntriesRequest],
) (*connect.Response[agentreplv1.ReportComposerEntriesResponse], error) {
	const rpc = "ReportComposerEntries"
	if err := validateReportComposerEntriesRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.ReportComposerEntriesResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	ws := subject.Record.ID
	report := composerReport{labels: slices.Clone(req.Msg.GetLabels()), atPoint: req.Msg.GetAtPoint().GetEntry().GetLabel(), edits: req.Msg.GetEdits()}
	s.composerMu.Lock()
	s.composerReports[ws] = report
	s.composerMu.Unlock()
	subject.Log.Debug(opComposerEntries, "the composer reported its entries", dlog.Context{
		"labels": len(report.labels), "at_point": report.atPoint,
	})
	s.composerChanges.Emit(ws)
	resp.Result = &agentreplv1.ReportComposerEntriesResponse_Success{Success: &agentreplv1.ReportComposerEntriesSuccess{}}
	return connect.NewResponse(resp), nil
}

// validateReportComposerEntriesRequest refuses a request naming no workspace,
// an empty label, or no entry at point, or a point entry the labels do not
// hold, as InvalidArgument.
func validateReportComposerEntriesRequest(req *agentreplv1.ReportComposerEntriesRequest) *connect.Error {
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	for _, label := range req.GetLabels() {
		if label == "" {
			return invalid("labels", "a label is never empty")
		}
	}
	switch at := req.GetAtPoint().GetAt().(type) {
	case *agentreplv1.ComposerEntryAtPoint_None:
	case *agentreplv1.ComposerEntryAtPoint_Entry:
		if !slices.Contains(req.GetLabels(), at.Entry.GetLabel()) {
			return invalid("at_point.entry.label", "the entry at point is one of the reported labels")
		}
	default:
		return invalid("at_point", "which entry point is on is required")
	}
	return nil
}

// FindEntryNeighbor answers the label a line opened beside an entry line
// answers. See endpoint_find_entry_neighbor.proto.
func (s *server) FindEntryNeighbor(
	ctx context.Context,
	req *connect.Request[agentreplv1.FindEntryNeighborRequest],
) (*connect.Response[agentreplv1.FindEntryNeighborResponse], error) {
	const rpc = "FindEntryNeighbor"
	if err := validateFindEntryNeighborRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.FindEntryNeighborResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	ws := subject.Record.ID
	label := req.Msg.GetLabel()
	way := neighborWayOf(req.Msg.GetDirection())
	success := &agentreplv1.FindEntryNeighborSuccess{Neighbor: &agentreplv1.FindEntryNeighborSuccess_None{None: &agentreplv1.FindEntryNeighborNone{}}}
	reply, answers := s.composerReply(ws)
	neighbor, ok := "", false
	if answers {
		neighbor, ok = s.deps.Feed.EntryNeighbor(ws, reply, label, way)
	}
	if ok {
		success.Neighbor = &agentreplv1.FindEntryNeighborSuccess_Found{Found: &agentreplv1.FindEntryNeighborFound{Label: neighbor}}
	}
	subject.Log.Debug(opComposerEntries, "answered an entry line's neighbor", dlog.Context{
		"label": label, "way": way.String(), "reply": reply.GetValue(), "answers": answers, "neighbor": neighbor, "found": ok,
	})
	resp.Result = &agentreplv1.FindEntryNeighborResponse_Success{Success: success}
	return connect.NewResponse(resp), nil
}

// neighborWayOf is the feed's NeighborWay for DIRECTION, which validation
// has seen name a way.
func neighborWayOf(direction *agentreplv1.EntryNeighborDirection) feed.NeighborWay {
	switch direction.GetWay().(type) {
	case *agentreplv1.EntryNeighborDirection_Next:
		return feed.NeighborNext
	case *agentreplv1.EntryNeighborDirection_Previous:
		return feed.NeighborPrevious
	case *agentreplv1.EntryNeighborDirection_Following:
		return feed.NeighborFollowing
	}
	panic(fmt.Sprintf("FindEntryNeighbor: validated direction carries way %T", direction.GetWay()))
}

// validateFindEntryNeighborRequest refuses a request naming no workspace, no
// label or no direction as InvalidArgument.
func validateFindEntryNeighborRequest(req *agentreplv1.FindEntryNeighborRequest) *connect.Error {
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	if req.GetLabel() == "" {
		return invalid("label", "the entry line's label is required")
	}
	if req.GetDirection().GetWay() == nil {
		return invalid("direction", "which neighbor of the node is asked for is required")
	}
	return nil
}

// followComposerEntries restates the composer's entries of every workspace a
// report, a selection or a response's tree announces, until ctx ends: the one
// consumer of both announcements, so restatements run one at a time.
func (s *server) followComposerEntries(ctx context.Context, own, labels <-chan ids.WorkspaceID) {
	for {
		var ws ids.WorkspaceID
		select {
		case <-ctx.Done():
			return
		case w, ok := <-own:
			if !ok {
				return
			}
			ws = w
		case w, ok := <-labels:
			if !ok {
				return
			}
			ws = w
		}
		s.restateComposerEntries(ctx, ws)
	}
}

// restateComposerEntries publishes what WS's reported composer entries
// answer: the node point's entry answers on the root feed's watch, and, when
// they moved, the labels that answer nothing on the host view.
func (s *server) restateComposerEntries(ctx context.Context, ws ids.WorkspaceID) {
	s.composerMu.Lock()
	report := s.composerReports[ws]
	s.composerMu.Unlock()
	reply, answers := s.composerReply(ws)
	var targets []*frontendv1.FeedId
	if answers {
		targets = s.deps.Feed.ComposerTargets(ws, reply, report.labels)
	}
	entry := composerEntryOf(report, answers, targets)
	unanswered := unansweredLabels(report, answers, targets)
	s.composerEntryTopic(ws).Publish(entry)
	s.composerMu.Lock()
	moved := !slices.Equal(s.composerUnanswered[ws], unanswered)
	if moved {
		s.composerUnanswered[ws] = unanswered
	}
	s.composerMu.Unlock()
	s.log.Debug(opComposerEntries, "restated what the composer's entries answer", dlog.Context{
		"workspace": string(ws), "labels": len(report.labels), "at_point": report.atPoint,
		"reply": reply.GetValue(), "answers": answers, "highlighted": entry.GetNode().GetRow().GetValue(),
		"unanswered": len(unanswered), "unanswered_moved": moved,
	})
	if moved {
		s.PublishHostWorkspace(ctx, ws)
	}
}

// composerEntryOf is the node REPORT's entry at point answers, given each
// reported label's target (TARGETS, in REPORT's order): none while the
// composer ANSWERS no response, point is on no entry, or its label targets
// nothing.
func composerEntryOf(report composerReport, answers bool, targets []*frontendv1.FeedId) *frontendv1.FeedComposerEntry {
	none := &frontendv1.FeedComposerEntry{At: &frontendv1.FeedComposerEntry_None{None: &frontendv1.FeedComposerEntryNone{}}, Edits: report.edits}
	if !answers || report.atPoint == "" {
		return none
	}
	i := slices.Index(report.labels, report.atPoint)
	if i < 0 || targets[i] == nil {
		return none
	}
	return &frontendv1.FeedComposerEntry{At: &frontendv1.FeedComposerEntry_Node{Node: &frontendv1.FeedComposerEntryNode{
		Row: targets[i], Label: report.atPoint,
	}}, Edits: report.edits}
}

// unansweredLabels is every label of REPORT whose target (TARGETS) is
// nothing, in order, each once; nothing while the composer ANSWERS no
// response.
func unansweredLabels(report composerReport, answers bool, targets []*frontendv1.FeedId) []string {
	var out []string
	if !answers {
		return out
	}
	for i, label := range report.labels {
		if targets[i] == nil && !slices.Contains(out, label) {
			out = append(out, label)
		}
	}
	return out
}

// composerUnansweredOf answers WS's labels that answer nothing, as last
// restated, for the host view.
func (s *server) composerUnansweredOf(ws ids.WorkspaceID) []string {
	s.composerMu.Lock()
	defer s.composerMu.Unlock()
	return slices.Clone(s.composerUnanswered[ws])
}

// composerEntryTopic answers a workspace's composer-entry topic, creating it
// on first use. It replays its latest value, so a root feed watch accepted
// mid-edit draws the highlight that stands.
func (s *server) composerEntryTopic(ws ids.WorkspaceID) *publish.Topic[*frontendv1.FeedComposerEntry] {
	return perWorkspace(s, s.composerEntryTopics, ws)
}
