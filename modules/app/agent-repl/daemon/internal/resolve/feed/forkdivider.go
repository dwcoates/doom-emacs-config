package feed

import (
	"math"

	"agentrepl/logging/logsite"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
)

// THE FORK-POINT DIVIDER: a grey line between the context a forked workspace
// inherited (above) and the context it made itself (below).
//
// It is drawn with the FIRST page read of each inherited generation, and keyed
// at the generation's own bound: the place of the fork point, after every
// entry the generation could have placed at or before it. That puts it below
// every row of the generation and above everything nearer the fork
// (order.go), whatever pages are loaded, withheld or still to come. It is
// identified by the generation's book, so a re-read of the same page restates
// the one row, and a grandparent generation gets its own line beneath the
// parent's. It cuts no context, so it bounds neither the feed's opening nor
// its history (cutsContext, endsHistory).

// drawForkPoint draws the fork-point divider under the inherited generation
// SEGMENT. Called with r.mu held, while the segment's first page replays.
func (r *resolver) drawForkPoint(s *wsState, agent *conversationv1.AgentId, segment *LineageSegment) {
	at, ok := r.place(s, agent)
	if !ok {
		return
	}
	defer s.placingEntry(&conversationv1.ConversationPlace{AtMs: segment.ThroughMs, Ordinal: math.MaxUint32})()
	id := r.rowID(s.id, at.feed, feedid.RowKey{Kind: feedid.KindSeparation, ID: "fork_point:" + segment.Book.GetValue()})
	r.logger(s.id).Debug("daemon.feed.separation",
		"a fork-point divider was drawn under an inherited generation",
		dlog.Context{"row": id.GetValue(), "kind": "forked", "book": segment.Book.GetValue()})
	label, forked := forkPointLine(segment.Workspace, r.forkSourceName(s, segment.Workspace))
	r.upsert(s, at, &frontendv1.FeedRow{
		Id: id,
		Row: &frontendv1.FeedRow_Separation{Separation: &frontendv1.FeedSessionSeparation{
			Label: &frontendv1.FeedSessionSeparationLabel{Text: label},
			Kind:  &frontendv1.FeedSessionSeparation_Forked{Forked: forked},
		}},
	}, true)
}

// forkSourceName is the name of the workspace the inherited generation came
// from, "" when it cannot be named (the read is recorded at ERROR). Called
// with r.mu held.
func (r *resolver) forkSourceName(s *wsState, source ids.WorkspaceID) string {
	name, _ := r.namedWorkspace(source, s.id, nameRecords{
		failedKey: "daemon.feed.fork_source_name_failed",
		emptyKey:  "daemon.feed.fork_source_name_empty",
		failed:    logsite.Msg("the workspace a fork's inherited context came from could not be named; the fork-point divider names no workspace and links nothing"),
		empty:     logsite.Msg("the workspace a fork's inherited context came from has an empty name; the fork-point divider names no workspace and links nothing"),
	})
	return name
}

// forkPointLabelTail ends the divider's label: what is below the line.
const forkPointLabelTail = " context above · this workspace's own context below"

// forkPointLine words the divider for the generation inherited from SOURCE,
// named NAME ("" when unnamed): the label, and the fork payload that names
// the workspace for the client to draw and link. The name appears verbatim in
// the label (frontend.v1.FeedContextForkedSource.name).
func forkPointLine(source ids.WorkspaceID, name string) (string, *frontendv1.FeedContextForked) {
	if name == "" {
		return "forked" + forkPointLabelTail, &frontendv1.FeedContextForked{}
	}
	return "forked " + name + forkPointLabelTail, &frontendv1.FeedContextForked{
		Source: &frontendv1.FeedContextForkedSource{Workspace: &workspacev1.WorkspaceRef{Id: string(source)}, Name: name},
	}
}
