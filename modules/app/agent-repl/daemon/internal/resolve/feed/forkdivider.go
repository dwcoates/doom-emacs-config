package feed

import (
	"math"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
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
	r.upsert(s, at, &frontendv1.FeedRow{
		Id: id,
		Row: &frontendv1.FeedRow_Separation{Separation: &frontendv1.FeedSessionSeparation{
			Label: &frontendv1.FeedSessionSeparationLabel{Text: forkPointLabel},
			Kind:  &frontendv1.FeedSessionSeparation_Forked{Forked: &frontendv1.FeedContextForked{}},
		}},
	}, true)
}

// forkPointLabel words the divider: what is above it, and what is below.
const forkPointLabel = "forked context above · this workspace's own context below"
