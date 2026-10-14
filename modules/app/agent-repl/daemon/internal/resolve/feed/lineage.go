package feed

import (
	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
)

// A FORK'S INHERITED PAST IS ITS PARENT'S BOOK (docs/protobuf-design/
// fork-lineage.md).
//
// A forked workspace's own book holds only what the fork itself said: the file
// plane never stores the records the vendor's fork copied from the parent. The
// conversation it inherited is read from the PARENT's book, bounded to the fork
// point, and — when the parent is itself a fork — from the grandparent's below
// that, one segment per generation. The root feed's page walk reads the fork's
// own book to its start and then each segment in turn (book.go).
//
// INHERITED ROWS ARE DRAWN BY THE SAME FUNCTIONS AS EVERY OTHER ROW, so they
// read exactly as the parent's feed draws them (the owner's principle): their
// final answers, caps and turn endings come from the parent's own prompts and
// terminals, which are on those pages. Two things alone mark them:
//
//   - They are drawn in the inherited PLANE, which orders them below everything
//     the fork has of its own and keeps them off the live push: pages serve
//     them, as a reader's "load more" asks for them.
//   - Their agent is a segment's book rather than the fork's main agent, and it
//     is a ROOT agent all the same: the parent's main agent's rows are the root
//     feed's rows in the fork too (agentFeed, rootFeedAgent).

// LineageSegment is one generation of a fork's inherited past: a book, read up
// to a bound, without the turns that were open when the fork was taken.
type LineageSegment struct {
	// Workspace is the workspace whose turns the book's entries are: the
	// parent, whose durable turn records (closes) describe them.
	Workspace ids.WorkspaceID
	// Book is the agent the parent's history is filed under.
	Book *conversationv1.AgentId
	// ThroughMs is the inclusive upper bound, epoch ms, on the places of the
	// book's inherited entries.
	ThroughMs int64
	// Uninherited are the parent's turns open at the fork: an entry of one of
	// them is not the fork's, wherever it is placed.
	Uninherited map[ids.TurnID]bool
}

// NoteLineage tells the resolver where WS's inherited past is read from,
// nearest generation first; nil for a workspace that is no fork. The fleet
// states it at every bring-up, before a reader can page past the workspace's
// own book.
func (r *resolver) NoteLineage(ws ids.WorkspaceID, segments []LineageSegment) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	s.lineage = segments
	s.inheritedAgents = make(map[string]bool, len(segments))
	books := make([]string, 0, len(segments))
	for _, segment := range segments {
		s.inheritedAgents[segment.Book.GetValue()] = true
		books = append(books, segment.Book.GetValue())
	}
	r.logger(ws).Info("daemon.feed.lineage",
		"the workspace's inherited past is read from these books below its own",
		dlog.Context{"generations": len(segments), "books": books})
}

// rootFeedAgent reports whether ID's rows are the root feed's: the session's
// main agent, or the book of a generation the workspace inherited from.
func (s *wsState) rootFeedAgent(id string) bool {
	return id != "" && (id == s.mainAgent || s.inheritedAgents[id])
}

// agentFeed is the feed an agent's rows go on: the root for the main agent and
// for every book the workspace inherited from, the agent's own sub-feed
// otherwise (feedid.AgentFeed).
func (s *wsState) agentFeed(id string) (feedid.Feed, error) {
	if id != "" && s.inheritedAgents[id] {
		return feedid.Feed{Root: true}, nil
	}
	return feedid.AgentFeed(id, s.mainAgent)
}

// entryTurn is the turn an entry belongs to: a prompt's own, or the entry's
// stamp ("" when it carries none).
func entryTurn(at *conversationv1.HistoryEntryAt) ids.TurnID {
	if prompt := at.GetEntry().GetUserPrompt(); prompt != nil {
		return ids.TurnID(prompt.GetId().GetValue())
	}
	return ids.TurnID(at.GetTurn().GetValue())
}

// inheritedEntries answers PAGE without the entries, and the turn heads, of
// turns the fork did not inherit, and how many entries and heads it left out.
// The page's boundary (`more`, `floor`) is kept as the store served it, so the
// walk continues from the store's own pointer whatever was left out.
func inheritedEntries(page *conversationv1.HistoryPage, uninherited map[ids.TurnID]bool) (*conversationv1.HistoryPage, int) {
	if len(uninherited) == 0 {
		return page, 0
	}
	kept := make([]*conversationv1.HistoryEntryAt, 0, len(page.GetEntries()))
	for _, at := range page.GetEntries() {
		if !uninherited[entryTurn(at)] {
			kept = append(kept, at)
		}
	}
	heads := make([]*conversationv1.HistoryTurnHead, 0, len(page.GetHeads()))
	for _, head := range page.GetHeads() {
		if !uninherited[entryTurn(head.GetEntry())] {
			heads = append(heads, head)
		}
	}
	dropped := len(page.GetEntries()) - len(kept) + len(page.GetHeads()) - len(heads)
	if dropped == 0 {
		return page, 0
	}
	return &conversationv1.HistoryPage{Entries: kept, Heads: heads, Boundary: page.GetBoundary()}, dropped
}

// entryTurns names every turn a page's entries belong to, for the durable
// records read before the page is drawn.
func entryTurns(page *conversationv1.HistoryPage) []ids.TurnID {
	var out []ids.TurnID
	for _, at := range page.GetEntries() {
		if turn := at.GetTurn().GetValue(); turn != "" {
			out = append(out, ids.TurnID(turn))
		}
		if turn := at.GetEntry().GetUserPrompt().GetId().GetValue(); turn != "" {
			out = append(out, ids.TurnID(turn))
		}
	}
	return out
}
