package feed

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
	"agent-repld/internal/sessionwatcher"
)

// A PAGE CARRIES THE PROMPTS OF THE TURNS IT TOUCHES (docs/protobuf-design/
// complete-pages.md; conversation.v1 HistoryPage.turn_heads). A long turn's
// newest store page holds only its steps, and an entry of a turn whose prompt
// this feed has not drawn is withheld until that prompt's page loads
// (withholds, book.go): before heads, an opening walked older pages until one
// drew a row. With heads, a loaded page FIRST draws the prompt of every turn
// it touches whose prompt sits below it, so its own entries are no longer
// withheld and an opening needs one store read.
//
//   - UNDER EACH DRAWN HEAD stands a FeedRow `earlier_steps`: the steps
//     between that prompt and the page's rows are on older pages not yet
//     loaded. It is keyed from the head's own conversation place, so it sorts
//     right after the prompt and before every later entry of the turn.
//   - THE WALK CLOSES THE GAP. A head is also an ordinary entry of an older
//     page; when a load meets it there, every step between it and the newer
//     pages is loaded (pages are contiguous), and the row is removed with the
//     `removed` arm.
//   - A HEAD MET AGAIN IS ABSORBED BY IDENTITY: a head whose turn this feed has
//     already seen opened (its prompt drawn, by a page or live) draws nothing
//     more, so neither the prompt nor its gap row is ever duplicated.
//   - A PAGE WITHOUT HEADS (a shim that serves none) draws exactly as before:
//     withheld entries and the walk that completes them.
//
// Only a reader's load draws heads. A watch's catch-up page carries what was
// written after what the daemon holds, never a page a reader stands on.

// earlierStepsSub is the row key's sub-part of a turn's earlier-steps row,
// beside the prompt's own row of the same turn.
const earlierStepsSub = "earlier_steps"

// gapRow is one earlier-steps row a book's loads drew: the feed it stands in
// (the head prompt's own feed, which an addressed turn puts in its tab) and
// its identity.
type gapRow struct {
	feed feedid.Feed
	id   string
}

// withHeads answers the entries a page draws, NEWEST FIRST as the page serves
// them: the page's own entries, then, for a reader's load, its turn heads,
// which are placed below every entry of the page. The second answer is how
// many leading entries are the page's own.
func withHeads(page *conversationv1.HistoryPage, load *pageLoad) ([]*conversationv1.HistoryEntryAt, int) {
	own := page.GetEntries()
	if load == nil || len(page.GetTurnHeads()) == 0 {
		return own, len(own)
	}
	all := make([]*conversationv1.HistoryEntryAt, 0, len(own)+len(page.GetTurnHeads()))
	all = append(all, own...)
	all = append(all, page.GetTurnHeads()...)
	return all, len(own)
}

// headsPage answers the page whose entries are ALL, for the reads taken before
// the lock (closes, addresses, mirrors, lineage), which must know the heads'
// turns as well as the page's own.
func headsPage(page *conversationv1.HistoryPage, all []*conversationv1.HistoryEntryAt) *conversationv1.HistoryPage {
	if len(all) == len(page.GetEntries()) {
		return page
	}
	return &conversationv1.HistoryPage{Entries: all}
}

// replayHead draws one turn head of a reader's load: its prompt, and the
// earlier-steps row under it. Called with r.mu held, inside the page's replay.
func (r *resolver) replayHead(s *wsState, agent *conversationv1.AgentId, head *conversationv1.HistoryEntryAt, load *pageLoad) {
	log := r.logger(s.id)
	turn := ids.TurnID(head.GetEntry().GetUserPrompt().GetId().GetValue())
	if turn == "" {
		// A HEAD IS A PROMPT ENTRY by contract (HistoryPage.turn_heads). One
		// that is not names no turn it could open: it is not drawn.
		log.Error("daemon.feed.turn_head_not_prompt",
			"a page's turn head is not a prompt entry naming its turn; it was not drawn",
			dlog.Context{"feed": load.feedKey, "agent": agent.GetValue(), "kind": entryKind(head)})
		load.outcomes.note(head, outcomeHeadRefused)
		return
	}
	if s.knownTurns[turn] {
		log.Debug("daemon.feed.turn_head_absorbed",
			"a page's turn head names a turn this feed already saw opened; it drew nothing more",
			dlog.Context{"feed": load.feedKey, "turn": string(turn)})
		load.outcomes.note(head, outcomeHeadAbsorbed)
		return
	}
	r.replayLoadedEntry(s, agent, head, load, outcomeHead)
	load.heads++
	draw := func() { r.drawEarlierSteps(s, head, turn, load) }
	if classAgent, classTurn := entryClass(head, agent); r.inherits(s, classAgent, classTurn) {
		r.drawInherited(s, draw)
		return
	}
	draw()
}

// drawEarlierSteps draws the row under TURN's head saying its earlier steps
// are on an older page, in the head prompt's own feed, keyed from the head's
// own place so it sorts right after the prompt.
func (r *resolver) drawEarlierSteps(s *wsState, head *conversationv1.HistoryEntryAt, turn ids.TurnID, load *pageLoad) {
	at := r.outputPlacement(s, &turn)
	stamp := &conversationv1.TurnId{Value: string(turn)}
	defer s.drawingEntry(stamp)()
	defer s.placingEntry(sessionwatcher.PlaceOf(head))()
	row := &frontendv1.FeedRow{
		Id:   r.rowID(s.id, at.feed, feedid.RowKey{Kind: feedid.KindPrompt, ID: string(turn), Sub: earlierStepsSub}),
		Turn: stamp,
		Row:  &frontendv1.FeedRow_EarlierSteps{EarlierSteps: &frontendv1.FeedTurnEarlierSteps{}},
	}
	r.upsert(s, at, row, true)
	if load.book.gaps == nil {
		load.book.gaps = map[ids.TurnID]gapRow{}
	}
	load.book.gaps[turn] = gapRow{feed: at.feed, id: row.GetId().GetValue()}
	r.logger(s.id).Info("daemon.feed.earlier_steps_drawn",
		"a turn's prompt was drawn from a page's turn heads; its earlier steps are on an older page, so a fold stands under it until the walk loads them",
		dlog.Context{"feed": load.feedKey, "turn": string(turn), "row": row.GetId().GetValue()})
}

// closeGap removes the earlier-steps row of the turn whose prompt AT is, when
// one stands: a load met the head as an ordinary entry of an older page, so
// every step between it and the newer pages is now loaded. Called with r.mu
// held.
func (r *resolver) closeGap(s *wsState, at *conversationv1.HistoryEntryAt, load *pageLoad) {
	turn := ids.TurnID(at.GetEntry().GetUserPrompt().GetId().GetValue())
	if turn == "" {
		return
	}
	gap, open := load.book.gaps[turn]
	if !open {
		return
	}
	delete(load.book.gaps, turn)
	removed := r.retire(s, gap.feed, gap.id)
	r.logger(s.id).Info("daemon.feed.earlier_steps_removed",
		"the walk loaded the page holding a turn's prompt, so every earlier step of it is loaded and its fold was removed",
		dlog.Context{"feed": load.feedKey, "turn": string(turn), "row": gap.id, "stood": removed})
}
