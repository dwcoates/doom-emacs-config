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
// complete-pages.md; conversation.v1 HistoryPage.heads). A long turn's
// newest store page holds only its steps, and an entry of a turn whose prompt
// this feed has not drawn is withheld until that prompt's page loads
// (withholds, book.go): before heads, an opening walked older pages until one
// drew a row. With heads, a loaded page FIRST draws the prompt of every turn
// it touches whose prompt sits below it, so its own entries are no longer
// withheld and an opening needs one store read.
//
//   - UNDER A DRAWN HEAD WHOSE `between` IS `steps_between` stands a FeedRow
//     `earlier_steps`: the steps between that prompt and the page's rows are
//     on older pages not yet loaded. It is keyed from the head's own
//     conversation place, so it sorts right after the prompt and before every
//     later entry of the turn. A head that is `adjacent` draws no such row:
//     the page's entries of the turn follow its prompt directly, so there is
//     nothing a walk would load and no row for one to remove.
//   - A HEAD WITH NO `between` ARM IS REFUSED, never drawn on a guess either
//     way: its turn's entries are withheld as on a page without heads.
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
// them: the page's own entries, then, for a reader's load, its turn heads'
// prompt entries, which are placed below every entry of the page. The second
// answer is how many leading entries are the page's own: entry own+i is the
// prompt of page.GetHeads()[i].
func withHeads(page *conversationv1.HistoryPage, load *pageLoad) ([]*conversationv1.HistoryEntryAt, int) {
	own := page.GetEntries()
	heads := page.GetHeads()
	if load == nil || len(heads) == 0 {
		return own, len(own)
	}
	all := make([]*conversationv1.HistoryEntryAt, 0, len(own)+len(heads))
	all = append(all, own...)
	for _, head := range heads {
		all = append(all, head.GetEntry())
	}
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

// replayHead draws one turn head of a reader's load: its prompt, and, when
// steps sit between it and the page, the earlier-steps row under it. Called
// with r.mu held, inside the page's replay.
func (r *resolver) replayHead(s *wsState, agent *conversationv1.AgentId, head *conversationv1.HistoryTurnHead, load *pageLoad) {
	log := r.logger(s.id)
	prompt := head.GetEntry()
	turn := ids.TurnID(prompt.GetEntry().GetUserPrompt().GetId().GetValue())
	if turn == "" {
		// A HEAD IS A PROMPT ENTRY by contract (HistoryPage.heads). One that is
		// not names no turn it could open: it is not drawn.
		log.Error("daemon.feed.turn_head_not_prompt",
			"a page's turn head is not a prompt entry naming its turn; it was not drawn",
			dlog.Context{"feed": load.feedKey, "agent": agent.GetValue(), "kind": entryKind(prompt)})
		load.outcomes.note(prompt, outcomeHeadRefused)
		return
	}
	if head.GetBetween() == nil {
		// EVERY HEAD SAYS WHETHER STEPS SIT BETWEEN by contract
		// (HistoryTurnHead.between). Drawing it would mean guessing whether a
		// fold belongs under it, so it is not drawn: its turn's entries are
		// withheld and the walk completes them, as on a page without heads.
		log.Error("daemon.feed.turn_head_no_between",
			"a page's turn head says nothing of whether steps sit between it and the page; it was not drawn",
			dlog.Context{"feed": load.feedKey, "agent": agent.GetValue(), "turn": string(turn)})
		load.outcomes.note(prompt, outcomeHeadRefused)
		return
	}
	if s.knownTurns[turn] {
		log.Debug("daemon.feed.turn_head_absorbed",
			"a page's turn head names a turn this feed already saw opened; it drew nothing more",
			dlog.Context{"feed": load.feedKey, "turn": string(turn)})
		load.outcomes.note(prompt, outcomeHeadAbsorbed)
		return
	}
	r.replayLoadedEntry(s, agent, prompt, load, outcomeHead)
	load.heads++
	if head.GetAdjacent() != nil {
		log.Debug("daemon.feed.turn_head_adjacent",
			"a turn's prompt was drawn from a page's turn heads and the page's entries of the turn follow it directly; no earlier-steps row was drawn",
			dlog.Context{"feed": load.feedKey, "turn": string(turn)})
		return
	}
	// THE ROW IS DRAWN IN THE PAGE'S OWN PLANE: a head of a fork's inherited
	// past is drawn in the inherited plane its page is replayed in.
	r.drawEarlierSteps(s, prompt, turn, load)
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
