package feed

import (
	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"google.golang.org/protobuf/proto"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/holds"
	"agent-repld/internal/wsm"
)

// THE HELD SECTION (frontend.v1 FeedHeld, feed_held.proto): the part of a
// workspace's root feed that holds what has not yet settled into the
// conversation's history — a prompt the daemon holds for delivery, a question
// waiting for an answer. THIS FILE IS ITS ONE OWNER. (A running merge's bubble
// is not held: it is a row pinned to the feed's bottom, FeedRowOrder.pinned.)
//
// HELD, THEN SETTLED, IN ONE PUBLICATION. A held prompt settles when its row
// is drawn: its acceptance mirror (UpsertAtTurnAddress), or the resolver's own
// draw of the same prompt row. The row and the section without the item are logged as
// ONE publication of the root feed (emitRow, publishHeld), so no reader draws
// the item twice or not at all. A prompt's mirror taken back down (a delivery
// that found no session) returns the prompt to the section the same way.
//
// The prompt half is whatever the prompt queue says is standing
// (SetHeldPrompts), minus the prompts whose row is already drawn. The state
// lives on the resolver rather than on wsState, because a bind's reset empties
// the feed and must not forget what the queue is still holding.

// The operations this file's records carry.
const opHeld = "daemon.feed.held"

// heldState is one workspace's held section accumulation.
type heldState struct {
	// prompts are the standing holds as the prompt queue last stated them.
	prompts []wsm.HeldPrompt
	// editing is the held prompt an EditHeldPrompt claim stands on, empty when
	// none does.
	editing ids.TurnID
	// loading and relaunching are the shim start facts the Sink is told
	// (holds.StartOf).
	loading, relaunching bool
	// published is the section as last published; nil until the first.
	published *frontendv1.FeedHeld
	// ingress are the held-prompt ingress's unaccepted entries as the ingress
	// last stated them, in the order they were written.
	ingress []holds.IngressEntry
	// claims names the turn each ingress entry is being submitted as, by its
	// idempotency key (ClaimIngress).
	claims map[string]ids.TurnID
	// settled names the ingress entries whose claimed turn has been drawn as
	// a held prompt or a prompt row: they are drawn as that and nowhere else,
	// whatever happens to the turn after.
	settled map[string]bool
}

// heldOf resolves a workspace's held section state, creating it on first sight.
// The caller holds the lock.
func (r *resolver) heldOf(ws ids.WorkspaceID) *heldState {
	h, ok := r.held[ws]
	if !ok {
		h = &heldState{claims: map[string]ids.TurnID{}, settled: map[string]bool{}}
		r.held[ws] = h
	}
	return h
}

// rootFeed is the workspace's root feed state.
func (r *resolver) rootFeed(s *wsState) *feedState {
	return r.feed(s, feedid.Feed{Root: true})
}

// SetHeldPrompts installs a workspace's standing holds, whole, and publishes
// the section when that changed it.
func (r *resolver) SetHeldPrompts(ws ids.WorkspaceID, held []wsm.HeldPrompt) {
	r.installHeld(ws, held, nil)
}

// FoldHeldPrompts installs a workspace's standing holds, whole, in the change
// FOLD made, and publishes the section naming the fold.
func (r *resolver) FoldHeldPrompts(ws ids.WorkspaceID, held []wsm.HeldPrompt, fold holds.Fold) {
	r.installHeld(ws, held, []holds.Fold{fold})
}

// installHeld is SetHeldPrompts and FoldHeldPrompts' one body: FOLDS is
// empty for a change that folded nothing.
func (r *resolver) installHeld(ws ids.WorkspaceID, held []wsm.HeldPrompt, folds []holds.Fold) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	r.heldOf(ws).prompts = held
	r.logger(ws).Debug(opHeld, "the held section took the standing holds", dlog.Context{
		"held": len(held), "folds": len(folds),
	})
	r.publishHeld(s, nil, folds)
}

// SetEditing names the held prompt being edited, or clears it with "".
func (r *resolver) SetEditing(ws ids.WorkspaceID, turn ids.TurnID) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	r.heldOf(ws).editing = turn
	r.logger(ws).Debug(opHeld, "the held section took the edit standing", dlog.Context{"editing_turn": string(turn)})
	r.publishHeld(s, nil, nil)
}

// SetShimLoading says whether a start of the workspace's shim is under way,
// which badges the prompts held for its session.
func (r *resolver) SetShimLoading(ws ids.WorkspaceID, loading bool) {
	r.setShimStart(ws, func(h *heldState) { h.loading = loading })
}

// SetRelaunching says whether the start under way relaunches a shim that
// died, which badges the prompts held for its session.
func (r *resolver) SetRelaunching(ws ids.WorkspaceID, relaunching bool) {
	r.setShimStart(ws, func(h *heldState) { h.relaunching = relaunching })
}

// setShimStart applies one shim start fact, republishing the section when the
// start it answers moved.
func (r *resolver) setShimStart(ws ids.WorkspaceID, set func(*heldState)) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	h := r.heldOf(ws)
	before := holds.StartOf(h.loading, h.relaunching)
	set(h)
	after := holds.StartOf(h.loading, h.relaunching)
	if before == after {
		return
	}
	r.logger(ws).Debug(opHeld, "the held section took the shim start under way", dlog.Context{
		"before": before.String(), "after": after.String(),
	})
	r.publishHeld(s, nil, nil)
}

// currentHeld answers the section as it stands at this moment of the root
// feed's log: the one last published, or the empty section when none ever was
// (which it then records as published, so the next change is judged against
// what this reader was served). The caller holds the lock.
func (r *resolver) currentHeld(s *wsState) *frontendv1.FeedHeld {
	h := r.heldOf(s.id)
	if h.published == nil {
		h.published = &frontendv1.FeedHeld{}
	}
	return h.published
}

// publishHeld publishes the section on the root feed when it changed, or
// whenever ROWS moved or FOLDS were made with it: the section, the rows and
// the folds in one publication. The caller holds the lock.
func (r *resolver) publishHeld(s *wsState, rows []*frontendv1.FeedRow, folds []holds.Fold) {
	h := r.heldOf(s.id)
	section := r.renderHeld(s, h)
	log := r.logger(s.id)
	if len(rows) == 0 && len(folds) == 0 && h.published != nil && proto.Equal(section, h.published) {
		log.Debug(opHeld, "the held section is unchanged; nothing was published", dlog.Context{"items": len(section.GetItems())})
		return
	}
	h.published = section
	moved := make([]string, 0, len(rows))
	for _, row := range rows {
		moved = append(moved, row.GetId().GetValue())
	}
	wire := r.heldFolds(s, folds)
	log.Info(opHeld, "the held section changed and was published on the root feed", dlog.Context{
		"items": len(section.GetItems()), "moved_rows": moved, "folds": len(wire),
	})
	r.emit(s, r.rootFeed(s), Publication{Held: &agentreplv1.WatchFeedHeldChange{Held: section, Rows: rows, Folds: wire}})
}

// heldFolds names each fold by the two held items' ids: the ids each prompt's
// card is keyed by, the same ids its row would carry once settled. The caller
// holds the lock.
func (r *resolver) heldFolds(s *wsState, folds []holds.Fold) []*agentreplv1.WatchFeedHeldFold {
	out := make([]*agentreplv1.WatchFeedHeldFold, 0, len(folds))
	for _, fold := range folds {
		_, folded := r.promptRowFor(s, fold.From)
		_, into := r.promptRowFor(s, fold.Into)
		out = append(out, &agentreplv1.WatchFeedHeldFold{Folded: folded, Into: into})
	}
	return out
}

// renderHeld composes the section from its accumulation: the held prompts
// whose row is not drawn yet, in queue order. The caller holds the lock.
func (r *resolver) renderHeld(s *wsState, h *heldState) *frontendv1.FeedHeld {
	log := r.logger(s.id)
	items := make([]*frontendv1.FeedHeldItem, 0, len(h.prompts))
	draw := func(content *conversationv1.UserContent) *frontendv1.FeedUserPromptBody {
		return &frontendv1.FeedUserPromptBody{Blocks: r.drawUserBlocks(s, content)}
	}
	for _, card := range holds.Prompts(h.prompts, h.editing, holds.StartOf(h.loading, h.relaunching), draw, log) {
		id, drawn := r.promptRowDrawn(s, card.Turn)
		if drawn {
			log.Debug(opHeld, "a standing hold's prompt row is drawn; it has settled out of the section", dlog.Context{
				"turn": string(card.Turn), "row": id.GetValue(),
			})
			continue
		}
		items = append(items, &frontendv1.FeedHeldItem{
			Id:   id,
			Item: &frontendv1.FeedHeldItem_Prompt{Prompt: card.Prompt},
		})
	}
	for _, entry := range h.ingress {
		if r.ingressSettled(s, h, entry.Key) {
			continue
		}
		items = append(items, &frontendv1.FeedHeldItem{
			Id:   r.ingressItemID(s, entry.Key),
			Item: &frontendv1.FeedHeldItem_Ingress{Ingress: holds.IngressPrompt(entry, draw)},
		})
	}
	return &frontendv1.FeedHeld{Items: items}
}

// ingressItemID is the identity an ingress entry's item carries: its own, by
// its idempotency key, on the root feed.
func (r *resolver) ingressItemID(s *wsState, key string) *frontendv1.FeedId {
	return r.rowID(s.id, feedid.Feed{Root: true}, feedid.RowKey{Kind: feedid.KindIngress, ID: key})
}

// ingressSettled reports whether the ingress entry KEY is drawn as what it
// settled into: its claimed turn stands as a held prompt, or its prompt row is
// drawn. The first such reading is recorded, so the entry never returns to the
// section once it has settled, whatever its turn does next. The caller holds
// the lock.
func (r *resolver) ingressSettled(s *wsState, h *heldState, key string) bool {
	if h.settled[key] {
		return true
	}
	turn, claimed := h.claims[key]
	if !claimed {
		return false
	}
	_, drawn := r.promptRowDrawn(s, turn)
	if !drawn && !holdStands(h.prompts, turn) {
		return false
	}
	h.settled[key] = true
	r.logger(s.id).Debug(opHeld, "an ingress entry settled into its turn; the section draws it as that", dlog.Context{
		"idempotency_key": key, "turn": string(turn), "row_drawn": drawn,
	})
	return true
}

// holdStands reports whether TURN is a standing (unretired) hold in HELD.
func holdStands(held []wsm.HeldPrompt, turn ids.TurnID) bool {
	for _, h := range held {
		if h.Turn == turn && h.Tombstone == nil {
			return true
		}
	}
	return false
}

// SetIngressPending installs a workspace's unaccepted ingress entries, whole,
// and publishes the section when that changed it. A claim or a settlement of
// an entry no longer stated is forgotten with it.
func (r *resolver) SetIngressPending(ws ids.WorkspaceID, entries []holds.IngressEntry) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	h := r.heldOf(ws)
	h.ingress = entries
	stated := make(map[string]bool, len(entries))
	for _, entry := range entries {
		stated[entry.Key] = true
	}
	for key := range h.claims {
		if !stated[key] {
			delete(h.claims, key)
		}
	}
	for key := range h.settled {
		if !stated[key] {
			delete(h.settled, key)
		}
	}
	r.logger(ws).Debug(opHeld, "the held section took the ingress's unaccepted entries", dlog.Context{"entries": len(entries)})
	r.publishHeld(s, nil, nil)
}

// ClaimIngress names the turn the ingress entry KEY is being submitted as. It
// changes nothing drawn by itself: the entry leaves the section in the
// publication that draws the turn.
func (r *resolver) ClaimIngress(ws ids.WorkspaceID, key string, turn ids.TurnID) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	h := r.heldOf(ws)
	h.claims[key] = turn
	r.logger(ws).Debug(opHeld, "an ingress entry's turn was claimed", dlog.Context{"idempotency_key": key, "turn": string(turn)})
	r.publishHeld(s, nil, nil)
}

// DropIngress removes the ingress entry KEY, which the queue answered, and
// publishes the section when that changed it.
func (r *resolver) DropIngress(ws ids.WorkspaceID, key string) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	h := r.heldOf(ws)
	kept := h.ingress[:0:0]
	for _, entry := range h.ingress {
		if entry.Key != key {
			kept = append(kept, entry)
		}
	}
	h.ingress = kept
	delete(h.claims, key)
	delete(h.settled, key)
	r.logger(ws).Debug(opHeld, "the queue answered an ingress entry; the section dropped it", dlog.Context{"idempotency_key": key})
	r.publishHeld(s, nil, nil)
}

// promptRowFor is the identity TURN's prompt row carries, on the feed the turn
// draws on: the one its acceptance mirror and the resolver's own draw share.
func (r *resolver) promptRowFor(s *wsState, turn ids.TurnID) (placement, *frontendv1.FeedId) {
	at := r.outputPlacement(s, &turn)
	return at, r.rowID(s.id, at.feed, feedid.RowKey{Kind: feedid.KindPrompt, ID: string(turn)})
}

// promptRowDrawn answers TURN's prompt row identity, and whether that row is
// drawn on its feed: a held prompt whose row is drawn has settled.
func (r *resolver) promptRowDrawn(s *wsState, turn ids.TurnID) (*frontendv1.FeedId, bool) {
	at, id := r.promptRowFor(s, turn)
	f, ok := s.feeds[r.feedKey(s.id, at.feed)]
	if !ok {
		return id, false
	}
	_, drawn := f.rows[id.GetValue()]
	return id, drawn
}

// heldPromptRow reports whether ID is the prompt row of a prompt the queue
// still holds: the row whose drawing (or retirement) moves that prompt out of
// (or back into) the section.
func (r *resolver) heldPromptRow(s *wsState, id string) bool {
	h, ok := r.held[s.id]
	if !ok {
		return false
	}
	for _, held := range h.prompts {
		if held.Tombstone != nil {
			continue
		}
		if _, rowID := r.promptRowFor(s, held.Turn); rowID.GetValue() == id {
			return true
		}
	}
	// A CLAIMED INGRESS ENTRY'S ROW moves it out of the section too: an
	// accepted entry delivered at once is drawn as its row, never as a hold.
	for key, turn := range h.claims {
		if h.settled[key] {
			continue
		}
		if _, rowID := r.promptRowFor(s, turn); rowID.GetValue() == id {
			return true
		}
	}
	return false
}

// emitRow is the one door a row publication leaves by (upsert and retire).
//
// A ROW THAT MOVES A HELD PROMPT is published WITH the section it changes: on
// the root feed, as one held-section publication carrying the row; on another
// feed (a turn drawing at a merge tab's address), as the row's own publication
// followed by the root's section without the prompt. Every other row is
// published on its own.
func (r *resolver) emitRow(s *wsState, f *feedState, row *frontendv1.FeedRow) {
	root := f.key == r.rootFeed(s).key
	if !r.heldPromptRow(s, row.GetId().GetValue()) {
		r.emit(s, f, Publication{Row: row})
		return
	}
	if root {
		r.publishHeld(s, []*frontendv1.FeedRow{row}, nil)
		return
	}
	r.emit(s, f, Publication{Row: row})
	r.publishHeld(s, nil, nil)
}
