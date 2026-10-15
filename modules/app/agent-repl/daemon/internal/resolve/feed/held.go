package feed

import (
	agentreplv1 "agentrepl/proto/agentrepl/v1"
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
	// published is the section as last published; nil until the first.
	published *frontendv1.FeedHeld
}

// heldOf resolves a workspace's held section state, creating it on first sight.
// The caller holds the lock.
func (r *resolver) heldOf(ws ids.WorkspaceID) *heldState {
	h, ok := r.held[ws]
	if !ok {
		h = &heldState{}
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
	for _, card := range holds.Prompts(h.prompts, h.editing, log) {
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
	return &frontendv1.FeedHeld{Items: items}
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
