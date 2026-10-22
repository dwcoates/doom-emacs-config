// Package holds renders what the daemon is holding for the user: each standing
// held prompt as the held section's card, in delivery order. It holds no state
// and publishes nothing.
//
// THE HELD SECTION LIVES IN THE ROOT FEED (frontend.v1 FeedHeld). The feed
// resolver owns it — its state, its publication, and the moment an item
// settles into a row — because a settle must reach a reader as ONE change of
// the feed. This package is the half that is about held PROMPTS alone: their
// projection onto frontend.v1 HeldPrompt, and the order they draw in. The
// section is fed by the prompt queue and the merge orchestrator, never by the
// session watcher, through Sink.
package holds

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// Sink is what the prompt queue tells the held section. The feed resolver
// implements it.
type Sink interface {
	// SetHeldPrompts installs a workspace's standing holds, whole. The prompt
	// queue is the only caller.
	SetHeldPrompts(ws ids.WorkspaceID, held []wsm.HeldPrompt)
	// FoldHeldPrompts installs a workspace's standing holds, whole, as
	// SetHeldPrompts does, in the change that FOLD made: the section is
	// published naming the fold (WatchFeedHeldChange.folds), so a client
	// flashes the entry folded into and shows the folded card leaving. The
	// prompt queue calls it after every fold, the user's and the
	// classifier's alike, and nothing else does.
	FoldHeldPrompts(ws ids.WorkspaceID, held []wsm.HeldPrompt, fold Fold)
	// SetEditing names the held prompt being edited (EditHeldPrompt), or
	// clears it with the empty turn. Its card carries HeldPrompt.editing. The
	// prompt queue is the only caller.
	SetEditing(ws ids.WorkspaceID, turn ids.TurnID)
}

// DrawBody resolves what a person said into the body a card draws: the very
// body the prompt's feed row will carry (feed.DrawUserBlocks). The feed
// resolver supplies it, because it owns the sentinel strip and the image
// resolution the drawing needs.
type DrawBody func(content *conversationv1.UserContent) *frontendv1.FeedUserPromptBody

// Prompts renders the standing holds as the held section's prompt cards, in
// delivery order: oldest first, a retired hold skipped, each card's "fold
// above" button naming the standing card drawn just before it. EDITING names
// the hold an EditHeldPrompt claim stands on, empty when none does. DRAW
// resolves each card's body.
//
// The answer pairs each card with the hold it was drawn from, because the
// section's caller keys each item by the hold's turn.
func Prompts(held []wsm.HeldPrompt, editing ids.TurnID, draw DrawBody, log dlog.Logger) []Card {
	cards := make([]Card, 0, len(held))
	// ahead is the standing entry drawn just before this one: what its "fold
	// above" button would fold it into.
	var ahead *wsm.HeldPrompt
	for _, h := range orderedHolds(held) {
		prompt := heldPrompt(h, editing != "" && h.Turn == editing, draw, log)
		if prompt == nil {
			continue
		}
		prompt.FoldAbove = foldAbove(h, ahead, editing, log)
		ahead = &h
		cards = append(cards, Card{Turn: h.Turn, Prompt: prompt})
	}
	return cards
}

// Card is one held prompt's card and the turn it is held under.
type Card struct {
	// Turn is the turn the prompt will run as.
	Turn ids.TurnID
	// Prompt is the card.
	Prompt *frontendv1.HeldPrompt
}

// Fold is one held prompt folded into the held entry directly ahead of it.
type Fold struct {
	// From is the folded prompt's turn: it left the queue.
	From ids.TurnID
	// Into is the turn of the entry it was folded into, which now carries
	// both prompts' content.
	Into ids.TurnID
}
