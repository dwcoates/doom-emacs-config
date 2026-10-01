package promptqueue

import (
	"context"
	"errors"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/holdfold"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// FOLDING A HELD PROMPT INTO THE ONE AHEAD (owner request, 2026-09-30;
// FoldHeldPrompt). The tray's "fold above" button appends a held prompt's
// content to the held prompt directly ahead of it and takes it out of the
// queue, so the two are delivered together as that one entry.
//
// IT IS THE CLASSIFIER'S COALESCE, ASKED FOR BY THE USER. Both write through
// wsm.CoalesceHeldPrompts, the one transaction that merges the entry ahead and
// retires the folded one together, and both retire it as `coalesced`. What a
// fold adds is what an edit's commit does to the entry whose words changed:
// its verdict and acceptance are discarded in that same transaction, the
// content epoch of both turns is bumped under the verdict lock so a verdict
// still being reached about either one's old words is discarded when it lands,
// a queue jump either had earned is dropped, and the merged entry is
// reclassified through the ordinary path.
//
// IT RUNS UNDER THE DELIVERY LOCK, THEN THE VERDICT LOCK, the order the queue
// always takes them in. A turn end pops under the delivery lock, so the entry
// ahead cannot be delivered between this reading it as standing and this
// folding into it.

// opFold is the fold's operation name.
const opFold = "daemon.promptqueue.fold"

// The fold's own refusals. The server maps each onto FoldHeldPromptError's arm
// of the same name (see the table in api.go).
var (
	// ErrNotAPrompt is a fold of a session act: an act is never folded.
	ErrNotAPrompt = errors.New("promptqueue: that entry is a session act, not a prompt")
	// ErrAboveNotAPrompt is a fold into a session act.
	ErrAboveNotAPrompt = errors.New("promptqueue: the entry ahead is a session act, not a prompt")
	// ErrAboveMoved is a fold naming an entry that is no longer the one
	// directly ahead. AboveMovedError carries it with the entry that is.
	ErrAboveMoved = errors.New("promptqueue: the entry named is no longer directly ahead")
)

// AboveMovedError is a fold refused because the entry the client named is no
// longer directly ahead of the folded prompt. It names the entry that is.
type AboveMovedError struct {
	// Current is the entry directly ahead now, empty when nothing is (the
	// folded prompt is first in the queue).
	Current ids.TurnID
}

// Error names the refusal and the entry that is ahead now.
func (e *AboveMovedError) Error() string {
	if e.Current == "" {
		return ErrAboveMoved.Error() + " (nothing stands ahead of it)"
	}
	return fmt.Sprintf("%s (turn %s is)", ErrAboveMoved.Error(), e.Current)
}

// Unwrap answers the sentinel, so `errors.Is(err, ErrAboveMoved)` holds.
func (e *AboveMovedError) Unwrap() error { return ErrAboveMoved }

// Fold folds the held prompt TURN into ABOVE, the entry the client saw
// directly ahead of it. See the Queue interface.
func (q *queue) Fold(ctx context.Context, ws ids.WorkspaceID, turn, above ids.TurnID) error {
	log, err := q.logger(ctx, ws)
	if err != nil {
		return err
	}
	log = log.With(dlog.Context{"turn": string(turn), "above_turn": string(above)})

	drain := &q.state(ws).drain
	drain.Lock()
	defer drain.Unlock()

	folded, err := q.heldForFold(ctx, ws, turn, log)
	if err != nil {
		return err
	}
	ahead, err := q.aheadForFold(ctx, ws, folded, above, log)
	if err != nil {
		return err
	}
	merged := foldedSaid(ahead.Said, folded.Said)
	if err := q.foldLocked(ctx, ws, folded.Turn, ahead.Turn, merged, log); err != nil {
		return err
	}
	q.clearHeadIf(ws, ahead.Turn)
	q.clearHeadIf(ws, folded.Turn)
	log.Info(opFold, "the held prompt was folded into the one ahead; the merged prompt's verdict is discarded and it is reclassified", nil)
	if err := q.pushTray(ctx, ws, log); err != nil {
		return err
	}

	ahead.Said = merged
	ahead.Coalesced = true
	ahead.Classification = nil
	ahead.Accepted = false
	q.reclassify(ctx, ws, ahead, log, opFold)
	return nil
}

// heldForFold resolves the prompt being folded, telling apart a turn nothing
// was ever held under from a hold that has left the queue. Each refusal is
// logged at INFO: it is the user's click on a card the tray has not yet taken
// down, never a fault.
func (q *queue) heldForFold(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID, log dlog.Logger) (wsm.HeldPrompt, error) {
	held, found, err := q.deps.DB.HeldPromptByTurn(ctx, turn)
	if err != nil {
		log.Error(opFold, "could not read the hold the fold names", dlog.Context{"cause": err.Error()})
		return wsm.HeldPrompt{}, fmt.Errorf("read hold %q on %q: %w", turn, ws, err)
	}
	switch {
	case !found || held.Workspace != ws:
		log.Info(opFold, "the fold is refused: no hold was ever recorded under the turn", nil)
		return wsm.HeldPrompt{}, ErrNoSuchHold
	case held.Tombstone != nil:
		log.Info(opFold, "the fold is refused: the prompt is no longer held",
			dlog.Context{"tombstone": held.Tombstone.Kind})
		return wsm.HeldPrompt{}, ErrNotHeld
	}
	return held, nil
}

// aheadForFold resolves the entry FOLDED folds into: the standing entry
// directly ahead of it, which must be ABOVE, and which holdfold.Foldable must
// accept. Every refusal is logged at INFO and folds nothing.
func (q *queue) aheadForFold(ctx context.Context, ws ids.WorkspaceID, folded wsm.HeldPrompt, above ids.TurnID, log dlog.Logger) (wsm.HeldPrompt, error) {
	standing, err := q.deps.DB.HeldPrompts(ctx, ws)
	if err != nil {
		log.Error(opFold, "could not read the queue to find the entry ahead", dlog.Context{"cause": err.Error()})
		return wsm.HeldPrompt{}, fmt.Errorf("read the holds for %q: %w", ws, err)
	}
	ahead, ok := holdfold.Ahead(standing, folded.Turn)
	if !ok || ahead.Turn != above {
		moved := &AboveMovedError{}
		if ok {
			moved.Current = ahead.Turn
		}
		log.Info(opFold, "the fold is refused: the entry named is no longer directly ahead",
			dlog.Context{"current_above": string(moved.Current)})
		return wsm.HeldPrompt{}, moved
	}
	editing, _ := q.Editing(ws)
	switch err := holdfold.Foldable(folded, ahead, editing.Turn); {
	case err == nil:
		return ahead, nil
	case errors.Is(err, holdfold.ErrNotAPrompt):
		log.Info(opFold, "the fold is refused: the entry is a session act, not a prompt", nil)
		return wsm.HeldPrompt{}, ErrNotAPrompt
	case errors.Is(err, holdfold.ErrAboveNotAPrompt):
		log.Info(opFold, "the fold is refused: the entry ahead is a session act, not a prompt", nil)
		return wsm.HeldPrompt{}, ErrAboveNotAPrompt
	case errors.Is(err, holdfold.ErrEdited):
		log.Info(opFold, "the fold is refused: one of the two entries is being edited",
			dlog.Context{"editing_turn": string(editing.Turn), "edit": editing.ID})
		return wsm.HeldPrompt{}, &BeingEditedError{Turn: editing.Turn}
	default:
		log.Error(opFold, "holdfold answered a reason the fold does not know; nothing was folded", dlog.Context{
			"cause":               err.Error(),
			"invariant_violation": "holdfold.Foldable answered an undeclared reason",
			"remediation":         "map the reason onto a FoldHeldPromptError arm",
		})
		return wsm.HeldPrompt{}, fmt.Errorf("fold %q into %q on %q: %w", folded.Turn, ahead.Turn, ws, err)
	}
}

// foldLocked writes the fold — the merged content into INTO with its verdict
// discarded, and FROM retired — in one store transaction, and bumps both
// turns' content epochs in the same verdict-lock hold. A store failure leaves
// both entries as they were and bumps nothing. The caller holds the delivery
// lock.
func (q *queue) foldLocked(ctx context.Context, ws ids.WorkspaceID, from, into ids.TurnID, merged *conversationv1.UserSaid, log dlog.Logger) error {
	state := q.state(ws)
	state.verdicts.Lock()
	defer state.verdicts.Unlock()
	if err := q.deps.DB.CoalesceHeldPrompts(ctx, wsm.Coalescence{
		Into:           into,
		From:           from,
		Said:           merged,
		Retired:        wsm.Tombstone{Kind: tombstoneCoalesced, At: q.deps.Now()},
		DiscardVerdict: true,
	}); err != nil {
		log.Error(opFold, "the fold could not be recorded; both entries stand as they were", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("fold %q into %q on %q: %w", from, into, ws, err)
	}
	state.bumpEpochLocked(into)
	state.bumpEpochLocked(from)
	return nil
}

// foldedSaid is INTO's content followed by FROM's, every block of both in
// order, with the two prompts' words separated by a blank line: where INTO
// ends in text and FROM begins with text, the two blocks become one text block
// joined by "\n\n". Where either side of the seam is not text (an image), the
// blocks are appended as they are.
func foldedSaid(into, from *conversationv1.UserSaid) *conversationv1.UserSaid {
	head := into.GetContent().GetBlocks()
	tail := from.GetContent().GetBlocks()
	blocks := make([]*conversationv1.UserContentBlock, 0, len(head)+len(tail))
	blocks = append(blocks, head...)
	if len(head) > 0 && len(tail) > 0 {
		last, lastIsText := head[len(head)-1].GetBlock().(*conversationv1.UserContentBlock_Text)
		first, firstIsText := tail[0].GetBlock().(*conversationv1.UserContentBlock_Text)
		if lastIsText && firstIsText {
			blocks[len(blocks)-1] = &conversationv1.UserContentBlock{Block: &conversationv1.UserContentBlock_Text{
				Text: &conversationv1.TextBlock{Text: last.Text.GetText() + "\n\n" + first.Text.GetText()},
			}}
			tail = tail[1:]
		}
	}
	blocks = append(blocks, tail...)
	return &conversationv1.UserSaid{Content: &conversationv1.UserContent{Blocks: blocks}}
}
