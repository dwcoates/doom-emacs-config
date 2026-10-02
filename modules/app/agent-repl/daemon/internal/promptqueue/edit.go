package promptqueue

import (
	"context"
	"errors"
	"fmt"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// EDITING A HELD PROMPT (owner spec, 2026-09-23; EditHeldPrompt).
//
// THE CLAIM LIVES UNDER THE DELIVERY LOCK. A workspace's edit claim is written
// only while its `drain` mutex is held — the mutex every delivery of a
// standing hold is decided under (a turn's end, a lease change, a revival's
// release, a release, and a submission that would go straight to the shim) —
// and every one of those decisions reads the claim under that same mutex. So
// "delivered mid-edit" is not merely unlikely, it cannot be scheduled: a
// begin that lands while a delivery is in flight waits for it and then finds
// the prompt delivered, and a delivery that starts after a begin sees the
// claim. The claim is ALSO guarded by q.mu for its readers that decide
// nothing (the host view and the tray), so they never wait on a delivery rpc.
//
// ONE CLAIM PER WORKSPACE. While it stands, the claimed prompt AND EVERY
// PROMPT QUEUED AFTER IT are withheld from delivery; the prompts queued before
// it are not, because nothing about the edit is ahead of them.
//
// THE CLAIM IS SCOPED TO THE EDITOR'S HOST STREAM, never to a timer. The
// server calls EditorGone the moment no WatchHostWorkspace stream stands for
// the workspace, and a begin is refused when none does. A daemon restart ends
// the claim with the process that held it, because the claim is in-memory
// and so was the stream.

// The edit's operation names.
const (
	opEditBegin   = "daemon.promptqueue.edit_begin"
	opEditCommit  = "daemon.promptqueue.edit_commit"
	opEditCancel  = "daemon.promptqueue.edit_cancel"
	opEditRelease = "daemon.promptqueue.edit_release"
)

// Edit is one standing held-prompt edit, as the host view and the tray state
// it.
type Edit struct {
	// Turn is the held prompt being edited.
	Turn ids.TurnID
	// Said is the prompt's content as the edit began.
	Said *conversationv1.UserSaid
	// ID is this edit's own identity, distinct for every begin.
	ID uint64
}

// editClaim is the claim a workspace's state carries: the edit, plus the
// queue position everything after it is withheld from.
type editClaim struct {
	Edit
	queuedAt time.Time
}

// EditorProbe reports whether an editor — a host stream — stands for the
// workspace right now. It is evaluated UNDER the delivery lock, which is what
// makes a begin and the editor's departure commit against each other.
type EditorProbe func() bool

// BeingEditedError is a begin refused because an edit already stands on the
// workspace. It names the prompt that edit is on.
type BeingEditedError struct {
	// Turn is the prompt the standing edit is on.
	Turn ids.TurnID
}

// Error names the refusal and the prompt being edited.
func (e *BeingEditedError) Error() string {
	return fmt.Sprintf("%s (turn %s)", ErrBeingEdited.Error(), e.Turn)
}

// Unwrap answers the sentinel, so `errors.Is(err, ErrBeingEdited)` holds.
func (e *BeingEditedError) Unwrap() error { return ErrBeingEdited }

// errDeliveryBehindEdit is the delivery path's own backstop: a hold that an
// edit withholds reached deliverHeld anyway. Every caller filters first, so
// reaching it is a defect in a caller, and it is refused rather than sent.
var errDeliveryBehindEdit = errors.New("promptqueue: a hold withheld by an edit reached delivery")

// BeginEdit claims a held prompt as being edited. See the Queue interface.
func (q *queue) BeginEdit(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID, editor EditorProbe) error {
	log, err := q.logger(ctx, ws)
	if err != nil {
		return err
	}
	log = log.With(dlog.Context{"turn": string(turn)})

	d := q.lockDelivery(ws)
	defer d.unlock()

	held, err := q.heldForEdit(ctx, ws, turn, log, opEditBegin)
	if err != nil {
		return err
	}
	if standing, ok := q.Editing(ws); ok {
		log.Info(opEditBegin, "the edit is refused: an edit already stands on this workspace",
			dlog.Context{"editing_turn": string(standing.Turn), "edit": standing.ID})
		return &BeingEditedError{Turn: standing.Turn}
	}
	if editor == nil || !editor() {
		log.Info(opEditBegin, "the edit is refused: no editor's host stream stands for the workspace", nil)
		return ErrNoEditor
	}

	q.mu.Lock()
	q.editSeq++
	claim := &editClaim{
		Edit:     Edit{Turn: held.Turn, Said: held.Said, ID: q.editSeq},
		queuedAt: held.QueuedAt,
	}
	q.states[ws].edit = claim
	q.mu.Unlock()

	log.Info(opEditBegin, "the held prompt is being edited; it and every prompt queued after it stay held",
		dlog.Context{"edit": claim.ID})
	q.publishEdit(ws, turn)
	return nil
}

// CommitEdit replaces the edited prompt's content and reclassifies it. See the
// Queue interface.
func (q *queue) CommitEdit(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID, said *conversationv1.UserSaid) error {
	log, err := q.logger(ctx, ws)
	if err != nil {
		return err
	}
	log = log.With(dlog.Context{"turn": string(turn)})

	d := q.lockDelivery(ws)
	defer d.unlock()

	held, err := q.heldForEdit(ctx, ws, turn, log, opEditCommit)
	if err != nil {
		return err
	}
	claim, err := q.claimOn(ws, turn, log, opEditCommit)
	if err != nil {
		return err
	}
	if said == nil {
		log.Error(opEditCommit, "the commit carried no content; the edit stands", dlog.Context{"edit": claim.ID})
		return fmt.Errorf("commit the edit of %q on %q: the new content is required", turn, ws)
	}
	// THE CONTENT IS REPLACED BEFORE THE CLAIM IS RETIRED. A replacement the
	// store refuses leaves the edit standing, so the user's words are never
	// released into the queue as the OLD content.
	if err := q.replaceContent(ctx, ws, turn, said); err != nil {
		log.Error(opEditCommit, "the edited content could not be recorded; the edit stands",
			dlog.Context{"edit": claim.ID, "cause": err.Error()})
		return fmt.Errorf("commit the edit of %q on %q: %w", turn, ws, err)
	}
	q.retireClaim(ws)
	// The old verdict is DISCARDED, and a queue jump it earned goes with it:
	// the new content earns its own.
	q.clearHeadIf(ws, turn)
	log.Info(opEditCommit, "the held prompt's content was replaced; its verdict is discarded and it is reclassified",
		dlog.Context{"edit": claim.ID})
	if err := q.pushTray(ctx, ws, log); err != nil {
		return err
	}
	q.publishEdit(ws, "")

	held.Said = said
	held.Classification = nil
	held.Accepted = false
	q.reclassify(ctx, d, held, log, opEditCommit)
	return nil
}

// CancelEdit retires an edit with the content unchanged. See the Queue
// interface.
func (q *queue) CancelEdit(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID) error {
	log, err := q.logger(ctx, ws)
	if err != nil {
		return err
	}
	log = log.With(dlog.Context{"turn": string(turn)})

	d := q.lockDelivery(ws)
	defer d.unlock()

	if _, err := q.heldForEdit(ctx, ws, turn, log, opEditCancel); err != nil {
		return err
	}
	claim, err := q.claimOn(ws, turn, log, opEditCancel)
	if err != nil {
		return err
	}
	q.retireClaim(ws)
	log.Info(opEditCancel, "the edit was cancelled; the content is unchanged and the queue resumes",
		dlog.Context{"edit": claim.ID})
	q.publishEdit(ws, "")
	q.resume(ctx, d, log, opEditCancel)
	return nil
}

// EditorGone retires the workspace's edit because no editor's host stream
// stands for it any more. See the Queue interface.
//
// EVERY HOST STREAM'S LAST CLOSE REACHES HERE, the daemon's own shutdown
// included, when the state store may already be closed. So a workspace with
// no claim is answered from memory alone and resolves nothing: no store read,
// no record, because nothing happened. (Regression watch: resolving the
// workspace's logger first logged "database is closed" at ERROR on every
// orderly exit.)
func (q *queue) EditorGone(ws ids.WorkspaceID) {
	if _, ok := q.Editing(ws); !ok {
		return
	}
	ctx := context.Background()
	log, err := q.logger(ctx, ws)
	if err != nil {
		return
	}

	d := q.lockDelivery(ws)
	defer d.unlock()

	claim, ok := q.Editing(ws)
	if !ok {
		log.Debug(opEditRelease, "the editor's host stream ended; the edit had already ended", nil)
		return
	}
	q.retireClaim(ws)
	log.Info(opEditRelease, "the editor's host stream ended; the edit is released with the content unchanged",
		dlog.Context{"turn": string(claim.Turn), "edit": claim.ID})
	q.publishEdit(ws, "")
	q.resume(ctx, d, log, opEditRelease)
}

// Editing answers the workspace's standing edit. It decides nothing, so it
// takes q.mu alone and never waits on a delivery.
func (q *queue) Editing(ws ids.WorkspaceID) (Edit, bool) {
	q.mu.Lock()
	defer q.mu.Unlock()
	state, ok := q.states[ws]
	if !ok || state.edit == nil {
		return Edit{}, false
	}
	return state.edit.Edit, true
}

// heldForEdit resolves the named hold for an edit step, telling apart a turn
// nothing was ever held under, a hold that was delivered, and one that was
// dropped. Each refusal is logged at INFO: it is the user's own click on a card
// the tray has not yet taken down, never a fault.
func (q *queue) heldForEdit(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID, log dlog.Logger, op string) (wsm.HeldPrompt, error) {
	held, found, err := q.deps.DB.HeldPromptByTurn(ctx, turn)
	if err != nil {
		log.Error(op, "could not read the hold the edit names", dlog.Context{"cause": err.Error()})
		return wsm.HeldPrompt{}, fmt.Errorf("read hold %q on %q: %w", turn, ws, err)
	}
	switch {
	case !found || held.Workspace != ws:
		log.Info(op, "the edit is refused: no hold was ever recorded under the turn", nil)
		return wsm.HeldPrompt{}, ErrNoSuchHold
	case held.Tombstone != nil && held.Tombstone.Kind == tombstoneDelivered:
		log.Info(op, "the edit is refused: the prompt was already delivered", nil)
		return wsm.HeldPrompt{}, ErrAlreadyDelivered
	case held.Tombstone != nil:
		log.Info(op, "the edit is refused: the prompt is no longer held",
			dlog.Context{"tombstone": held.Tombstone.Kind})
		return wsm.HeldPrompt{}, ErrNotHeld
	}
	if err := q.unclaimed(ws, held, log, op); err != nil {
		return wsm.HeldPrompt{}, err
	}
	return held, nil
}

// claimOn answers the standing edit when it is on TURN, and refuses with
// ErrNotEditing otherwise.
func (q *queue) claimOn(ws ids.WorkspaceID, turn ids.TurnID, log dlog.Logger, op string) (Edit, error) {
	claim, ok := q.Editing(ws)
	if !ok || claim.Turn != turn {
		fields := dlog.Context{"edit_standing": ok}
		if ok {
			fields["editing_turn"] = string(claim.Turn)
		}
		log.Info(op, "the step is refused: no edit stands on this prompt", fields)
		return Edit{}, ErrNotEditing
	}
	return claim, nil
}

// retireClaim drops the workspace's claim. The caller holds the delivery lock.
func (q *queue) retireClaim(ws ids.WorkspaceID) {
	q.mu.Lock()
	defer q.mu.Unlock()
	if state, ok := q.states[ws]; ok {
		state.edit = nil
	}
}

// retireEditIf retires the claim when it is on TURN, because that prompt left
// the queue (dropped) while it was being edited, and resumes the queue. It
// reports whether it did. The caller holds the delivery lock (d).
func (q *queue) retireEditIf(ctx context.Context, d *delivery, turn ids.TurnID, why string, log dlog.Logger) bool {
	ws := d.ws
	claim, ok := q.Editing(ws)
	if !ok || claim.Turn != turn {
		return false
	}
	q.retireClaim(ws)
	log.Info(opEditRelease, "the prompt being edited left the queue; the edit is released",
		dlog.Context{"turn": string(turn), "edit": claim.ID, "why": why})
	q.publishEdit(ws, "")
	q.resume(ctx, d, log, opEditRelease)
	return true
}

// publishEdit states the edit's change on both surfaces that carry it: the
// tray's editing marker and the host view's standing edit.
func (q *queue) publishEdit(ws ids.WorkspaceID, editing ids.TurnID) {
	q.deps.Holds.SetEditing(ws, editing)
	q.deps.PublishHost(ws)
}

// withheldByEdit reports whether the workspace's standing edit withholds HELD
// from delivery: it is the edited prompt, or it was queued after it. The
// order is the tray's own — queued_at, then the turn id — so what the reader
// sees after the edited card is exactly what is withheld.
func (q *queue) withheldByEdit(ws ids.WorkspaceID, held wsm.HeldPrompt) bool {
	q.mu.Lock()
	defer q.mu.Unlock()
	state, ok := q.states[ws]
	if !ok || state.edit == nil {
		return false
	}
	claim := state.edit
	if held.QueuedAt.Equal(claim.queuedAt) {
		return held.Turn >= claim.Turn
	}
	return held.QueuedAt.After(claim.queuedAt)
}

// reclassify re-enters a committed prompt into the ordinary classifier path:
// a running turn judges it exactly as a fresh hold is judged; with nothing
// running, the queue resumes and it is delivered in its place.
//
// A LEASE-HELD PROMPT IS NOT CLASSIFIED, exactly as a submission held by a
// lease is not: the lease owns it, and its release delivers it.
//
// OP names the verb whose new content is being judged: an edit's commit or a
// fold.
func (q *queue) reclassify(ctx context.Context, d *delivery, held wsm.HeldPrompt, log dlog.Logger, op string) {
	ws := d.ws
	if held.Hold != nil {
		log.Info(op, "the prompt's new content is held by a lease; no classifier runs until the lease releases it",
			dlog.Context{"hold": held.Hold.String()})
		return
	}
	if watcher, ok := q.deps.Watcher(ws); ok {
		if running := watcher.TurnInFlight(); running != nil {
			q.classifyHeld(ctx, submissionOf(held), *running, log)
			return
		}
	}
	q.resume(ctx, d, log, op)
}

// resume is what an edit ending owes the queue: when nothing is running, the
// next deliverable prompt is delivered now, because no turn's end is coming to
// do it. A running turn's own end delivers otherwise. The caller holds the
// delivery lock (d).
func (q *queue) resume(ctx context.Context, d *delivery, log dlog.Logger, op string) {
	if watcher, ok := q.deps.Watcher(d.ws); ok && watcher.TurnInFlight() != nil {
		log.Debug(op, "a turn is running; its end delivers the next held prompt", nil)
		return
	}
	if _, err := q.popAndDeliver(ctx, d, log); err != nil {
		log.Error(op, "the queue could not resume after the edit ended", dlog.Context{"cause": err.Error()})
	}
}

// holdBehindEdit parks a submission that would have gone straight to the shim
// but arrived while an edit stands: it is queued after the edited prompt, so
// it is withheld with it. Its verdict is stated rather than left unjudged, so
// the tray says why it waits.
func (q *queue) holdBehindEdit(ctx context.Context, sub Submission, claim Edit, log dlog.Logger) (Disposition, error) {
	log.Info(opHold, "an edit stands ahead of the submission; it is held behind the edited prompt",
		dlog.Context{"editing_turn": string(claim.Turn), "edit": claim.ID})
	disposition, err := q.hold(ctx, sub, "", nil, log)
	if err != nil {
		return Disposition{}, err
	}
	verdict := wsm.Classification{
		Arm:    wsm.ArmHoldForTurnEnd,
		Reason: "a held prompt ahead of it is being edited, so it waits for that edit to end",
		At:     q.deps.Now(),
	}
	q.record(ctx, sub, verdict, log)
	disposition.Classification = &verdict
	return disposition, nil
}
