package merge

import (
	"errors"
	"fmt"

	"claude-repld/internal/ids"
)

// This file holds the merge orchestrator's PRE-STATE refusals.
//
// Enqueue refuses before it records anything: an unmergeable workspace leaves
// no enqueuing→failed trail, because there is no state to stamp yet. Each
// refusal is a typed value carrying the ARM NAME the contract owes it, and the
// server maps that name onto the arm of whichever rpc it is answering — the
// same refusal is `MergeWorkspaceError.already_queued` under MergeWorkspace and
// `UpdateMergeQueueError.no_such_queued_merge` under UpdateMergeQueue, so the
// per-rpc choice belongs to the handler and the CAUSE belongs here. Every arm
// below landed in landing 4; none of them owes an ERROR-ARMS.md row.

// The arm names, spelled exactly as the landed `<Rpc>Error` oneof arms spell
// them.
const (
	// ArmNoLayoutFacts refuses a workspace whose creation job recorded no merge
	// geometry. Geometry is recorded at creation and NEVER inferred later, so
	// its absence means this workspace can never be merged.
	ArmNoLayoutFacts = "no_layout_facts"
	// ArmSessionDeleted refuses a workspace whose session is deleted. A deleted
	// session refuses resurrection, so the configured prompts could never run.
	ArmSessionDeleted = "session_deleted"
	// ArmAlreadyQueued refuses a second enqueue of a merge that is already
	// waiting: a merge is queued once.
	ArmAlreadyQueued = "already_queued"
	// ArmAlreadyMerging refuses an enqueue for the workspace whose merge is
	// running right now.
	ArmAlreadyMerging = "already_merging"
	// ArmNoSuchQueuedMerge refuses an evict or a dequeue answer for a workspace
	// with nothing on the queue (UpdateMergeQueueError.no_such_queued_merge).
	ArmNoSuchQueuedMerge = "no_such_queued_merge"
	// ArmNoOfferStanding refuses a dequeue answer when no offer stands
	// (AnswerHeldOfferError.no_offer_standing).
	ArmNoOfferStanding = "no_offer_standing"
	// ArmOfferSuperseded refuses a dequeue answer for an offer a newer one
	// replaced (AnswerHeldOfferError.offer_superseded).
	ArmOfferSuperseded = "offer_superseded"
	// ArmAlreadyPaused refuses a pause of a queue that is already paused
	// (UpdateMergeQueueError.already_paused).
	ArmAlreadyPaused = "already_paused"
	// ArmUnknownRepository refuses a pause or a resume whose repository ref
	// matches no registered repository
	// (UpdateMergeQueueError.unknown_repository, landed at landing 6).
	ArmUnknownRepository = "unknown_repository"
	// ArmNotPaused refuses an unpause of a queue that is not paused
	// (UpdateMergeQueueError.not_paused).
	ArmNotPaused = "not_paused"
)

// RefusalError is one pre-state refusal: which arm the contract owes it, and
// the reason the server puts after the arm.
type RefusalError struct {
	// Arm is the intended <Rpc>Error arm name.
	Arm string
	// Workspace is the refused workspace.
	Workspace ids.WorkspaceID
	// Reason is the sentence explaining the refusal, kept as evidence.
	Reason string
}

// Error spells the refusal the way the transport answers it, so the arm travels
// with the message rather than being reattached by the handler.
func (e *RefusalError) Error() string {
	return fmt.Sprintf("merge: refused %s: %s (workspace %s)", e.Arm, e.Reason, e.Workspace)
}

// refuse builds a refusal for one workspace.
func refuse(arm string, ws ids.WorkspaceID, format string, args ...any) *RefusalError {
	return &RefusalError{Arm: arm, Workspace: ws, Reason: fmt.Sprintf(format, args...)}
}

// Refused reports the intended arm of a refusal, and false for any other error.
// The server uses it to decide between an unlanded-arm answer and an ordinary
// internal failure.
func Refused(err error) (string, bool) {
	var r *RefusalError
	if errors.As(err, &r) {
		return r.Arm, true
	}
	return "", false
}
