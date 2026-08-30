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
// refusal is a typed error carrying the intended contract arm, which the
// server answers through server.UnlandedArm until the arm lands. The arms are
// recorded in daemon/ERROR-ARMS.md.

// The intended MergeWorkspaceError arm names. They are the vocabulary the
// server spells into `intended arm: MergeWorkspaceError.<arm>: …`.
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
	// ArmNoQueuedMerge refuses an evict or a dequeue answer for a workspace with
	// nothing on the queue.
	ArmNoQueuedMerge = "no_queued_merge"
	// ArmNoOfferStanding refuses a dequeue answer when no offer stands.
	ArmNoOfferStanding = "no_offer_standing"
	// ArmAlreadyPaused refuses a pause of a queue that is already paused.
	ArmAlreadyPaused = "already_paused"
	// ArmNotPaused refuses an unpause of a queue that is not paused.
	ArmNotPaused = "not_paused"
	// ArmNotParked refuses a parked route for a workspace whose lease is not
	// parked. The lease state is the recognition, so a route without one is the
	// caller's bug, never guidance to deliver anyway.
	ArmNotParked = "not_parked"
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
