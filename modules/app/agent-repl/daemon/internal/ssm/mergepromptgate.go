package ssm

import (
	"errors"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// mergepromptgate.go — WHO MAY TAKE THE ACCEPTED-PROMPT EDGE OVER A MERGE STATE.
//
// The accepted edge's publication invariant (publishPromptAcceptedLocked)
// requires the resolved render state to be `submitting` or `thinking`. The merge
// axis outranks the session-status axis for every phase that OWNS the shim
// (connectivity.go), so a prompt submitted while the workspace rests on one of
// those resolves to the merge state and the invariant refuses it. For a user's
// own prompt that refusal is exactly right: a merge run is driving the session
// and nothing else may.
//
// IT WAS WRONG FOR THE DAEMON'S OWN MACHINERY, and the cost was measured. A
// workspace sitting in `merge_queued` is WAITING for a merge lock it does not
// hold: no coordinator owns its shim, its session is idle, and the merge that
// will eventually drive it may be an hour away. The cache keep-alive ping and
// the pre-expiry warm compaction are precisely the two turns that keep such a
// session cheap, and both were refused here — so the prompt cache expired while
// the workspace queued, the cache-cold policy then hibernated it, and the
// conflict resolution that finally ran paid a full uncached re-ingest (215k
// uncached input tokens, observed 2026-08-06).
//
// So the gate is by SUBMITTER, not by phase alone. Machinery is admitted over
// the merge phases that leave the session IDLE AND UNOWNED; every phase where a
// coordinator holds the shim still refuses everyone, because the merge's own
// turns keep that cache warm organically and a second turn racing them is the
// thing the lease exists to prevent.

// PromptAdmission says WHOSE prompt is taking the accepted edge.
//
// It is a parameter of the accepted edge rather than a lookup because the SSM
// cannot answer it: "the daemon submitted this on its own behalf" is a fact
// about the submitting call site and nothing durable records it. The two values
// are deliberately not a boolean — a reader of a call site sees which kind of
// prompt it is submitting without having to know what `true` meant.
type PromptAdmission int

const (
	// PromptAdmissionUser is every prompt with a person behind it: a frontend
	// submit, a queued prompt's delivery, an interject, a merge run's own
	// resolution prompt. It gets the pre-existing behavior in every respect.
	PromptAdmissionUser PromptAdmission = iota
	// PromptAdmissionIdleMachinery is a turn the DAEMON generates against a
	// session it believes is idle: the cache keep-alive ping and the pre-expiry
	// warm compaction. Neither is anybody's conversation, neither can be
	// retried later without cost (the cache it protects is dying on a clock),
	// and both are safe precisely because the session is idle.
	PromptAdmissionIdleMachinery
)

func (a PromptAdmission) String() string {
	if a == PromptAdmissionIdleMachinery {
		return "idle_machinery"
	}
	return "user"
}

// ErrPromptRefusedByMergeState anchors the accepted edge's MERGE-AXIS refusal.
//
// It is its own sentinel because it is a DIFFERENT fact from the generic
// "the state premise did not hold" failure the invariant otherwise reports: the
// workspace is fine, the daemon is fine, and the prompt was refused because a
// merge owns this workspace. A caller — and a frontend — can say that, where the
// raw invariant sentence could only be rendered as an internal fault.
var ErrPromptRefusedByMergeState = errors.New("ssm: the workspace rests on a merge state that refuses prompts")

// mergeAxisRenderStates are the render states the merge axis produces. A
// refusal on one of them is a merge refusal; a refusal on anything else is the
// invariant's generic failure and must not borrow the merge sentinel.
func mergeAxisRenderState(s frontendv1.RenderState) bool {
	switch s {
	case frontendv1.RenderState_RENDER_STATE_MERGE_ENQUEUING,
		frontendv1.RenderState_RENDER_STATE_MERGING,
		frontendv1.RenderState_RENDER_STATE_MERGE_QUEUED,
		frontendv1.RenderState_RENDER_STATE_MERGE_CONFLICT,
		frontendv1.RenderState_RENDER_STATE_MERGE_FAILED,
		frontendv1.RenderState_RENDER_STATE_MERGED:
		return true
	}
	return false
}

// mergeStateLeavesSessionIdle reports whether a merge render state leaves the
// workspace's shim UNOWNED and its session idle.
//
// THE TWO MEMBERS ARE THE TWO WAITING STATES, and neither has a coordinator
// behind it:
//
//   - MERGE_QUEUED is a workspace waiting its turn for the merge lock. The
//     coordinator has not taken the lease, has not touched the shim, and will
//     not until the queue reaches it.
//   - MERGE_FAILED is TERMINAL: the run is over, nothing is coming to clear it,
//     and the workspace is simply a live session with a red badge. The
//     resolver already says as much — merge_failed does not mask a turn claim
//     (connectivity.go) — so machinery over it is admitted by the resolver
//     alone and this membership is what states that rather than leaving it to
//     be rediscovered.
//
// Every OTHER merge phase is a coordinator holding the shim (enqueuing,
// merging, before/after action), a workspace parked awaiting a human
// (merge_conflict), or a finished merge whose workspace is being torn down
// (merged). None of those is idle and none admits a machinery turn.
func mergeStateLeavesSessionIdle(s frontendv1.RenderState) bool {
	switch s {
	case frontendv1.RenderState_RENDER_STATE_MERGE_QUEUED,
		frontendv1.RenderState_RENDER_STATE_MERGE_FAILED:
		return true
	}
	return false
}

// admitsIdleMachineryTurn reports whether a published state that FAILED the
// accepted edge's ordinary `submitting`/`thinking` premise may nonetheless be
// published for an idle-machinery submitter.
//
// THE TURN CLAIM IS STILL REQUIRED. What the invariant guards is that a turn is
// claimed at all before a prompt is exposed, and that guarantee is not the merge
// axis's to relax: the machinery exemption only says the merge PHASE may hide
// the claim from the render state, never that the claim may be absent.
func admitsIdleMachineryTurn(admission PromptAdmission, state *frontendv1.WorkspaceState) bool {
	return admission == PromptAdmissionIdleMachinery &&
		state.GetTurnActive() &&
		mergeStateLeavesSessionIdle(state.GetState())
}
