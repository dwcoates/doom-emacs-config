package sessioncontroller

import (
	"context"
	"fmt"

	"claude-repld/internal/errclass"
)

// This file is the session controller's half of the merge exclusivity lease.
//
// While merge.Coordinator holds the lease on a workspace, that workspace's shim
// belongs to the merge: the user's in-flight turn was stopped to make room for
// it, further user prompts are refused, and ONLY the holder may submit. The SSM
// owns the durable claim (ssm/mergelease.go); everything here is the two edges
// that claim has on the prompt path.

// submitter names who is asking a prompt to be submitted. It exists so the
// merge lease's exclusivity is decided from an explicit fact rather than from
// sniffing the vendor-visible origin string, which is free-form and would make
// the gate a naming convention instead of a check.
type submitter int

const (
	// submitterUser is every ordinary prompt: a frontend command, a queued
	// prompt being drained, a workspace-create job's opening prompt. All of
	// them are refused while the lease is held.
	submitterUser submitter = iota
	// submitterMergeLeaseHolder is merge.Coordinator driving the session it
	// took the lease over. It is refused when the lease is NOT held: a merge
	// prompt without a lease behind it means the exclusivity claim this path
	// relies on was never taken, and the user could be prompting the same shim.
	submitterMergeLeaseHolder
)

func (s submitter) String() string {
	switch s {
	case submitterMergeLeaseHolder:
		return "merge-lease-holder"
	default:
		return "user"
	}
}

// guardMergeLease decides whether who may submit to workspace right now.
//
// IT IS CHECKED TWICE ON PURPOSE, at the top of submitPrompt and again in
// forwardPrompt. The first refuses the user's command where it can still be
// reported to them as a failed command instead of a prompt quietly parked on
// the queue. The second is the funnel EVERY submit reaches — the immediate one,
// the queue's drain, an interject's head jump — so a path that forgets the
// first check cannot slip a user prompt into a leased session. The redundancy
// is what makes the bypass structurally impossible rather than merely covered.
//
// The error is deliberately explanatory: it is surfaced verbatim to whoever
// typed the prompt, and "refused" with no reason is indistinguishable from the
// silent drop this exists to prevent.
func (m *Manager) guardMergeLease(workspace string, who submitter, requestID, origin string) error {
	held := m.cfg.SSM.MergeLeaseHeld(workspace)
	switch {
	case held && who == submitterUser:
		err := fmt.Errorf("workspace %q is being merged: merge.Coordinator holds the exclusivity lease on its session, so prompts are refused until the merge reaches a terminal phase (merged, merge_failed, or an abandoned merge_conflict). Nothing was submitted and nothing was queued — resubmit once the merge finishes", workspace)
		m.logf("session-controller: prompt REFUSED ws=%q request_id=%s origin=%q submitter=%s — the merge lease is held; the prompt was neither submitted nor queued",
			workspace, requestID, origin, who)
		return err
	case !held && who == submitterMergeLeaseHolder:
		err := fmt.Errorf("session-controller: a merge prompt for workspace %q arrived with NO merge lease held; the exclusivity claim it relies on was never taken, so the user may be driving the same shim", workspace)
		m.logf("session-controller: merge prompt REFUSED ws=%q request_id=%s origin=%q — no merge lease is held for this workspace",
			workspace, requestID, origin)
		return err
	}
	return nil
}

// SubmitMergePrompt submits a prompt on behalf of merge.Coordinator, the holder
// of the workspace's exclusivity lease. It is the ONLY entry point that is not
// refused while the lease stands, and it refuses itself when the lease does not.
//
// It carries a merge-shaped origin so the vendor-visible provenance of a
// conflict-resolution turn is distinguishable from a person's prompt, and the
// conversation items it produces are stamped CONVERSATION_SOURCE_MERGE off the
// lease ledger by the ordinary provenance path (provenance.go).
func (m *Manager) SubmitMergePrompt(ctx context.Context, workspace, requestID, text, permissionMode string) error {
	if requestID == "" {
		return fmt.Errorf("session-controller: a merge prompt for workspace %q needs a request id; it is what the prompt receipt and the durable transcript line reconcile on", workspace)
	}
	return m.submitPromptAs(ctx, workspace, requestID, text, permissionMode, "merge:"+requestID, submitterMergeLeaseHolder)
}

// InterruptForMerge stops the workspace's in-flight turn so merge.Coordinator
// can take its session over. It is half of ssm.SessionAuthority; TeardownMerged
// (mergedteardown.go) is the other.
//
// IT IS MACHINERY, NOT A USER'S STOP, and that is the whole reason it is not
// Manager.Interrupt. The user did not ask for their turn to end — a merge
// needed the shim — so it must not open the interrupt window, must not paint
// the turn's outcome `interrupted`, and must not pause the queue. Exactly the
// same distinction the queue's own interject draws (queue.go), and for the same
// reason.
//
// ALREADY_COMPLETE IS A SUCCESS. The lease wants no turn running; a turn that
// had already ended is that condition, reached without this call's help.
func (m *Manager) InterruptForMerge(ctx context.Context, workspace string) error {
	d, err := m.existing(workspace)
	if err != nil {
		// NO SESSION IS NO TURN. The lease's requirement is that nothing of the
		// user's is running behind the shim it is taking, and a workspace with
		// no live session controller satisfies it outright. Reported, never
		// swallowed: a merge proceeding over a workspace nobody has opened is
		// worth seeing in the log.
		m.logf("session-controller: merge interrupt ws=%q found no live session controller (%v) — nothing of the user's is in flight, so the lease's precondition already holds",
			workspace, err)
		return nil
	}
	outcome, err := d.client.Interrupt(ctx)
	if err != nil {
		m.logf("session-controller: merge interrupt FAILED ws=%q session=%s: %v — the user's turn is still running and the merge lease must not stand over it",
			workspace, d.sessionID, err)
		return err
	}
	if failed := errclass.InterruptError(outcome); failed != nil {
		m.logf("session-controller: merge interrupt UNDELIVERABLE ws=%q session=%s outcome=%s", workspace, d.sessionID, outcome)
		return failed
	}
	m.logf("session-controller: merge interrupt ws=%q session=%s outcome=%s — the shim is handed to merge.Coordinator",
		workspace, d.sessionID, outcome)
	return nil
}
