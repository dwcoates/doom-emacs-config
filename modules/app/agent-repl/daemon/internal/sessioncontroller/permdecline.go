package sessioncontroller

import (
	"context"
	"errors"
	"fmt"
)

// permdecline.go — A DECLINE IS A STOP.
//
// # What a decline means
//
// Declining a permission prompt is the user taking the session back. It is not
// a hint the agent works around, and it never was meant to be: a user who says
// no to a tool call is saying "stop what you are doing; I will tell you what I
// want next". So a decline carries exactly the consequences a user's interrupt
// carries — the turn ends, the queue pauses, the interrupt window opens, and
// NOTHING further reaches the SDK until the user submits a prompt themselves.
//
// It used to mean the opposite. A denial travelled to the shim as a
// PermissionResponse carrying DENY, canUseTool resolved with it, and the agent
// read the denial as a tool result and KEPT GOING — picking a different tool,
// explaining itself, or asking again — which is the one thing a user who
// declined did not ask for.
//
// # Why the wire carries the stop and not the denial
//
// The decline is delivered by the Interrupt and by nothing else. permHandler
// returns a nil response for a denied request (sessioncontroller.go), so no
// PermissionResponse is written, and the shim's interrupt handler force-denies
// every parked canUseTool of its own accord (uds-session.ts interrupt →
// control.cancelAll) before it aborts the turn.
//
// THAT IS WHAT MAKES THE ORDERING A NON-QUESTION. A decline that sent both a
// PermissionResponse and an Interrupt would put two messages on the wire from
// two different goroutines — the response is written by the shimclient's own
// per-permission goroutine once the handler returns, the interrupt by the
// caller's — with no ordering between them. Interrupt-first would find the
// permission already cancelled and log the answer as unknown; response-first
// would let the SDK act on the denial before the stop landed. One message
// cannot race itself, so the sequence is not merely unlikely to invert: there
// is no second message to invert with.
//
// The deny message survives as the RECORDED reason on the DENIED permission
// item that frontends render. It is deliberately not agent-visible: the turn it
// would have been read in is over by construction.
//
// # The other way in
//
// A prompt submitted while a question is parked is the same event with the
// answer implied — the user has stopped reading the question and started typing
// the next instruction — so SubmitPrompt declines what is pending before it
// submits, through this same funnel. The prompt then takes the ordinary
// interrupted-queue path (queue.go: a head jump that runs alone and whose clean
// end resumes the drain), which is precisely what a manual decline followed by
// a manual prompt would have done.

// permissionSupersession is a submit's answer to "does this prompt decline what
// the workspace has parked?".
//
// It is a named type rather than a bare bool for the reason `submitter` is one:
// the fact belongs to the caller, and a positional bool at a nine-argument call
// site is unreadable at exactly the place the answer matters.
type permissionSupersession int

const (
	// leavesParkedPermissions is every prompt that is not the human's own: a
	// queued prompt being drained, a merge's prompt, a workspace-create job's
	// opening prompt, the daemon's keep-alive ping. None of them is the user
	// turning away from the question, so none of them answers it.
	leavesParkedPermissions permissionSupersession = iota
	// supersedesParkedPermissions is the frontend composer's own prompt: the
	// user stopped reading the question and started typing, which declines it.
	supersedesParkedPermissions
)

// declinePermissions releases the named parked permissions as DENIALS and then
// stops the workspace's turn, which is the one act that delivers them.
//
// THE STOP IS THE USER-COMMANDED ONE (Manager.Interrupt), deliberately, and not
// a bare client.Interrupt: a decline is the user taking the session back, so it
// owes the same three consequences a user's stop owes — the interrupt window,
// the `interrupted` turn outcome, and the paused queue. Routing it anywhere
// else would reproduce the interject's machinery stop, which means the opposite.
//
// It also skips the interrupt confirm gate (server/frontendcmd.go
// interruptChallenge), and must: the gate exists to question a stop aimed at
// subagent work the user may not have in mind, while a decline is aimed at a
// question the user is looking at.
//
// NOTHING IS STOPPED WHEN NOTHING WAS DECLINED. Every id that could not be
// released is a stale or duplicate answer — the request was already resolved —
// and a stop issued on one would end a turn on the strength of an answer that
// arrived twice. When no id at all was released the errors are returned and the
// turn is left alone; when some were, the stop is issued and every error
// travels back joined to its outcome, so a partially stale batch reports both
// halves rather than letting either hide the other.
func (m *Manager) declinePermissions(ctx context.Context, workspace, requestID, denyMessage string, ids []string) error {
	if len(ids) == 0 {
		return fmt.Errorf("session-controller: decline permissions ws=%q request_id=%s names no permission request", workspace, requestID)
	}
	released, errs := m.recordDeclines(workspace, requestID, denyMessage, ids)
	if released == 0 {
		m.logf("session-controller: permission decline released NOTHING ws=%q request_id=%s permission_request_ids=%v — every named request was already resolved, so no turn is stopped",
			workspace, requestID, ids)
		return errors.Join(errs...)
	}
	m.logf("session-controller: permission decline STOPPING the turn ws=%q request_id=%s declined=%d of %d — a decline is a user-commanded stop, so nothing more reaches the agent until the user prompts",
		workspace, requestID, released, len(ids))
	errs = append(errs, m.Interrupt(ctx, workspace, requestID))
	return errors.Join(errs...)
}

// recordDeclines releases each named request as a DENIAL and reports how many
// releases took, with every refusal collected rather than the first one winning.
//
// It is the one place a decline is written down, shared by the two callers that
// have opposite jobs afterwards: the funnel above, which stops the turn and owes
// the errors to its caller, and the post-stop sweep, which stops nothing and
// only logs. Both must agree on what a decline IS — one release, one line,
// nothing on the wire — and one function is how they cannot drift.
//
// A refusal here is always the same thing: the request resolved between the
// caller's read and this release. Nothing is retried and nothing is fabricated.
func (m *Manager) recordDeclines(workspace, requestID, denyMessage string, ids []string) (int, []error) {
	var errs []error
	released := 0
	for _, id := range ids {
		if err := m.reg.answerDecline(id, denyMessage); err != nil {
			errs = append(errs, err)
			m.logf("session-controller: permission decline found permission_request_id=%s ws=%q request_id=%s already resolved: %v",
				id, workspace, requestID, err)
			continue
		}
		released++
		m.logf("session-controller: permission DECLINED ws=%q request_id=%s permission_request_id=%s deny_message_present=%v — nothing is sent to the shim; the stop is what releases its canUseTool",
			workspace, requestID, id, denyMessage != "")
	}
	return released, errs
}

// declinePendingPermissionsForPrompt declines everything a workspace has parked
// because the user has just submitted a prompt, which is the answer stated by
// other means.
//
// A workspace with nothing parked is the ordinary case and is a NO-OP: there is
// no question to answer and, crucially, no turn to stop — an unconditional stop
// here would interrupt every prompt the user ever sends.
func (m *Manager) declinePendingPermissionsForPrompt(ctx context.Context, workspace, requestID string) error {
	ids := m.reg.idsForWorkspace(workspace)
	if len(ids) == 0 {
		return nil
	}
	m.logf("session-controller: prompt SUPERSEDES %d parked permission prompt(s) ws=%q request_id=%s — the user typed instead of answering, which declines the question and stops the turn before the prompt is submitted",
		len(ids), workspace, requestID)
	return m.declinePermissions(ctx, workspace, requestID, promptSupersededDenyMessage, ids)
}

// releaseParkedPermissionsOnStop declines every permission the workspace still
// has parked after a stop the shim acknowledged.
//
// IT MIRRORS WHAT THE SHIM ALREADY DID. An interrupt force-denies every blocked
// canUseTool on the shim's own side (uds-session.ts interrupt →
// control.cancelAll) and answers nothing back, so a daemon that kept its
// rendezvous would be holding a waiter for a round-trip that no longer exists:
// the question would stay PENDING on every frontend, the workspace would stay
// resolved to PERMISSION, and the pending badge would stay lit over a session
// with nothing to ask.
//
// It runs on EVERY acknowledged stop, not only on a decline, because the shim's
// cancellation is likewise unconditional. A decline reaches here having already
// released its own request under the user's reason; this then finds nothing,
// which is the correct no-op rather than a second answer.
func (m *Manager) releaseParkedPermissionsOnStop(workspace, requestID string) {
	ids := m.reg.idsForWorkspace(workspace)
	if len(ids) == 0 {
		return
	}
	m.logf("session-controller: stop RELEASES %d parked permission prompt(s) ws=%q request_id=%s — the shim cancelled them on its own side, so the daemon's rendezvous goes with them rather than waiting on questions nobody can answer",
		len(ids), workspace, requestID)
	// The errors are LOGGED BY recordDeclines and dropped here rather than
	// returned: a stop that already landed is not undone by a question that
	// resolved itself a moment earlier, and there is nothing a caller could
	// decide differently.
	m.recordDeclines(workspace, requestID, stoppedDenyMessage, ids)
}

// stoppedDenyMessage is the reason recorded on a permission a user's stop
// released. Like every other decline's reason it is a RECORD, not a message to
// the agent.
const stoppedDenyMessage = "declined: the user stopped the turn that asked"

// promptSupersededDenyMessage is the reason recorded on a permission the user
// declined by typing rather than by answering. It is a RECORD, not a message to
// the agent — see the file comment.
const promptSupersededDenyMessage = "declined: the user submitted a new prompt instead of answering"
