package sessioncontroller

import (
	"context"
	"fmt"
	"strings"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// This file is the daemon's whole reading of a submitted prompt: the one place
// that looks at what the user typed and decides what it MEANS, and the one
// place that acts on the answer.
//
// It is one file rather than two because the consequences of that reading are
// not independent of each other. A `/clear` is not a prompt at all — it is an
// instruction to the CLI to discard the conversation — so it must not be
// echoed as a purple bubble, and must open the SSM's clearing axis. Those used
// to be decided in different places from different readings of the string, and
// they disagreed:
//
//   - the receipt was pushed before anything was recognized, so `/clear` drew a
//     bubble reading "/clear" beside the red divider that reported the very same
//     cut;
//   - the queue's delivery path recognized nothing at all, so a `/clear` held
//     behind a running turn was cut with no axis open either.
//
// Deciding all of it from a single classification at a single site is what makes
// those disagreements unrepresentable rather than merely fixed.

// clearSessionCommand is the session command that discards a conversation's
// context. Matched on the SUBMITTED text, which is where the daemon sees it:
// the harness recognizes `/clear` itself and expands it into a command
// envelope inside the CLI, so by the time anything is on the file plane the
// clear has already happened.
const clearSessionCommand = "/clear"

// sessionCommand is what the daemon RECOGNIZED in one submitted prompt's text:
// a command it has its own consequences for, beyond handing the text along.
//
// A zero value is the ordinary prompt — text meant for the agent and nothing
// else — which is what the overwhelming majority of submits are.
type sessionCommand struct {
	// clear is a bare `/clear`, the command that cuts the conversation.
	clear bool
}

// classifyPrompt reads a submitted prompt's text for meaning. THE ONLY PLACE
// the daemon does so.
//
// Matched on the whole trimmed string: an argument after the command means the
// user asked for something else entirely ("/clear the build cache" is a prompt),
// and the CLI's own expansion is just as literal — it recognizes the command
// only when the command is the entire prompt.
func classifyPrompt(text string) sessionCommand {
	return sessionCommand{clear: strings.TrimSpace(text) == clearSessionCommand}
}

// recognized reports whether the daemon found a command it handles itself.
func (c sessionCommand) recognized() bool { return c.clear }

// echoes reports whether the prompt earns a receipt bubble in the frontend.
//
// A recognized command earns none. `/clear` is not something the user SAID to
// the agent, it is something they DID to the conversation, and the cut already
// draws its own divider exactly where it happened. A bubble beside that divider
// reading "/clear" is the machinery narrating itself, and it is worse than
// noise: it sits above the divider, which is the region the clear exists to
// discard.
func (c sessionCommand) echoes() bool { return !c.recognized() }

// forwardPrompt hands one submitted prompt to its shim.
//
// BOTH SUBMIT FUNNELS END HERE — the immediate submit (submitPrompt) and the
// queue's delivery (deliver) — and that is the point of it. The recognition and
// the receipt are decided together, from a single reading of a single string, so
// a command the daemon handles specially can never also be echoed to the
// frontend as though it were ordinary prompt text.
//
// requestID is the frontend command's own id, empty for a caller with no
// frontend request behind it. origin is the vendor-visible provenance.
//
// Must be called with m.mu RELEASED: the receipt reaches the frontend server.
func (m *Manager) forwardPrompt(ctx context.Context, d *sessionController, requestID, text, origin, permissionMode string, who submitter) error {
	// THE MERGE LEASE'S BACKSTOP. submitPromptAs already refused a user prompt
	// for a leased workspace, and this catches every path that does not pass
	// through it — the queue's drain, an interject's head jump, anything added
	// later. Both funnels end here by design, so the gate placed here is the one
	// a new caller cannot forget to ask for.
	if err := m.guardMergeLease(d.workspace, who, requestID, origin); err != nil {
		return err
	}
	cmd := classifyPrompt(text)

	// THE ACCEPTED EDGE AND ITS SYNCHRONOUS PUBLICATION, BEFORE THE SUBMIT.
	//
	// It used to follow the shim's Ack, on the reasoning that the daemon should
	// not claim a turn the shim had not confirmed. The cost of that ordering is
	// paid by the user on every prompt: the Ack is a round-trip to a node
	// process that is frequently busy serving the stream this very workspace is
	// about to produce, so the workspace stayed GREEN for the whole of it —
	// which is the one window in which a user cannot tell a prompt that was
	// sent from one that was dropped, and the window they reported as "it takes
	// too long to register as in flight".
	//
	// So the claim is now made on the daemon's OWN decision to submit, which it
	// has already taken by this line, and the honesty of the claim is
	// maintained on the failure path instead (retractPromptAccepted below).
	// Only an ordinary prompt creates the edge: `/clear` is a session command,
	// not a turn, and its clearing axis below is the truthful status premise.
	// MarkPromptAccepted holds the SSM transition lock through the frontend
	// publication, so neither the asynchronous SSM subscriber nor a later
	// TurnEnded can overtake it.
	// THE MERGE'S OWN PROMPT IS MACHINERY, NOT THE USER'S, so it takes neither
	// the accepted edge nor the receipt: the user did not type it.
	//
	// It is also the only honest reading of the state axis. A workspace whose
	// merge parked on a conflict IS `merge_conflict`, and the SSM (correctly)
	// refuses to publish a `submitting` premise over it — the accepted edge's
	// invariant fails outright, so claiming the edge here would fail every
	// conflict-resolution submit on a state the merge axis rightly owns. The
	// turn itself is still claimed durably, by the shim's own TurnStarted.
	accepted, activeBefore := false, false
	if cmd.echoes() && who != submitterMergeLeaseHolder {
		before, err := m.notePromptAccepted(d, requestID)
		if err != nil {
			// NOTHING EXTERNAL HAS HAPPENED YET, which is the whole advantage
			// of failing here rather than after the submit: the prompt is not
			// sent, no daemon-local frame is exposed without its state premise,
			// and the frontend command fails loudly on a session left exactly
			// as it was found. The user can resubmit.
			return err
		}
		accepted, activeBefore = true, before
	}

	if err := d.client.SubmitPrompt(ctx, text, origin, permissionMode); err != nil {
		if accepted {
			// The `thinking` every frontend was just shown described a turn
			// that is not going to happen, and nothing else will ever close it:
			// the lifecycle retires `thinking` on a TurnEnded, and no turn that
			// never began reports an end.
			m.retractPromptAccepted(d, requestID, activeBefore, err)
		}
		return err
	}

	// THE DELIVERED EDGE: the shim has the prompt, so the turn advances from
	// `submitting` to `thinking`. This is the whole reason the two are split —
	// the accepted edge above publishes on the daemon's own intent, and only
	// here is the agent actually holding anything.
	if accepted {
		m.notePromptDelivered(d, requestID)
	}

	// THE RECEIPT, only after every frontend has been synchronously offered the
	// accepted prompt's `thinking` state, and only once the shim has actually
	// TAKEN the prompt. It closes the transcript-latency gap, but never at the
	// cost of a green prompt bubble — and never at the cost of a bubble for a
	// prompt no session received, which is why it stays behind the submit while
	// the state edge moved ahead of it: a state edge can be retracted, a
	// conversation item the frontend has already drawn cannot.
	if accepted {
		m.echo(d, requestID, text)
	}

	// AFTER THE ACCEPT, never before: an axis opened for a prompt that never
	// reached the shim would be waiting on a cut that is not coming, and would
	// hold the phase word until the watchdog expired it.
	m.noteClearDispatched(d.workspace, cmd)
	return nil
}

// notePromptAccepted applies every daemon-local consequence of the daemon
// committing to submit an immediately delivered prompt, and reports the queue
// latch's value BEFORE the edge — what a retraction must restore.
//
// Every outward mutation happens before the prompt is handed to the shim: the
// SSM edge makes every status surface `thinking`, and the progress edge starts
// the footer clock. The session controller's queue latch moves on the same
// edge. Waiting for the durable TurnStarted would let a second prompt bypass
// the queue while every frontend already reported an active turn.
func (m *Manager) notePromptAccepted(d *sessionController, requestID string) (activeBefore bool, err error) {
	// The accepted edge is authoritative for BOTH user-visible state and queue
	// ordering. If the SSM says turn_active while the queue manager still says
	// idle, a second prompt can bypass the queue before the durable TurnStarted
	// arrives. The observed TurnStarted is an idempotent confirmation.
	m.mu.Lock()
	controllerActiveBefore := d.turnActive
	d.turnActive = true
	m.mu.Unlock()

	publish := func(state *frontendv1.WorkspaceState) {
		// Clear the prior turn's interrupt window only after the SSM has
		// validated and committed this edge, but before the active state crosses
		// the frontend boundary. This makes ALREADY_COMPLETE plus SUBMITTING or
		// THINKING unrepresentable without mutating progress on an SSM failure.
		if progress := m.progress().NoteTurnAccepted(d.workspace, d.sessionID); progress != nil {
			d.consumer.push.PushProgressView(progress)
		}
		d.consumer.push.PushWorkspaceState(state)
	}
	if err := m.cfg.SSM.MarkPromptAccepted(d.workspace, d.sessionID, requestID, publish); err != nil {
		// The prompt has NOT been submitted yet, so the latch is put back where
		// it was found: a session left claiming a turn that was never started
		// would queue every subsequent prompt behind a turn end that can never
		// arrive. Fail the frontend command loudly and withhold every dependent
		// frame.
		m.mu.Lock()
		d.turnActive = controllerActiveBefore
		m.mu.Unlock()
		err = fmt.Errorf("session-controller: synchronous state publication failed before submitting for workspace %q session %q request %q: %w",
			d.workspace, d.sessionID, requestID, err)
		m.logf("session-controller: prompt accepted state edge FAILED ws=%s session=%s request_id=%q session_controller_turn_active_before=%v session_controller_turn_active_after=%v publish_sync=true prompt_submitted=false dependent_frames=withheld error=%v",
			d.workspace, d.sessionID, requestID, controllerActiveBefore, controllerActiveBefore, err)
		return controllerActiveBefore, err
	}
	m.logf("session-controller: prompt accepted state edge APPLIED ws=%s session=%s request_id=%q session_controller_turn_active_before=%v session_controller_turn_active_after=true publish_sync=true next=shim_submit_then_prompt_echo",
		d.workspace, d.sessionID, requestID, controllerActiveBefore)
	return controllerActiveBefore, nil
}

// retractPromptAccepted undoes notePromptAccepted for a submit the shim
// refused, restoring the queue latch, withdrawing the published `thinking`, and
// closing the footer clock.
//
// THE PRICE OF PUBLISHING EARLY, and the reason publishing early is safe. The
// accepted edge is now a statement of intent rather than of confirmed fact, so
// exactly one path can falsify it — this one — and it runs before the failing
// frontend command returns, so the workspace is green again by the time the
// user is told the prompt did not go.
//
// The latch is restored FIRST and unconditionally: it is daemon-local, nothing
// else can have a claim on it, and leaving it set would queue every later
// prompt behind a turn that no TurnEnded is coming for. The two published
// surfaces are gated on the SSM confirming it actually retracted the row,
// because a durable TurnStarted (or a permission, or a cut) landing in the
// window between the accept and the failure means a real turn now owns the
// axis, and closing THAT would report an idle workspace over a working session.
//
// Every failure here is loud-logged and swallowed: the caller is already
// returning the submit's own error, which is the news, and a retraction failure
// must not replace the account of why the prompt did not go.
func (m *Manager) retractPromptAccepted(d *sessionController, requestID string, activeBefore bool, cause error) {
	m.mu.Lock()
	d.turnActive = activeBefore
	m.mu.Unlock()

	publish := func(state *frontendv1.WorkspaceState) {
		d.consumer.push.PushWorkspaceState(state)
	}
	retracted, err := m.cfg.SSM.MarkPromptRejected(d.workspace, d.sessionID, requestID, publish)
	if err != nil {
		m.logf("session-controller: prompt rejected state edge FAILED ws=%s session=%s request_id=%q session_controller_turn_active_restored=%v submit_error=%v error=%v (the workspace may hold `thinking` for a turn that never began)",
			d.workspace, d.sessionID, requestID, activeBefore, cause, err)
		return
	}
	if !retracted {
		m.logf("session-controller: prompt rejected state edge PRESERVED ws=%s session=%s request_id=%q session_controller_turn_active_restored=%v submit_error=%v — something more authoritative owns the state axis, so neither it nor the footer clock is touched",
			d.workspace, d.sessionID, requestID, activeBefore, cause)
		return
	}
	m.progress().NoteTurnRejected(d.workspace, d.sessionID)
	m.logf("session-controller: prompt rejected state edge APPLIED ws=%s session=%s request_id=%q session_controller_turn_active_restored=%v publish_sync=true turn_clock=closed submit_error=%v",
		d.workspace, d.sessionID, requestID, activeBefore, cause)
}

// notePromptDelivered advances the workspace from `submitting` to `thinking` on
// the shim's ack.
//
// A failure is loud-logged and SWALLOWED, unlike the accepted edge's. The prompt
// has already reached the agent, so there is nothing to fail back to the caller,
// and failing the submit over a phase word would report a prompt as lost when it
// is running. The durable TurnStarted still arrives to state the same thing.
func (m *Manager) notePromptDelivered(d *sessionController, requestID string) {
	advanced, err := m.cfg.SSM.MarkPromptDelivered(d.workspace, d.sessionID, requestID)
	if err != nil {
		m.logf("session-controller: prompt delivered state edge FAILED ws=%s session=%s request_id=%q error=%v (the workspace holds `submitting` until the durable TurnStarted lands)",
			d.workspace, d.sessionID, requestID, err)
		return
	}
	if !advanced {
		m.logf("session-controller: prompt delivered state edge PRESERVED ws=%s session=%s request_id=%q — something more authoritative already owns the state axis",
			d.workspace, d.sessionID, requestID)
		return
	}
	m.logf("session-controller: prompt delivered state edge APPLIED ws=%s session=%s request_id=%q submitting->thinking",
		d.workspace, d.sessionID, requestID)
}

// noteClearDispatched opens the SSM's clearing axis for a `/clear` the daemon
// just handed to a shim.
//
// THE DAEMON IS THE ONLY THING THAT KNOWS. Nothing in the event stream announces
// a clear as it BEGINS — the first-class ContextCleared reports one that already
// finished — so a footer that waited for an event would say `thinking` through
// the entire cut and then jump straight to the cleared bubble.
//
// Takes the CLASSIFICATION rather than the text, which is what keeps the
// recognition in one place: there is no second reading here to drift from the
// one the receipt was decided by. A failure is loud-logged and does not fail the
// submit — the prompt was accepted, and losing it over a footer word would be
// the larger harm.
func (m *Manager) noteClearDispatched(workspace string, cmd sessionCommand) {
	if !cmd.clear {
		return
	}
	m.logf("session-controller: /clear dispatched ws=%q — opening the SSM's clearing axis until its ContextCleared lands", workspace)
	if err := m.cfg.SSM.ApplyClearing(workspace, true, "clear_dispatched"); err != nil {
		m.logf("session-controller: opening the clearing axis FAILED ws=%q: %v (the cut will render as an ordinary turn)", workspace, err)
	}
}
