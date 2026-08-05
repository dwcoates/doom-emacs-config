package sessioncontroller

import (
	"context"
	"fmt"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/statedb"
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

// sessionCommand is what the daemon RECOGNIZED in one submitted prompt's text:
// a command it has its own consequences for, beyond handing the text along.
//
// A zero value is the ordinary prompt — text meant for the agent and nothing
// else — which is what the overwhelming majority of submits are.
type sessionCommand struct {
	// command is the recognized session command, or UNSPECIFIED for a prompt.
	// The set of recognizable commands and the matching rule both live in
	// sessioncommand.go, which is also where the wire enum they map to is
	// documented.
	command frontendv1.SessionCommand
}

// classifyPrompt reads a submitted prompt's text for meaning. THE ONLY PLACE
// the daemon does so.
func classifyPrompt(text string) sessionCommand {
	return sessionCommand{command: lookupSessionCommand(text)}
}

// recognized reports whether the daemon found a command it handles itself.
func (c sessionCommand) recognized() bool {
	return c.command != frontendv1.SessionCommand_SESSION_COMMAND_UNSPECIFIED
}

// clear reports whether the recognized command is the one that CUTS the
// conversation, which is the only session command with a state axis of its own.
func (c sessionCommand) clear() bool {
	return c.command == frontendv1.SessionCommand_SESSION_COMMAND_CLEAR
}

// echoes reports whether the prompt earns a receipt bubble in the frontend.
//
// A recognized command earns none. `/model` is not something the user SAID to
// the agent, it is something they DID to the session — the CLI answers it
// locally and the model never sees it — so a purple bubble reading "/model"
// claims a question was asked that nobody received. `/clear` is worse still:
// the cut already draws its own divider exactly where it happened, and the
// bubble sits ABOVE that divider, in the region the clear exists to discard.
//
// What the frontend gets instead is the invocation item (pushSessionCommand),
// which carries the command's identity and no text at all.
func (c sessionCommand) echoes() bool { return !c.recognized() }

// claimsTurn reports whether the submit takes the accepted-prompt state edge —
// the `submitting`/`thinking` claim, its durable turn latch, and the footer
// clock.
//
// EVERY RECOGNIZED COMMAND EXCEPT `/clear` DOES, and the split is not the same
// one `echoes` makes. A `/model` or a `/compact` really does occupy the shim:
// the CLI runs it and closes a turn with a result, so a workspace that stayed
// green through it would be lying about a session that is busy. `/clear` alone
// runs no turn — it cuts the conversation and re-inits — and its truthful
// status premise is the SSM's clearing axis (noteClearDispatched), which is why
// claiming a turn for it would be the false statement instead.
func (c sessionCommand) claimsTurn() bool { return !c.clear() }

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
func validatePromptOrigin(origin corev1.PromptOrigin) error {
	if origin == corev1.PromptOrigin_PROMPT_ORIGIN_UNSPECIFIED {
		return fmt.Errorf("session-controller: prompt origin must not be UNSPECIFIED")
	}
	if _, ok := corev1.PromptOrigin_name[int32(origin)]; !ok {
		return fmt.Errorf("session-controller: unknown prompt origin %d", origin)
	}
	return nil
}

func (m *Manager) forwardPrompt(ctx context.Context, d *sessionController, requestID, text, origin, permissionMode string, promptOrigin corev1.PromptOrigin, who submitter) error {
	// THE MERGE LEASE'S BACKSTOP. submitPromptAs already refused a user prompt
	// for a leased workspace, and this catches every path that does not pass
	// through it — the queue's drain, an interject's head jump, anything added
	// later. Both funnels end here by design, so the gate placed here is the one
	// a new caller cannot forget to ask for.
	if err := m.guardMergeLease(d.workspace, who, requestID, origin); err != nil {
		return err
	}
	// THE REVIVAL GATE'S BACKSTOP, at the funnel every one of the prompt paths
	// reaches — the immediate submit, the queue's drain, an interject's head
	// jump, a merge's own submit, and anything added later. submitPromptAs asks
	// first so a refused prompt never pays a bring-up; this is the one a new
	// caller cannot forget to ask for (hibernation.go).
	if err := m.guardHibernation(d.workspace, requestID, origin, who); err != nil {
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
	// `/clear` alone creates no edge: it is a session command that runs no turn,
	// and its clearing axis below is the truthful status premise. Every OTHER
	// session command does occupy the shim exactly as a prompt would (see
	// claimsTurn), so it takes the edge while still earning no bubble.
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
	//
	// THE TWO GATES ARE SEPARATE, and separating them is what lets a session
	// command be honest on both axes at once. `claimsTurn` decides whether the
	// shim is about to be busy; `echoes` decides whether the user said something.
	// A `/model` is the first without being the second, and collapsing the two
	// (as one gate did) forced a choice between a green workspace over a running
	// command and a purple bubble for a prompt nobody wrote. `echoes` implies
	// `claimsTurn` by construction — an ordinary prompt is both — so the receipt
	// below always sits inside a claimed turn.
	// THE KEEP-ALIVE PING EARNS NO BUBBLE AND MINTS NO RECEIPT, for the reason
	// `/model` does not: the user did not say it. It is conversation PLUMBING —
	// the daemon refreshing a cache — and a purple bubble reading "respond with
	// only a '.'" would claim a question the user never asked and would replay
	// as one across every reconnect, since the receipt is durable.
	//
	// It DOES claim the turn. The shim really is occupied for the length of the
	// ping, and a workspace that stayed green through it would be lying about a
	// session that is busy — the same split `/model` makes.
	claimsTurn := cmd.claimsTurn() && who != submitterMergeLeaseHolder
	echoes := cmd.echoes() && who != submitterMergeLeaseHolder && who != submitterKeepAlive

	accepted := false
	var turnBefore turnRecord
	var acceptedAtMs int64
	if claimsTurn {
		before, err := m.notePromptAccepted(d, requestID)
		if err != nil {
			// NOTHING EXTERNAL HAS HAPPENED YET, which is the whole advantage
			// of failing here rather than after the submit: the prompt is not
			// sent, no daemon-local frame is exposed without its state premise,
			// and the frontend command fails loudly on a session left exactly
			// as it was found. The user can resubmit.
			return err
		}
		accepted, turnBefore = true, before
	}

	// THE DURABLE RECEIPT is written only for a prompt that EARNS A BUBBLE. Its
	// sole purpose is replaying that bubble across a daemon bounce, so recording
	// one for a session command would resurrect exactly the "/model" bubble this
	// whole path exists to withhold — and would resurrect it from durable
	// storage, where nothing downstream could tell it from a real prompt.
	if echoes {
		// THE DURABLE RECEIPT, PART OF THE ACCEPTANCE ITSELF and therefore
		// ahead of BOTH the submit and the pushed bubble.
		//
		// The ordering is the guarantee. A receipt the user saw must never be
		// unrecoverable, so the record cannot come after the push; and a prompt
		// this daemon handed to a shim must never be lost, so it cannot come
		// after the submit either. Writing it here puts the durable evidence
		// ahead of everything that could make the prompt real to anyone else,
		// which makes "the user saw a bubble for a prompt with no record" and
		// "a shim is running a prompt with no record" both unrepresentable
		// rather than merely improbable.
		//
		// The window it opens instead is the honest one: a record for a prompt
		// whose submit then FAILS. That is closed on the failure path
		// (retractPromptAccepted), and a daemon that dies inside it replays a
		// receipt for a prompt the user genuinely typed and the daemon
		// genuinely accepted — which is the truth, and the strictly safer of
		// the two ways to be wrong.
		acceptedAtMs = m.now()
		if err := m.recordPromptReceipt(d, requestID, text, acceptedAtMs); err != nil {
			m.retractPromptAccepted(d, requestID, turnBefore, err)
			return err
		}
	}

	// THE SUBMITTED PROMPT CARRIES THE ID THE DAEMON ALREADY KEYED IT BY.
	// The shim adopts this request id as the turn_id of the TurnStarted and
	// TurnEnded it produces, so passing the daemon's own id is what makes the
	// daemon's name for the prompt and the durable ledger's name for its turn
	// ONE identity rather than two that need translating.
	//
	// It matters most where the daemon holds state keyed by that name before
	// the turn exists — the keep-alive ping's claim, its queue holds, its
	// window row and the dropped-turn list of the rewind that follows it. The
	// client used to mint its own id here, so the ping's end boundary named a
	// turn nothing was keyed by: the match at the boundary never fired, the
	// window never closed, and the pings rendered as the user's own prompts.
	if err := d.client.SubmitPrompt(ctx, requestID, text, origin, permissionMode, promptOrigin); err != nil {
		if accepted {
			// The `thinking` every frontend was just shown described a turn
			// that is not going to happen, and nothing else will ever close it:
			// the lifecycle retires `thinking` on a TurnEnded, and no turn that
			// never began reports an end.
			m.retractPromptAccepted(d, requestID, turnBefore, err)
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
	if echoes {
		m.echo(d, requestID, text, acceptedAtMs)
	}

	// THE INVOCATION ITEM, in the receipt's place and on the receipt's terms:
	// after the submit, so nothing is drawn for a command no session took.
	//
	// It is the ONLY thing the feed will ever say about this command. The
	// receipt was withheld above, and the CLI's own transcript bookkeeping for
	// the command is withheld as machinery (machinery.go), so without this the
	// user's `/model` would vanish from the conversation entirely and the
	// session's model would appear to change for no reason.
	m.noteSessionCommand(d, requestID, cmd)

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
func (m *Manager) notePromptAccepted(d *sessionController, requestID string) (turnBefore turnRecord, err error) {
	// The accepted edge is authoritative for BOTH user-visible state and queue
	// ordering. If the SSM says turn_active while the queue manager still says
	// idle, a second prompt can bypass the queue before the durable TurnStarted
	// arrives. The observed TurnStarted is an idempotent confirmation.
	m.mu.Lock()
	turnBefore = d.noteTurnAcceptedLocked(requestID)
	turnAfter := d.turn
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
		//
		// THIS RESTORES THIS SIDE'S HALF ONLY, and the SSM restores its own: a
		// failed accepted edge retracts the `submitting` row it appended before
		// returning (MarkPromptAccepted), so the two representations of "a turn
		// is claimed" cannot come apart on the failure path. They used to, and
		// the surviving row wedged the workspace against every later prompt.
		m.mu.Lock()
		d.noteTurnRestoreLocked(turnBefore)
		m.mu.Unlock()
		err = fmt.Errorf("session-controller: synchronous state publication failed before submitting for workspace %q session %q request %q: %w",
			d.workspace, d.sessionID, requestID, err)
		m.logf("session-controller: prompt accepted state edge FAILED ws=%s session=%s request_id=%q session_controller_turn_before=%s session_controller_turn_after=%s publish_sync=true prompt_submitted=false dependent_frames=withheld error=%v",
			d.workspace, d.sessionID, requestID, turnBefore, turnBefore, err)
		return turnBefore, err
	}
	m.logf("session-controller: prompt accepted state edge APPLIED ws=%s session=%s request_id=%q session_controller_turn_before=%s session_controller_turn_after=%s publish_sync=true next=shim_submit_then_prompt_echo",
		d.workspace, d.sessionID, requestID, turnBefore, turnAfter)
	return turnBefore, nil
}

// recordPromptReceipt persists the durable evidence that this daemon accepted
// one user prompt, BEFORE the prompt reaches a shim or its bubble reaches a
// frontend.
//
// A caller with no request id behind it (an internal re-submit, a harness)
// records nothing, exactly as it pushes nothing: the record is keyed by the
// identity the frontend reconciles the bubble on, and a minted id would name a
// bubble nothing could ever claim.
//
// A WRITE FAILURE FAILS THE SUBMIT. This is a write to the same state store the
// accepted edge just wrote to, so a failure here is a state store that cannot
// be written — the condition under which every durable claim the daemon makes
// is already void. Carrying on would submit a prompt the daemon has no record
// of, which is precisely the loss this whole mechanism exists to end, so the
// caller retracts the accepted edge and fails the frontend command instead.
func (m *Manager) recordPromptReceipt(d *sessionController, requestID, text string, acceptedAtMs int64) error {
	if requestID == "" {
		return nil
	}
	if m.cfg.PromptReceipts == nil {
		m.logf("session-controller: durable prompt receipt NOT recorded ws=%s session=%s request_id=%q accepted_at_ms=%d — no PromptReceiptStore is wired, so this prompt cannot be replayed if the daemon dies before its turn becomes durable",
			d.workspace, d.sessionID, requestID, acceptedAtMs)
		return nil
	}
	if err := m.cfg.PromptReceipts.Record(statedb.PromptReceipt{
		RequestID:    requestID,
		Workspace:    d.workspace,
		Text:         text,
		AcceptedAtMs: acceptedAtMs,
	}); err != nil {
		err = fmt.Errorf("session-controller: recording the durable prompt receipt for workspace %q session %q request %q failed before submitting: %w",
			d.workspace, d.sessionID, requestID, err)
		m.logf("session-controller: durable prompt receipt record FAILED ws=%s session=%s request_id=%q accepted_at_ms=%d len=%d prompt_submitted=false: %v",
			d.workspace, d.sessionID, requestID, acceptedAtMs, len(text), err)
		return err
	}
	m.logf("session-controller: durable prompt receipt recorded ws=%s session=%s request_id=%q accepted_at_ms=%d len=%d next=shim_submit_then_prompt_echo",
		d.workspace, d.sessionID, requestID, acceptedAtMs, len(text))
	return nil
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
func (m *Manager) retractPromptAccepted(d *sessionController, requestID string, turnBefore turnRecord, cause error) {
	m.mu.Lock()
	d.noteTurnRestoreLocked(turnBefore)
	m.mu.Unlock()

	// The durable receipt goes with the edge it was written beside. It was
	// recorded on the daemon's INTENT to submit, and that intent has now been
	// falsified, so replaying a bubble for it after a bounce would testify to a
	// prompt no session ever received. This is the one window the accept-time
	// write opens, and this is where it closes.
	if requestID != "" {
		d.consumer.retireDurableReceipt(requestID, "submit_failed_after_acceptance")
	}

	publish := func(state *frontendv1.WorkspaceState) {
		d.consumer.push.PushWorkspaceState(state)
	}
	retracted, err := m.cfg.SSM.MarkPromptRejected(d.workspace, d.sessionID, requestID, publish)
	if err != nil {
		m.logf("session-controller: prompt rejected state edge FAILED ws=%s session=%s request_id=%q session_controller_turn_restored=%s submit_error=%v error=%v (the workspace may hold `thinking` for a turn that never began)",
			d.workspace, d.sessionID, requestID, turnBefore, cause, err)
		return
	}
	if !retracted {
		m.logf("session-controller: prompt rejected state edge PRESERVED ws=%s session=%s request_id=%q session_controller_turn_restored=%s submit_error=%v — something more authoritative owns the state axis, so neither it nor the footer clock is touched",
			d.workspace, d.sessionID, requestID, turnBefore, cause)
		return
	}
	m.progress().NoteTurnRejected(d.workspace, d.sessionID)
	m.logf("session-controller: prompt rejected state edge APPLIED ws=%s session=%s request_id=%q session_controller_turn_restored=%s publish_sync=true turn_clock=closed submit_error=%v",
		d.workspace, d.sessionID, requestID, turnBefore, cause)
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

// noteSessionCommand pushes the invocation item for a recognized session
// command the daemon just handed to a shim, and does nothing at all for an
// ordinary prompt.
//
// Takes the CLASSIFICATION rather than the text, which is what keeps the
// recognition in one place — and, here, is also what keeps the text off the
// wire: this function is not given the prompt, so it cannot pass it on.
//
// A submit with no request id behind it (an internal re-submit, a harness)
// pushes nothing, exactly as the receipt path does: the item's uuid is derived
// from that identity, and a minted one would name an item nothing could ever
// reconcile or replace.
//
// Must be called with m.mu RELEASED: the push reaches the frontend server.
func (m *Manager) noteSessionCommand(d *sessionController, requestID string, cmd sessionCommand) {
	if !cmd.recognized() || requestID == "" {
		return
	}
	d.consumer.pushSessionCommand(requestID, cmd.command)
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
	if !cmd.clear() {
		return
	}
	m.logf("session-controller: /clear dispatched ws=%q — opening the SSM's clearing axis until the vendor session rotation it causes lands, or its ContextCleared arrives first", workspace)
	if err := m.cfg.SSM.ApplyClearing(workspace, true, "clear_dispatched"); err != nil {
		m.logf("session-controller: opening the clearing axis FAILED ws=%q: %v (the cut will render as an ordinary turn)", workspace, err)
	}
}
