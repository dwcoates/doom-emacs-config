package sessioncontroller

import (
	"context"
	"fmt"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"

	"claude-repld/internal/frontend"
	"claude-repld/internal/statedb"
)

// turnresumption.go — WHAT A BOUNCE OWES THE USER.
//
// A planned bounce that must take the shim with it interrupts whatever turn was
// in flight. The interrupt is right: the shim holds the vendor SDK session, it
// is about to be SIGTERMed, and its own interrupt is the only way that turn's
// end gets reported honestly rather than synthesized over a corpse.
//
// What was WRONG is that the interrupt was the end state. The user asked for
// work; a deploy landed on top of it; the work was silently dropped and the
// turn was recorded as interrupted, which reads to the user as "the thing I
// asked for was cancelled and nobody will tell me why". Nothing in the system
// remembered anything was owed.
//
// # Why there is no native mechanism to lean on
//
// The vendor SDK has no resume-the-interrupted-turn primitive. `resume` loads a
// session's conversation history, `continue` picks the most recent
// conversation, and `resumeSessionAt` narrows a resume to a message uuid — all
// three restore CONTEXT, and none of them restarts an aborted turn. `interrupt`
// itself reports which queued messages survive and re-enqueues nothing. So a
// successor daemon that wants the work to continue has to ask for it.
//
// # The re-drive is a daemon fact, and it is invisible
//
// It follows that the daemon must submit something, and that the something is
// NOT a prompt the user wrote. It is never rendered: it is not a conversation
// item, not a prompt echo, and not a visible turn origin. Three separate things
// enforce that, at the three places the text can re-enter from:
//
//   - the daemon's own receipt ledger cannot serve it, because a pending
//     resumption is a prompt_receipt row the receipt query does not return
//     (statedb/promptreceipt.go);
//   - the VENDOR conversation's copy — a real transcript line, because the
//     model has to be told something — is suppressed by the one curator every
//     route from a store event to conversation content passes through, keyed on
//     a marker the submitted text carries into the transcript itself
//     (frontend/internalresume.go); and
//   - a re-drive is issued AT MOST ONCE per interrupted turn, because the row
//     is claimed before the submit, so a thrashing bring-up cannot put several
//     copies of the instruction into the conversation for a filter to have to
//     hide.
//
// The second of those is what the live incident of 2026-08-10 was missing. The
// curator filtered on the EVENT's request id, and a file-plane user line has
// none: the daemon correlates a line to a submit from its outstanding receipts,
// and a re-drive mints no receipt, so the instruction arrived unattributed and
// rendered — live, and again on every replay.
//
// The only thing the user sees is the status surface saying the session resumed
// after a restart, and the work continuing.

// resumptionInstruction is the re-drive's text.
//
// IT IS DELIBERATELY NOT A RESTATEMENT OF THE USER'S PROMPT. The vendor session
// is resumed with its full transcript, so the interrupted request is already in
// the model's context, verbatim, in its real place. Repeating it would put a
// second copy of the user's words into the conversation as though they had
// asked twice — which is both wrong and precisely the duplicate the invisibility
// requirement exists to prevent. The instruction names the situation instead
// and lets the resumed context supply the task.
const resumptionInstruction = "Your previous turn was interrupted by a planned restart of the tooling that hosts this session, not by the user. Continue that interrupted work from where it stopped. Do not restate or re-plan it, and do not ask the user to repeat themselves."

// resumptionRequestIDPrefix marks a request id as a re-drive's.
//
// The id is minted at TEARDOWN, before the submit it names exists, which is
// what makes the re-drive recognizable as this record's discharge rather than
// as a fresh prompt that happens to look similar.
//
// IT IS THE CURATOR'S CONSTANT, not a second copy of it. The one place the
// re-drive is made invisible is frontend.CurateEvent, which keys on exactly
// this prefix; a private duplicate here would be two strings that have to be
// kept equal for the invisibility to hold.
const resumptionRequestIDPrefix = frontend.InternalResumeRequestIDPrefix

// recordInterruptedTurnResumption durably records that this teardown is about
// to interrupt a turn the successor daemon owes the user.
//
// IT IS ADVISORY IN EXACTLY ONE DIRECTION. A teardown that cannot record the
// resumption still tears down: the alternative is a daemon that refuses to shut
// down because a bookkeeping row would not write, which trades a lost turn for
// a stuck deploy. But it is never silent — a resumption that failed to record
// is work the user will never get back, so it is logged as loudly as the
// interrupt it accompanies.
func (m *Manager) recordInterruptedTurnResumption(workspace, sessionID string, cause StopCause, turnID string) {
	if !cause.owesTurnResumption() {
		m.logf("session-controller: teardown turn resumption NOT OWED ws=%q session=%s path=%s turn_id=%q — this stop ends the work on purpose rather than displacing it, so the interrupted turn is not re-driven",
			workspace, sessionID, cause.path(), turnID)
		return
	}
	if m.cfg.PromptReceipts == nil {
		m.logf("session-controller: teardown turn resumption NOT RECORDED ws=%q session=%s path=%s turn_id=%q — no PromptReceiptStore is wired, so the turn this bounce interrupts will not be re-driven",
			workspace, sessionID, cause.path(), turnID)
		return
	}
	// ONE reading of the clock, because the request id is derived from the
	// instant: two readings would mint an id naming a moment the row does not
	// record, and the collision that makes a retried teardown idempotent would
	// stop happening.
	at := m.now()
	rec := statedb.PendingResumption{
		RequestID:       resumptionRequestID(workspace, turnID, at),
		Workspace:       workspace,
		TurnID:          turnID,
		Text:            resumptionInstruction,
		InterruptedAtMs: at,
	}
	if err := m.cfg.PromptReceipts.RecordPendingResumption(rec); err != nil {
		m.logf("session-controller: teardown turn resumption RECORD FAILED ws=%q session=%s path=%s turn_id=%q request_id=%q: %v — the teardown proceeds, and the interrupted turn is lost work rather than owed work",
			workspace, sessionID, cause.path(), turnID, rec.RequestID, err)
		return
	}
	m.logf("session-controller: teardown turn resumption RECORDED ws=%q session=%s path=%s turn_id=%q request_id=%q interrupted_at_ms=%d — the successor daemon re-drives this turn once the session is wired again",
		workspace, sessionID, cause.path(), turnID, rec.RequestID, rec.InterruptedAtMs)
}

// driveOwedResumptions re-drives every turn this workspace is still owed.
//
// IT IS LEVEL-TRIGGERED, and that is the whole of the exactly-once story. It
// asks the STORE what is owed rather than remembering what it queued, so:
//
//   - a daemon that died between recording and re-driving is indistinguishable
//     from one that never got round to it — both leave the row, and the next
//     daemon to wire the session finds it;
//   - a SECOND bounce landing during the resumption re-drives the same row
//     rather than a second one, because the row is only discharged when the
//     re-driven turn is accepted; and
//   - two wire events for one session (a reattach followed by a respawn) find
//     an empty set the second time, because the first discharged it.
//
// It runs on its own goroutine because the caller is the wire hook, which holds
// the manager mutex and must not block a bring-up on a submit.
func (m *Manager) driveOwedResumptions(workspace, sessionID string) {
	if m.cfg.PromptReceipts == nil {
		return
	}
	owed, err := m.cfg.PromptReceipts.PendingResumptions(workspace)
	if err != nil {
		// NEVER read as "nothing is owed". An unreadable store means the answer
		// is unknown, and the honest report of an unknown is a loud line rather
		// than a silent no.
		m.logf("session-controller: turn resumption READ FAILED ws=%q session=%s: %v — whether this session owes a re-drive is UNKNOWN, so none is issued and the record stands for the next wire",
			workspace, sessionID, err)
		return
	}
	if len(owed) == 0 {
		return
	}
	m.logf("session-controller: turn resumption OWED ws=%q session=%s count=%d — the session is wired again, so the turns the last bounce interrupted are re-driven",
		workspace, sessionID, len(owed))
	for _, r := range owed {
		m.driveOneResumption(workspace, sessionID, r)
	}
}

// driveOneResumption claims one owed re-drive and submits it.
//
// THE CLAIM COMES FIRST, AND IT IS WHAT MAKES THE RE-DRIVE HAPPEN ONCE. The
// store moves the row pending → delivering in a single conditional update, so
// the caller that comes away with the claim is the only one that submits — a
// second wire of the same session, or a second daemon, is told the turn is
// already being delivered and does nothing.
//
// THE DISCHARGE IS NOT HERE, and this is the correction the incident forced. A
// submit's return says only what the daemon's control request was told: a
// timeout on a shim that ran the prompt anyway looks exactly like a refusal, so
// re-driving on that evidence is choosing the duplicate and discharging on it is
// choosing the loss. The row is discharged where the fact is knowable — when the
// instruction turns up in the vendor conversation and the curator suppresses it
// (frontend/internalresume.go, dischargeDeliveredResumptions).
//
// A SUBMIT THAT FAILED THEREFORE LEAVES A CLAIMED ROW, deliberately: the turn is
// not re-driven again, and the row stands as the durable evidence that it was
// owed. It is loud, because a turn the user asked for and never got back is
// exactly what this whole path exists to prevent.
func (m *Manager) driveOneResumption(workspace, sessionID string, r statedb.PendingResumption) {
	claimed, err := m.cfg.PromptReceipts.ClaimResumptionForDelivery(r.RequestID, m.now())
	if err != nil {
		m.logf("session-controller: turn resumption CLAIM FAILED ws=%q session=%s request_id=%q turn_id=%q: %v — no re-drive is issued, because a submit whose claim did not durably land could be issued a second time by the next wire",
			workspace, sessionID, r.RequestID, r.TurnID, err)
		return
	}
	if !claimed {
		m.logf("session-controller: turn resumption ALREADY CLAIMED ws=%q session=%s request_id=%q turn_id=%q — another wire is delivering this same interrupted turn, so this one submits nothing",
			workspace, sessionID, r.RequestID, r.TurnID)
		return
	}
	ctx, cancel := context.WithTimeout(m.rootCtx, resumptionSubmitTimeout)
	defer cancel()
	// THE MARKER IS PART OF WHAT IS SUBMITTED (frontend/internalresume.go). The
	// vendor writes the submitted text into the transcript verbatim, so this is
	// what puts the re-drive's identity on disk — the only thing that makes the
	// instruction suppressible on a replay, where no request id survives.
	_, err = m.submitPromptAs(
		ctx, workspace, r.RequestID, frontend.MarkInternalResumeInstruction(r.RequestID, r.Text), "",
		"turn-resumption", corev1.PromptOrigin_PROMPT_ORIGIN_RESUME_AFTER_RESTART,
		submitterTurnResumption, leavesParkedPermissions,
	)
	if err != nil {
		m.logf("session-controller: turn resumption SUBMIT FAILED ws=%q session=%s request_id=%q turn_id=%q: %v — the record stands CLAIMED and is NOT re-driven again, because the shim may have run a submit this daemon was told failed; the interrupted turn is lost work unless the user asks for it again",
			workspace, sessionID, r.RequestID, r.TurnID, err)
		return
	}
	m.logf("session-controller: turn resumption RE-DRIVEN ws=%q session=%s request_id=%q turn_id=%q interrupted_at_ms=%d — the work the bounce interrupted continues, with no prompt shown to the user; the record clears when the instruction reaches the conversation",
		workspace, sessionID, r.RequestID, r.TurnID, r.InterruptedAtMs)
}

// dischargeDeliveredResumptions retires the resumptions whose instruction has
// now been seen in the vendor conversation.
//
// IT IS THE ONLY CONFIRMED DELIVERY THERE IS. The submit's own return describes
// a control request, not a conversation; the transcript carrying the
// instruction is the fact that the re-drive landed, and it is a fact a replay
// re-establishes just as well as the live push does. The ids come from the
// curator that suppressed the instruction, so the daemon reads the delivery off
// the same decision that hides it rather than deriving it a second time.
//
// IT IS IDEMPOTENT. Discharging a resumption that is already gone reports false
// with no error, which is the ordinary case on every replay of a conversation
// whose re-drive was discharged long ago.
func (c *consumer) dischargeDeliveredResumptions(requestIDs []string) {
	if len(requestIDs) == 0 {
		return
	}
	if c.receipts == nil {
		c.logf("session-controller: turn resumption NOT DISCHARGED ws=%q session=%s request_ids=%v — no durable receipt store is wired to this session controller, so the delivered re-drive's record stands",
			c.workspace, c.sessionID, requestIDs)
		return
	}
	for _, requestID := range requestIDs {
		discharged, err := c.receipts.DischargeResumption(requestID)
		if err != nil {
			// LOUD, AND THE STREAM CONTINUES. The instruction is already
			// suppressed, so nothing the user sees is wrong; what stands is a
			// row for a re-drive that has demonstrably landed, and the honest
			// report of that is a line rather than a stalled conversation.
			c.logf("session-controller: turn resumption DISCHARGE FAILED ws=%q session=%s request_id=%q: %v — the instruction is in the conversation, so the record is stale rather than owed, and it stays until a later delivery of the same line clears it",
				c.workspace, c.sessionID, requestID, err)
			continue
		}
		if !discharged {
			continue // already discharged: every replay of this line reaches here
		}
		c.logf("session-controller: turn resumption DELIVERED ws=%q session=%s request_id=%q — the re-drive's instruction is in the vendor conversation, so the interrupted turn is no longer owed",
			c.workspace, c.sessionID, requestID)
	}
}

// cancelOwedResumptions discards what a workspace is owed because the USER got
// there first.
//
// THE PREEMPTION IS REAL AND IT IS LOUD. Someone who submits a new prompt, or
// interrupts, after a bounce has moved on: they did not ask for the old turn to
// resume, and re-driving it behind their back would start work they had
// implicitly abandoned. But a silent drop is the failure this whole feature
// exists to end, so the cancellation is recorded against the turn it abandons
// rather than the row merely disappearing.
//
// It is a no-op — and silent — when nothing is owed, which is almost every
// prompt: a workspace with no interrupted turn behind it must not pay a log
// line per submit.
func (m *Manager) cancelOwedResumptions(workspace, cause string) {
	if m.cfg.PromptReceipts == nil {
		return
	}
	// EVERY UNDISCHARGED ROW, CLAIMED OR NOT. A re-drive already claimed for
	// this workspace is a turn the user has just moved on from exactly as an
	// unclaimed one is, and the claimed row is the one nothing else will ever
	// clear — leaving it would keep a record of owed work the user abandoned.
	owed, err := m.cfg.PromptReceipts.UndischargedResumptions(workspace)
	if err != nil {
		m.logf("session-controller: turn resumption PREEMPTION READ FAILED ws=%q cause=%s: %v — whether anything was owed is UNKNOWN, so nothing is cancelled and the next wire decides",
			workspace, cause, err)
		return
	}
	for _, r := range owed {
		discharged, err := m.cfg.PromptReceipts.DischargeResumption(r.RequestID)
		if err != nil {
			m.logf("session-controller: turn resumption PREEMPTION FAILED ws=%q request_id=%q turn_id=%q cause=%s: %v — the record stands, so a later wire may re-drive a turn the user has moved on from",
				workspace, r.RequestID, r.TurnID, cause, err)
			continue
		}
		m.logf("session-controller: turn resumption CANCELLED ws=%q request_id=%q turn_id=%q cause=%s state=%s discharged=%v — the user acted first, so the turn the last bounce interrupted is NOT re-driven",
			workspace, r.RequestID, r.TurnID, cause, r.State, discharged)
	}
}

// resumptionSubmitTimeout bounds one re-drive's submit.
//
// It is a FAILURE bound rather than a tuned delay: the submit returns as soon
// as the shim accepts, and this only decides how long a re-drive waits on a
// session that has stopped answering before leaving the record for the next
// wire to retry.
const resumptionSubmitTimeout = 30 * time.Second

// resumptionRequestID mints the re-drive's identity.
//
// It is derived from the workspace, the turn and the interruption instant
// rather than randomly, so two teardowns of the SAME interrupted turn — a
// bounce retried, a teardown path that runs twice — collide on one request id
// and therefore on one row, rather than accumulating one owed resumption per
// attempt. That is the exactly-once property doing its work at the point where
// the record is created rather than only at the point where it is discharged.
//
// An UNNAMEABLE turn falls back to the instant alone, which is the honest
// identity available for it: a turn this process cannot name is still one turn
// at one moment.
func resumptionRequestID(workspace, turnID string, atMs int64) string {
	if turnID == "" {
		return fmt.Sprintf("%s%s@%d", resumptionRequestIDPrefix, workspace, atMs)
	}
	return fmt.Sprintf("%s%s/%s", resumptionRequestIDPrefix, workspace, turnID)
}
