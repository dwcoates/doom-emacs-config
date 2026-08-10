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
// item, not a prompt echo, and not a visible turn origin. The invisibility is
// enforced where the text is STORED rather than at each surface — a pending
// resumption is a prompt_receipt row that the receipt query cannot return
// (statedb/promptreceipt.go) — so a live push, a connect snapshot, a resync
// replay and a store re-pull are all incapable of serving it, and no client
// filter has to be trusted or kept in step.
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

// driveOneResumption submits one owed re-drive and discharges it on acceptance.
//
// THE DISCHARGE IS ON ACCEPTANCE, NOT ON ISSUE. A re-drive the shim refused is
// still owed: discharging it when the submit was merely attempted would lose
// exactly the work in the case where the successor daemon is itself unhealthy,
// which is the case this whole path exists for.
func (m *Manager) driveOneResumption(workspace, sessionID string, r statedb.PendingResumption) {
	ctx, cancel := context.WithTimeout(m.rootCtx, resumptionSubmitTimeout)
	defer cancel()
	_, err := m.submitPromptAs(
		ctx, workspace, r.RequestID, r.Text, "",
		"turn-resumption", corev1.PromptOrigin_PROMPT_ORIGIN_RESUME_AFTER_RESTART,
		submitterTurnResumption, leavesParkedPermissions,
	)
	if err != nil {
		m.logf("session-controller: turn resumption SUBMIT FAILED ws=%q session=%s request_id=%q turn_id=%q: %v — the record STANDS, so the next wire tries again rather than the turn being lost",
			workspace, sessionID, r.RequestID, r.TurnID, err)
		return
	}
	discharged, err := m.cfg.PromptReceipts.DischargeResumption(r.RequestID)
	if err != nil {
		// The submit LANDED and the row did not clear. Saying so matters more
		// than usual: the next wire will re-drive the same turn, which is a
		// duplicate rather than a loss, and a duplicate nobody was told about
		// is indistinguishable from the model deciding to repeat itself.
		m.logf("session-controller: turn resumption DISCHARGE FAILED ws=%q session=%s request_id=%q: %v — the re-drive was ACCEPTED but its record stands, so a later wire may re-drive the same turn",
			workspace, sessionID, r.RequestID, err)
		return
	}
	m.logf("session-controller: turn resumption RE-DRIVEN ws=%q session=%s request_id=%q turn_id=%q discharged=%v interrupted_at_ms=%d — the work the bounce interrupted continues, with no prompt shown to the user",
		workspace, sessionID, r.RequestID, r.TurnID, discharged, r.InterruptedAtMs)
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
	owed, err := m.cfg.PromptReceipts.PendingResumptions(workspace)
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
		m.logf("session-controller: turn resumption CANCELLED ws=%q request_id=%q turn_id=%q cause=%s discharged=%v — the user acted first, so the turn the last bounce interrupted is NOT re-driven",
			workspace, r.RequestID, r.TurnID, cause, discharged)
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
