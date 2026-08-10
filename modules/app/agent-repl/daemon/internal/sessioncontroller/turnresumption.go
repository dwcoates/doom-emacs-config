package sessioncontroller

import (
	"fmt"

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
// as a fresh prompt that happens to look similar. The prefix is what every
// later layer keys "this submit is internal" on.
const resumptionRequestIDPrefix = "resume-after-restart:"

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
