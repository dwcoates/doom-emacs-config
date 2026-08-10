package sessioncontroller

import (
	"fmt"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/errclass"
)

// uninterruptibleturn.go — THE TURNS A QUEUED PROMPT NEVER INTERRUPTS.
//
// A CONTEXT CUT is `/compact` or `/clear`: the two session commands whose whole
// purpose is to change what the conversation carries. Everything else the queue
// holds a prompt behind is ordinary work, and asking whether the user's next
// prompt should cut in front of it is a real question. For a cut it is not:
//
//   - A compaction READS THE WHOLE CONVERSATION and writes a summary at the
//     end. Interrupting it pays for the read and lands nothing, so the context
//     is exactly as large as it was and the money is gone.
//   - A clear DISCARDS the conversation and is over in an instant. There is
//     nothing to save by interrupting it and nothing gained by trying.
//
// So no classifier runs on a prompt queued behind one, and no force sends an
// interrupt at one. The prompt is DELAYED, never dropped: it is delivered by
// the ordinary turn-end drain, in the order it was typed, the moment the cut's
// turn ends.
//
// IT IS A VERDICT, NOT A `hold` ARM. Every hold on QueueEntry names something
// OTHER than the running turn keeping a prompt back — a drain lease, a
// keep-alive, a revival, a build refresh — and `queueEntry.held()` is what
// stops every delivery path from submitting one. This entry is held by the
// running turn like any ordinary queued prompt, and the drain is exactly how it
// gets delivered; what it lacks is a classifier behind it. Making it a hold
// would have to exempt itself from the one predicate holds exist for.
//
// THE DECISION IS TAKEN TWICE, ON PURPOSE. Once at submit, so no model call is
// spent; and again where an INTERJECT would act, because a cut can START while
// a classification is in flight — a warm compaction or a revival's `/compact`
// is submitted at a turn boundary, and the classifier's answer can land after
// it. Checking only at submit would leave a verdict about a finished turn free
// to interrupt the compaction that replaced it.

// uninterruptibleRunningCommand reports the CONTEXT CUT the session is running
// right now, or UNSPECIFIED when the running turn is anything else (including
// when no turn is running at all). Caller holds m.mu.
//
// It reads the running prompt's TEXT through the same recognizer the bubble
// suppression uses (sessioncommand.go), so a cut submitted by the user, by a
// warm compaction, and by a revival are one case rather than three: each of
// them is `/compact` or `/clear` reaching the shim as prompt text, and the
// daemon records what it forwarded in d.runningText.
//
// AN UNKNOWN RUNNING TURN IS NOT A CUT. d.runningText is empty when the turn
// predates this daemon, and answering UNSPECIFIED there is the honest reading:
// the entry is classified as any other prompt behind an unknown turn is.
func uninterruptibleRunningCommand(d *sessionController) (frontendv1.SessionCommand, bool) {
	if !d.turn.active() {
		return frontendv1.SessionCommand_SESSION_COMMAND_UNSPECIFIED, false
	}
	command, _ := lookupSessionCommand(d.runningText)
	switch command {
	case frontendv1.SessionCommand_SESSION_COMMAND_COMPACT,
		frontendv1.SessionCommand_SESSION_COMMAND_CLEAR:
		return command, true
	default:
		return frontendv1.SessionCommand_SESSION_COMMAND_UNSPECIFIED, false
	}
}

// stampUninterruptible records on e that the cut named by command is what
// decided its delivery, and that nothing classified it.
//
// ONE FUNCTION FOR ALL THREE STAMPING SITES — the submit, the discarded verdict
// and the refused interject — because the verdict, the command and the
// rationale are ONE fact in three fields, and a site that set two of the three
// would produce an entry whose arm and whose prose disagree.
func stampUninterruptible(e *queueEntry, command frontendv1.SessionCommand) {
	e.classification = VerdictUninterruptibleTurn
	e.uninterruptibleCommand = command
	e.rationale = uninterruptibleRationale(command)
}

// uninterruptibleRationale is the entry's free-text account of why nothing
// classified it. The wire carries the command on its own arm; this is what the
// log line and any text-only surface read.
func uninterruptibleRationale(command frontendv1.SessionCommand) string {
	return fmt.Sprintf("queued behind %s, which is never interrupted; delivered when it ends",
		sessionCommandLiteral(command))
}

// sessionCommandLiteral spells a session command as the user types it, off the
// schema's own table (protocmd) rather than a second copy of the spelling.
//
// A command with no spec in the table renders as its enum name. That is not a
// fallback for a missing literal: every command that can reach here has one,
// and a name is what makes an impossible value visible in the line instead of
// rendering as an empty string that reads like a bug in the sentence.
func sessionCommandLiteral(command frontendv1.SessionCommand) string {
	for _, spec := range sessionCommandSpecs {
		if spec.command == command {
			return spec.Literal
		}
	}
	return command.String()
}

// refuseUninterruptibleForce is the loud nack for a force aimed at a prompt
// queued behind a context cut.
//
// It is shaped exactly like the keep-alive and revival refusals, and its
// argument is theirs: a force's MECHANISM is an interrupt, and the interrupt is
// the one thing that must not happen to a cut. Refusing costs the user the rest
// of a turn that is already running; allowing it costs the whole-conversation
// read the compaction was paying for and leaves the context uncompacted anyway.
//
// The user is not left without a way to stop a cut — the interrupt command
// itself still stops it, and says so. What is refused is interrupting one AS
// THE SIDE EFFECT of asking for a prompt to run sooner.
func (m *Manager) refuseUninterruptibleForce(workspace, entryID, sessionID string, command frontendv1.SessionCommand) error {
	literal := sessionCommandLiteral(command)
	m.logf("session-controller: force REFUSED for a queue entry behind a context cut entry=%s ws=%q session=%s command=%s — a force's mechanism is an interrupt, and %s is never interrupted for a queued prompt: stopping a compaction pays for the whole-conversation read and lands no summary. The prompt is delivered on its own when the cut's turn ends, and is still cancellable",
		entryID, workspace, sessionID, command.String(), literal)
	return fmt.Errorf("session-controller: cannot force queued prompt %q on workspace %q: it is waiting behind %s, which is never interrupted; the prompt is delivered automatically when that turn ends, and can be cancelled: %w",
		entryID, workspace, literal, errclass.ErrQueueEntryUninterruptibleTurn)
}
