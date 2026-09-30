package footer

import (
	"fmt"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

// The compaction's progress lines, composed HERE and nowhere else.
//
// WHY THE DAEMON WORDS THEM. Every surface draws the footer's activity text
// verbatim, so a compaction that read one way in the strip and another in the
// gate card would be two accounts of one act. The producers state PHASES (the
// shim's `SessionUpdate.compaction_progress`) and figures; the sentence is
// this file's.
//
// GROUNDED (owner's report, 2026-09-14): the cold gate's compaction ran from
// 11:14:18 to 11:14:19 with the daemon writing one record at the very end and
// nothing on the footer at all, so the owner clicked and then watched a card
// that had gone inert. A line per phase is what that report asked for.

// vendorCompactionLine is the whole of what a VENDOR-initiated auto-compaction
// states about itself: presence. `SessionUpdate.compacting` carries no phase
// and no figure, and this line says nothing it does not know.
const vendorCompactionLine = "compacting the context…"

// CompactionLine composes one phase's line. It is exported because the verb
// that spends a cold gate's answer relays the same phases onto the same cell
// and must not word them a second way.
func CompactionLine(p *conversationv1.SessionCompactionProgress) string {
	switch p.GetPhase() {
	case conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_SUMMARIZING:
		if before := p.GetTokensBefore(); before > 0 {
			return fmt.Sprintf("summarizing the conversation (%s)…", tokenFigure(before))
		}
		return "summarizing the conversation…"
	case conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_SUMMARIZED:
		return "the summary is written; the context is cut"
	case conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_RESUMING:
		return "resuming the session from the summary…"
	case conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_STARTED:
		before, after := p.GetTokensBefore(), p.GetTokensAfter()
		if before > 0 && after > 0 {
			return fmt.Sprintf("compacted and resumed (%s → %s)", tokenFigure(before), tokenFigure(after))
		}
		return "compacted and resumed"
	case conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_FAILED:
		if why := p.GetError(); why != "" {
			return "compaction failed — " + truncate(why, DefaultWarningRowWidth)
		}
		return "compaction failed"
	default:
		// AN UNSET PHASE IS STILL A COMPACTION SAYING SOMETHING. The producer
		// sent a frame, so the act is happening; drawing nothing for it would
		// be the silence this whole file exists to end.
		return vendorCompactionLine
	}
}

// CompactionRequestLine is what the footer says the INSTANT a gate's answer is
// taken, before the daemon has dialed the shim at all. The owner's ruling of
// 2026-09-14 is that the request itself is a footer act: the click is
// acknowledged, then the phases refine it.
func CompactionRequestLine(choice, detail string) string {
	switch choice {
	case ChoiceCompact:
		if detail != "" {
			return "compaction requested (" + detail + ")"
		}
		return "compaction requested"
	case ChoiceClear:
		return "clearing the context and resuming the session…"
	case ChoicePay:
		return "resuming the session and paying for the cold read…"
	default:
		return "answering the cold gate…"
	}
}

// ---- the line's lifetime ---------------------------------------------------
//
// A COMPACTION LINE CANNOT OUTLIVE THE ACT IT NARRATES (owner's report,
// 2026-09-23: "compacting the context…" stood on every later turn for over an
// hour after the compaction had finished, because the one signal that ended
// it — the context cut — never reached the footer). The line is therefore
// ended by EVERY edge that ends its act, not by one:
//
//   - the CUT (OnContextCut), the compaction's own end signal;
//   - the main TURN'S TERMINAL (OnAgentTerminal). A vendor auto-compaction
//     runs inside a turn and a `/compact` is its own turn, so no compaction
//     the vendor narrates survives the turn it ran in. Reaching the terminal
//     with the line still standing means the cut was MISSED upstream, which is
//     an invariant violation: it is recorded at ERROR with the line and its
//     age, and the line is ended, because the terminal IS the end of the act —
//     the record exposes the missing cut, the clearing is not a fallback;
//   - a CONCLUDED PHASE (`started`, `failed`): the act is over, so its line
//     ends at once and the conclusion is announced as a TRANSIENT —
//     `compaction_concluded` ("compacted and resumed (…)"), or `context_budget`
//     for a failure that left the context as large as it was. NO TIMER ends
//     the salient line (owner ruling, 2026-09-28: a timer may end only a
//     transient);
//   - the vendor query DYING: no turn survives it, so no compaction does;
//   - the COLD GATE'S ANSWER being cleared. While an answer is in flight the
//     line is the answer's own (owner ruling, 2026-09-14): the answer's verb
//     clears it on every way out, and no other edge takes it from under the
//     answer, so the gate's "compacted and resumed" reads until the verb is
//     done, exactly as ruled.
//
// A TURN OPENING WITH A LINE STANDING IS NOT A VIOLATION. The vendor's
// `compacting` start signal lands BEFORE the turn-open edge of the turn it
// belongs to (see applyTurnStarted), so a line standing at an opening is, in
// the ordinary case, this turn's own. The turn's terminal bounds it either way.

// standCompaction stands the compaction line. A repeat of the words already
// standing keeps the instant they began standing: the vendor re-sends its
// `compacting` signal about every thirty seconds while it compacts, and the
// strip's age — and a violation record's — is the age of the ACT, not of the
// latest re-send.
func (r *resolver) standCompaction(ws ids.WorkspaceID, s *wsState, text, cause string) {
	if s.compaction != nil && s.compaction.text == text {
		return
	}
	s.compaction = &standing{text: text, at: r.opts.clock.Now()}
	r.logOf(ws, s).Debug("daemon.footer.compaction_line_stood", "the footer stood a compaction line",
		dlog.Context{"text": text, "cause": cause})
}

// endCompaction ends the standing compaction line, if any, and records which
// edge ended it.
func (r *resolver) endCompaction(ws ids.WorkspaceID, s *wsState, cause string) {
	if s.compaction == nil {
		return
	}
	r.logOf(ws, s).Debug("daemon.footer.compaction_line_ended", "the footer ended a compaction line",
		dlog.Context{"text": s.compaction.text, "age_ms": r.compactionAge(s).Milliseconds(), "cause": cause})
	s.compaction = nil
}

// concludeCompaction ends a compaction at its CONCLUDED phase: the salient
// line goes, and the outcome is announced as a transient — the composed
// "compacted and resumed (…)" line as `compaction_concluded`, a failure as
// `context_budget`, because the context is still as large as it was. While a
// cold gate's answer is in flight the salient line is the ANSWER's, which its
// verb ends; the transient is raised beneath it all the same.
func (r *resolver) concludeCompaction(ws ids.WorkspaceID, s *wsState, progress *conversationv1.SessionCompactionProgress) {
	const cause = "daemon.footer.on_session_update.compaction_progress"
	if s.coldAnswer == nil {
		r.endCompaction(ws, s, cause)
	}
	line := CompactionLine(progress)
	if progress.GetPhase() == conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_FAILED {
		r.raiseContextBudget(ws, s, "", line)
		return
	}
	r.raiseTransient(ws, s, "", &frontendv1.FooterActivityTransient{
		Kind: &frontendv1.FooterActivityTransient_CompactionConcluded{
			CompactionConcluded: &frontendv1.FooterActivityTransientCompactionConcluded{Text: line}},
	})
}

// endCompactionAtTerminal ends the line at the main turn's terminal, recording
// the invariant violation when the line narrates an act that should already
// have been ended by its cut.
func (r *resolver) endCompactionAtTerminal(ws ids.WorkspaceID, s *wsState, turn ids.TurnID) {
	const cause = "daemon.footer.on_agent_terminal"
	switch {
	case s.compaction == nil:
		return
	case s.coldAnswer != nil:
		// The answer owns the line; its verb ends it.
		return
	}
	r.logOf(ws, s).Error("daemon.footer.compaction_line_outlived_turn",
		"the turn ended with a compaction line still standing: the context cut that ends the compaction never reached the footer",
		dlog.Context{
			"turn_id":             string(turn),
			"text":                s.compaction.text,
			"age_ms":              r.compactionAge(s).Milliseconds(),
			"stood_at":            s.compaction.at.UTC().Format(time.RFC3339Nano),
			"invariant_violation": "a compaction line outlived the turn it ran in",
			"remediation":         "find where the compaction's context_cut was held or dropped before the footer",
		})
	r.endCompaction(ws, s, cause)
}

// compactionAge is how long the standing line has stood.
func (r *resolver) compactionAge(s *wsState) time.Duration {
	return r.opts.clock.Now().Sub(s.compaction.at)
}

// concludedPhase reports whether a phase ends the compaction.
func concludedPhase(phase conversationv1.SessionCompactionPhase) bool {
	return phase == conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_STARTED ||
		phase == conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_FAILED
}

// tokenFigure scales a token count the way every other figure on this strip is
// scaled: thousands as `k` with one decimal, smaller counts whole.
func tokenFigure(n uint64) string {
	if n < 1000 {
		return fmt.Sprintf("%d", n)
	}
	return trimZero(fmt.Sprintf("%.1f", float64(n)/1000)) + "k"
}
