package footer

import (
	"fmt"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/topbar"
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

// compactionText is one compaction line as the footer draws it: the sentence,
// and its parts when it names a figure that takes the cold gate's gradient.
type compactionText struct {
	text  string
	parts *FiguredLine
}

// plainCompaction is a compaction line that names no colored figure.
func plainCompaction(text string) compactionText {
	return compactionText{text: text}
}

// compactionLine words one relayed phase, for both of the routes a phase
// reaches the footer by: the session stream and the cold gate's answer. A
// phase that states what its summary cost names the summarizer and its spend,
// the spend measured against the window the standing gate's own figure was
// (ColdGate.Window), so it takes the gate's gradient.
//
// A SPEND WITH NO GATE STANDING IS AN INVARIANT VIOLATION. Only the cold
// gate's remediation runs a summarizer, and the gate is lifted only after its
// re-open lands, so every spend arrives while the gate's window is known. A
// spend measured against no window would be a color invented here; it is
// recorded at ERROR and the resolver fails hard instead.
func (r *resolver) compactionLine(ws ids.WorkspaceID, s *wsState, p *conversationv1.SessionCompactionProgress) compactionText {
	sentence := compactionSentence(p)
	spend := p.GetSpend()
	if spend == nil {
		return plainCompaction(sentence)
	}
	if !s.coldGate.Standing || s.coldGate.Window <= 0 {
		r.logOf(ws, s).Error("daemon.footer.compaction_spend_without_gate",
			"a compaction stated its summarizer's spend with no cold gate standing to measure it against",
			dlog.Context{
				"phase":               p.GetPhase().String(),
				"summarizer_model":    spend.GetModel().GetName(),
				"summarizer_tokens":   spend.GetTokens(),
				"gate_standing":       s.coldGate.Standing,
				"gate_window":         s.coldGate.Window,
				"invariant_violation": "a summarizer's spend arrives only while its cold gate stands",
				"remediation":         "find what lifted the gate, or zeroed its window, before the compaction concluded",
			})
		panic(fmt.Sprintf("footer: compaction spend on %q with no cold gate window (standing=%t, window=%d)",
			ws, s.coldGate.Standing, s.coldGate.Window))
	}
	parts := compactionSpendLine(sentence, spend, s.coldGate.Window)
	return compactionText{text: parts.Text(), parts: &parts}
}

// compactionSpendLine is a phase's sentence followed by what its summary
// cost: "compacted and resumed (101.6k → 12.4k) · claude-sonnet-5 spent
// 104.3k tokens", the spend as the colored figure.
func compactionSpendLine(sentence string, spend *conversationv1.SessionCompactionSpend, window int64) FiguredLine {
	tokens := spend.GetTokens()
	return FiguredLine{
		Lead:       sentence + " · " + spend.GetModel().GetName() + " spent ",
		Figure:     tokenFigure(tokens),
		Tail:       " tokens",
		WindowFill: topbar.WindowFill(int64(tokens), window),
	}
}

// compactionSentence composes one phase's sentence, before any spend.
func compactionSentence(p *conversationv1.SessionCompactionProgress) string {
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
	case ChoiceCompact, ChoiceCompactAndResume:
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
//     `compaction_concluded` ("compacted and resumed (…)"); a failure raises
//     no footer line, the feed's outcome marker is its whole account (owner
//     ruling, 2026-10-06). NO TIMER ends
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
func (r *resolver) standCompaction(ws ids.WorkspaceID, s *wsState, line compactionText, cause string) {
	if s.compaction != nil && s.compaction.text == line.text {
		return
	}
	s.compaction = &standing{text: line.text, parts: line.parts, at: r.opts.clock.Now()}
	r.logOf(ws, s).Debug("daemon.footer.compaction_line_stood", "the footer stood a compaction line",
		dlog.Context{"text": line.text, "cause": cause})
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

// concludeCompaction ends a compaction at its CONCLUDED phase: the running
// compaction's salient line goes. A compaction that SUCCEEDED is announced as
// the `compaction_concluded` transient ("compacted and resumed (…)"); a
// failure raises no footer line at all (owner ruling, 2026-10-06): the feed's
// outcome marker is its whole account. While a cold gate's answer is in
// flight the running line is the ANSWER's, which its verb ends; the outcome
// is recorded beneath it all the same.
func (r *resolver) concludeCompaction(ws ids.WorkspaceID, s *wsState, progress *conversationv1.SessionCompactionProgress) {
	const cause = "daemon.footer.on_session_update.compaction_progress"
	if s.coldAnswer == nil {
		r.endCompaction(ws, s, cause)
	}
	if progress.GetPhase() == conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_FAILED {
		r.logOf(ws, s).Debug("daemon.footer.compaction_failed_no_line",
			"a compaction failed; the footer raises no line, the feed's outcome marker carries it",
			dlog.Context{"text": compactionSentence(progress), "cause": cause})
		return
	}
	line := r.compactionLine(ws, s, progress)
	r.raiseTransient(ws, s, "", &frontendv1.FooterActivityTransient{
		Kind: &frontendv1.FooterActivityTransient_CompactionConcluded{
			CompactionConcluded: &frontendv1.FooterActivityTransientCompactionConcluded{
				Text: line.text, Parts: line.parts.wire()}},
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

// maxConcludedCompactions bounds the concluded-compaction memory. A start
// signal outrun by its cut lands within one stream hop of it, so only the
// latest few can ever be stale.
const maxConcludedCompactions = 8

// concludeCompactionID records that a cut ended the compaction named ID.
func concludeCompactionID(s *wsState, id string) {
	if id == "" {
		return
	}
	s.concludedCompactions = append(s.concludedCompactions, id)
	if n := len(s.concludedCompactions); n > maxConcludedCompactions {
		s.concludedCompactions = s.concludedCompactions[n-maxConcludedCompactions:]
	}
}

// compactionConcluded reports whether a cut already ended the compaction
// named ID: a start signal naming it arrived after its own cut.
func compactionConcluded(s *wsState, id string) bool {
	if id == "" {
		return false
	}
	for _, done := range s.concludedCompactions {
		if done == id {
			return true
		}
	}
	return false
}

// cutCompactionID is the identity of the compaction a cut ends, or "" when
// the cut names none (a clear, or a compaction no start signal preceded).
func cutCompactionID(cut *conversationv1.ContextCut) string {
	switch c := cut.GetCut().(type) {
	case *conversationv1.ContextCut_Compacted:
		return c.Compacted.GetCompaction().GetValue()
	case *conversationv1.ContextCut_CompactionFailed:
		return c.CompactionFailed.GetCompaction().GetValue()
	}
	return ""
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
