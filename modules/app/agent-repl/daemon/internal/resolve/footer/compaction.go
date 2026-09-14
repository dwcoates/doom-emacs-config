package footer

import (
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"
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

// tokenFigure scales a token count the way every other figure on this strip is
// scaled: thousands as `k` with one decimal, smaller counts whole.
func tokenFigure(n uint64) string {
	if n < 1000 {
		return fmt.Sprintf("%d", n)
	}
	return trimZero(fmt.Sprintf("%.1f", float64(n)/1000)) + "k"
}
