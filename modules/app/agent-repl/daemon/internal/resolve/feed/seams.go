package feed

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

// The five LANDING-3 shapes, each behind ONE function so the rest of the
// package never repeats a wire decision. Landing 3 is merged, so every one of
// them now sets the generated arm; the functions stay because they are the one
// place each of these facts is spelled.

// headline is FeedTurnEndedErrored.headline: the daemon-composed per-arm
// sentence that retires the client's sentence table.
type headline struct {
	// Text is the composed sentence.
	Text string
}

// applyHeadline puts the composed headline and the vendor's own wording onto
// an errored turn terminal. The headline is REQUIRED — the client holds no
// per-arm sentence table — and the vendor's sentence stays beside it rather
// than being folded in, because only one of the two is ours.
func applyHeadline(errored *frontendv1.FeedTurnEndedErrored, h headline, vendorMessage string) {
	errored.Headline = &frontendv1.FeedTurnErrorHeadline{Text: h.Text}
	if vendorMessage != "" {
		errored.Message = &frontendv1.FeedTurnErrorMessage{Text: vendorMessage}
	}
}

// returnedForm sets a returned card's output form. It is a SETTER rather than
// the generated oneof interface because that interface's method is unexported
// and unimplementable from here; a nil returnedForm is therefore the honest
// spelling of "no form", which applyReturnedForm turns into the `none` arm.
type returnedForm func(*frontendv1.FeedToolCallReturned)

// applyReturnedForm sets a returned card's output form. A nil form is the
// `none` arm: a call that returned NOTHING to draw is presence, never an empty
// text — `text{""}` would be a sentinel standing in for a state.
func applyReturnedForm(returned *frontendv1.FeedToolCallReturned, form returnedForm) {
	if form == nil {
		returned.Form = &frontendv1.FeedToolCallReturned_None{None: &frontendv1.FeedToolCallNoOutput{}}
		return
	}
	form(returned)
}

// inputForm is FeedToolCallInput.form: the daemon's statement of how the input
// line is DRAWN — a shell line, a muted path, or a query.
type inputForm int

const (
	// inputFormNone leaves the line plain: the daemon named no treatment.
	inputFormNone inputForm = iota
	// inputFormCommand is a shell line, drawn as a command.
	inputFormCommand
	// inputFormPath is a file path, drawn muted.
	inputFormPath
	// inputFormQuery is a search query, drawn as a query.
	inputFormQuery
)

// String names the form for logs.
func (f inputForm) String() string {
	switch f {
	case inputFormCommand:
		return "command"
	case inputFormPath:
		return "path"
	case inputFormQuery:
		return "query"
	}
	return "none"
}

// applyInputForm states how a tool call's input line is drawn. Only the daemon
// knows the tool; the client applies the treatment and still knows no tool.
func applyInputForm(input *frontendv1.FeedToolCallInput, form inputForm) {
	switch form {
	case inputFormCommand:
		input.Form = &frontendv1.FeedToolCallInput_Command{Command: &frontendv1.FeedToolCallInputCommand{}}
	case inputFormPath:
		input.Form = &frontendv1.FeedToolCallInput_Path{Path: &frontendv1.FeedToolCallInputPath{}}
	case inputFormQuery:
		input.Form = &frontendv1.FeedToolCallInput_Query{Query: &frontendv1.FeedToolCallInputQuery{}}
	}
}

// detachedLostCause is the DetachedLost vocabulary: the three ways work stops
// being VISIBLE, as opposed to being known to have failed.
type detachedLostCause int

const (
	// lostNone means the terminal names no lost cause: an ordinary ending.
	lostNone detachedLostCause = iota
	// lostFileVanished: the spool or transcript disappeared from disk.
	lostFileVanished
	// lostWentSilent: the run produced nothing past the reader's silence
	// ruling.
	lostWentSilent
	// lostSweptUp: a boot sweep found the run open with no living producer.
	lostSweptUp
)

// String names the cause for logs and for the composed sentence.
func (c detachedLostCause) String() string {
	switch c {
	case lostFileVanished:
		return "file_vanished"
	case lostWentSilent:
		return "went_silent"
	case lostSweptUp:
		return "swept_up"
	}
	return "none"
}

// lostCauseOf reads a DetachedLost's arm.
func lostCauseOf(lost *conversationv1.DetachedLost) detachedLostCause {
	switch lost.GetHow().(type) {
	case *conversationv1.DetachedLost_FileVanished:
		return lostFileVanished
	case *conversationv1.DetachedLost_WentSilent:
		return lostWentSilent
	case *conversationv1.DetachedLost_SweptUp:
		return lostSweptUp
	}
	return lostNone
}

// lostCauseOfSubagent reads a subagent failure's lost cause. LOST IS NOT
// FAILED: the run is not known to have failed, and the bubble's own word says
// so.
func lostCauseOfSubagent(failure *conversationv1.AgentSubagentFailure) detachedLostCause {
	lost, ok := failure.GetCause().(*conversationv1.AgentSubagentFailure_Lost)
	if !ok {
		return lostNone
	}
	return lostCauseOf(lost.Lost)
}

// lostCauseOfBash reads an interrupted shell's lost cause, on the same terms.
func lostCauseOfBash(interrupted *conversationv1.AgentBashInterrupted) detachedLostCause {
	lost, ok := interrupted.GetCause().(*conversationv1.AgentBashInterrupted_Lost)
	if !ok {
		return lostNone
	}
	return lostCauseOf(lost.Lost)
}

// lostCauseOfAgentFailure reads an agent terminal's lost cause, on the same
// terms.
func lostCauseOfAgentFailure(failure *conversationv1.AgentFailure) detachedLostCause {
	lost, ok := failure.GetFailure().(*conversationv1.AgentFailure_Lost)
	if !ok {
		return lostNone
	}
	return lostCauseOf(lost.Lost)
}

// lostSentence words a lost ending for a headline. WE STOPPED BEING ABLE TO
// SEE IT is the whole claim; nothing here says the work failed.
func lostSentence(cause detachedLostCause) string {
	switch cause {
	case lostFileVanished:
		return "we lost sight of this work — its transcript disappeared from disk"
	case lostWentSilent:
		return "we lost sight of this work — it went silent past the reader's ruling"
	case lostSweptUp:
		return "we lost sight of this work — a boot sweep found it open with no living producer"
	}
	return "we lost sight of this work"
}

// applySubagentLostHow relays a DetachedLost arm by name onto a subagent's
// lost row. It reports false when the cause names no arm this build carries:
// AN UNLANDED ARM IS NEVER SILENTLY DEFAULTED — the caller says so in the log
// and the row goes out with no `how` rather than with a wrong one.
func applySubagentLostHow(lost *frontendv1.FeedSubagentLost, cause detachedLostCause) bool {
	switch cause {
	case lostFileVanished:
		lost.How = &frontendv1.FeedSubagentLost_FileVanished{FileVanished: &frontendv1.FeedSubagentLostFileVanished{}}
	case lostWentSilent:
		lost.How = &frontendv1.FeedSubagentLost_WentSilent{WentSilent: &frontendv1.FeedSubagentLostWentSilent{}}
	case lostSweptUp:
		lost.How = &frontendv1.FeedSubagentLost_SweptUp{SweptUp: &frontendv1.FeedSubagentLostSweptUp{}}
	default:
		return false
	}
	return true
}

// applyShellLostHow relays a DetachedLost arm by name onto a shell's lost row,
// on the same terms as applySubagentLostHow.
func applyShellLostHow(lost *frontendv1.FeedShellLost, cause detachedLostCause) bool {
	switch cause {
	case lostFileVanished:
		lost.How = &frontendv1.FeedShellLost_FileVanished{FileVanished: &frontendv1.FeedShellLostFileVanished{}}
	case lostWentSilent:
		lost.How = &frontendv1.FeedShellLost_WentSilent{WentSilent: &frontendv1.FeedShellLostWentSilent{}}
	case lostSweptUp:
		lost.How = &frontendv1.FeedShellLost_SweptUp{SweptUp: &frontendv1.FeedShellLostSweptUp{}}
	default:
		return false
	}
	return true
}
