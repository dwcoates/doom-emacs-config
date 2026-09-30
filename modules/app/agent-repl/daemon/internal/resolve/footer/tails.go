package footer

import (
	"regexp"
	"strings"
	"unicode/utf8"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

// THE STREAMED TAILS: the transient `thinking` and `response` lines, raised from
// the agent's reasoning and prose as they stream.
//
// THE RESOLVER KEEPS THE TAIL OF EACH STREAMING UNIT'S TEXT, bounded, and draws
// its LAST LINE. A delta arrives many times a second and a view is published
// whole, so a line is raised ONLY WHEN THE UNIT'S LAST COMPLETE LINE CHANGES
// (footer-activity-tiers.md, "Accepted cost: push volume"): a delta that only
// extends the line in progress changes nothing, the view renders identically,
// and the topic's value dedup publishes nothing. The unit's settle raises the
// final line, which no newline ever completed. The footer topic is latest-only
// besides (resolver.go topicLocked), so a slow subscriber is never replayed a
// backlog of superseded lines.

// streamKeep is how much of a unit's text the resolver keeps: enough to hold
// any line the strip can draw, with room for the line in progress.
const streamKeep = 4096

// streamKind names which transient a unit's text raises.
type streamKind int

// The streamed kinds.
const (
	streamThinking streamKind = iota
	streamResponse
)

// streamTail is one streaming unit's bounded text and the line last raised
// from it.
type streamTail struct {
	kind streamKind
	// text is the tail of the unit's text so far, at most streamKeep bytes and
	// always cut at a rune boundary.
	text string
	// raised is the line last raised from this unit, so an unchanged line is
	// never raised again.
	raised string
	// withheld reports that the unit's withheld line has been raised.
	withheld bool
}

// streamFor answers the unit's tail, opening one when the unit has none (a
// delta can be the first frame the footer sees of a unit).
func (s *wsState) streamFor(unit string, kind streamKind) *streamTail {
	tail, ok := s.streams[unit]
	if !ok {
		tail = &streamTail{kind: kind}
		s.streams[unit] = tail
	}
	return tail
}

// append adds a delta to the tail, keeping at most streamKeep bytes from the
// end, cut at a rune boundary.
func (t *streamTail) append(delta string) {
	t.text += delta
	if len(t.text) <= streamKeep {
		return
	}
	cut := len(t.text) - streamKeep
	for cut < len(t.text) && !utf8.RuneStart(t.text[cut]) {
		cut++
	}
	t.text = t.text[cut:]
}

// applyThinking folds one reasoning frame into its unit's tail and raises the
// `thinking` line when the last complete line changed, or once per unit when
// the vendor withholds the text.
func (r *resolver) applyThinking(ws ids.WorkspaceID, s *wsState, agent, unit string, thinking *conversationv1.AgentThinking) {
	switch item := thinking.GetResult().(type) {
	case *conversationv1.AgentThinking_Start:
		s.streamFor(unit, streamThinking)
	case *conversationv1.AgentThinking_Update:
		tail := s.streamFor(unit, streamThinking)
		switch reasoning := item.Update.GetReasoning().(type) {
		case *conversationv1.AgentThinkingUpdate_Text:
			tail.append(reasoning.Text.GetNewText())
			r.raiseStreamLine(ws, s, agent, unit, tail, completeLastLine(tail.text, plainReasoning))
		case *conversationv1.AgentThinkingUpdate_Withheld:
			r.raiseWithheld(ws, s, agent, unit, tail)
		}
	case *conversationv1.AgentThinking_Success:
		tail := s.streamFor(unit, streamThinking)
		switch reasoning := item.Success.GetReasoning().(type) {
		case *conversationv1.AgentThinkingSuccess_Text:
			r.raiseStreamLine(ws, s, agent, unit, tail, lastLine(reasoning.Text.GetText(), plainReasoning))
		case *conversationv1.AgentThinkingSuccess_Withheld:
			r.raiseWithheld(ws, s, agent, unit, tail)
		}
		delete(s.streams, unit)
	default:
		delete(s.streams, unit)
	}
}

// applyResponseTail folds one prose frame into its unit's tail and raises the
// `response` line when the last complete line changed.
func (r *resolver) applyResponseTail(ws ids.WorkspaceID, s *wsState, agent, unit string, resp *conversationv1.AgentResponse) {
	switch item := resp.GetResult().(type) {
	case *conversationv1.AgentResponse_Start:
		s.streamFor(unit, streamResponse)
	case *conversationv1.AgentResponse_Update:
		tail := s.streamFor(unit, streamResponse)
		tail.append(item.Update.GetNewMarkdown())
		r.raiseStreamLine(ws, s, agent, unit, tail, completeLastLine(tail.text, plainMarkdown))
	case *conversationv1.AgentResponse_Success:
		tail := s.streamFor(unit, streamResponse)
		r.raiseStreamLine(ws, s, agent, unit, tail, lastLine(item.Success.GetProse().GetMarkdown(), plainMarkdown))
		delete(s.streams, unit)
	default:
		delete(s.streams, unit)
	}
}

// raiseStreamLine raises the unit's line when it is non-empty and differs from
// the line last raised from the unit. It is the throttle: nothing else raises a
// streamed line.
func (r *resolver) raiseStreamLine(ws ids.WorkspaceID, s *wsState, agent, unit string, tail *streamTail, line string) {
	if line == "" || line == tail.raised {
		return
	}
	tail.raised = line
	drawn := tailOf(line, DefaultWarningRowWidth)
	t := &frontendv1.FooterActivityTransient{}
	if tail.kind == streamThinking {
		t.Kind = &frontendv1.FooterActivityTransient_Thinking{Thinking: &frontendv1.FooterActivityTransientThinking{
			Reasoning: &frontendv1.FooterActivityTransientThinking_Text{
				Text: &frontendv1.FooterActivityTransientThinkingText{Tail: drawn}}}}
	} else {
		t.Kind = &frontendv1.FooterActivityTransient_Response{Response: &frontendv1.FooterActivityTransientResponse{Tail: drawn}}
	}
	r.logOf(ws, s).Debug("daemon.footer.stream_line", "a streaming unit's last line changed",
		dlog.Context{"unit": unit, "kind": transientKind(t), "chars": utf8.RuneCountInString(line)})
	r.raiseTransient(ws, s, agent, t)
}

// raiseWithheld raises the withheld `thinking` line once per unit: the vendor
// says the agent is reasoning and shows nothing, so there is no line to move.
func (r *resolver) raiseWithheld(ws ids.WorkspaceID, s *wsState, agent, unit string, tail *streamTail) {
	if tail.withheld {
		return
	}
	tail.withheld = true
	r.logOf(ws, s).Debug("daemon.footer.stream_line", "a reasoning unit's text is withheld",
		dlog.Context{"unit": unit, "kind": "thinking", "withheld": true})
	r.raiseTransient(ws, s, agent, &frontendv1.FooterActivityTransient{
		Kind: &frontendv1.FooterActivityTransient_Thinking{Thinking: &frontendv1.FooterActivityTransientThinking{
			Reasoning: &frontendv1.FooterActivityTransientThinking_Withheld{
				Withheld: &frontendv1.FooterActivityTransientThinkingWithheld{}}}},
	})
}

// completeLastLine is the last non-empty line among the COMPLETE lines of a
// streaming text — every line a newline has ended — after `plain` renders it.
// The line in progress is not drawn until it completes or the unit settles.
func completeLastLine(text string, plain func(string) string) string {
	end := strings.LastIndexByte(text, '\n')
	if end < 0 {
		return ""
	}
	return lastLine(text[:end], plain)
}

// lastLine is the last line of text that is non-empty once `plain` renders it.
func lastLine(text string, plain func(string) string) string {
	lines := strings.Split(text, "\n")
	for i := len(lines) - 1; i >= 0; i-- {
		if line := plain(lines[i]); line != "" {
			return line
		}
	}
	return ""
}

// tailOf caps a line to max runes FROM ITS END, marking the cut with a leading
// ellipsis: the newest words are the ones the reader is following.
func tailOf(line string, max int) string {
	runes := []rune(line)
	if max <= 1 || len(runes) <= max {
		return line
	}
	return "…" + string(runes[len(runes)-(max-1):])
}

// plainReasoning renders one line of reasoning text: trimmed, nothing else.
func plainReasoning(line string) string {
	return strings.TrimSpace(line)
}

// The markdown markup plainMarkdown removes.
var (
	mdFence    = regexp.MustCompile("^\\s*(```|~~~)")
	mdLead     = regexp.MustCompile(`^\s*(#{1,6}\s+|>\s*|[-*+]\s+(\[[ xX]\]\s+)?|\d+[.)]\s+)`)
	mdRule     = regexp.MustCompile(`^\s*([-*_]\s*){3,}$`)
	mdImage    = regexp.MustCompile(`!\[([^\]]*)\]\([^)]*\)`)
	mdLink     = regexp.MustCompile(`\[([^\]]*)\]\([^)]*\)`)
	mdEmphasis = regexp.MustCompile("(\\*\\*|__|~~|`)")
)

// plainMarkdown renders one line of markdown as the plain text a reader sees:
// fences and rules draw nothing, a heading, quote or list marker is dropped,
// an image or link keeps its words, and emphasis and code marks go. It is a
// LINE renderer by design: the strip draws one line, and a construct spanning
// lines (a table, a code block's body) reads as its own text.
func plainMarkdown(line string) string {
	if mdFence.MatchString(line) || mdRule.MatchString(line) {
		return ""
	}
	line = mdLead.ReplaceAllString(line, "")
	line = mdImage.ReplaceAllString(line, "$1")
	line = mdLink.ReplaceAllString(line, "$1")
	line = mdEmphasis.ReplaceAllString(line, "")
	return strings.TrimSpace(line)
}
