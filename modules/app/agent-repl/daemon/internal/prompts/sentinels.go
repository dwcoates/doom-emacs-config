package prompts

import (
	"fmt"
	"strings"
)

// The metaprompt sentinels, pinned by ARCHITECTURE.md "Cross-system literals
// the daemon consumes" and spelled by Emacs `agent-repl--meta-wrap`
// (lisp/core.el): an injected span is MetaOpen + text + MetaClose. The daemon
// wraps its OWN injected spans (one-shot decoration, add-support briefs, merge
// briefs) with the same markers.
const (
	MetaOpen  = "<!--agent-repl:meta-->"
	MetaClose = "<!--/agent-repl:meta-->"
)

// Wrap marks text as an injected span, so a drawn prompt can have it stripped
// back out. The record keeps the full text; only the drawn text is stripped.
func Wrap(text string) string { return MetaOpen + text + MetaClose }

// StripSentinels removes every injected span, and the whitespace it leaves,
// from DRAWN prompt text. The record keeps the full text: this is a drawing
// concern only.
//
// Whitespace: after a span is removed, the whitespace runs that now touch each
// other are collapsed into one separator — a newline if either run carried
// one, otherwise a single space — and are removed outright when the junction
// falls at the start or the end of the text. A span alone on its own line
// therefore leaves no blank line behind.
//
// An unbalanced marker is a producer bug and is surfaced, never guessed at.
func StripSentinels(text string) (string, error) {
	var out strings.Builder
	rest := text
	for {
		open := strings.Index(rest, MetaOpen)
		if open < 0 {
			break
		}
		if stray := strings.Index(rest[:open], MetaClose); stray >= 0 {
			return "", fmt.Errorf("prompts: an injected span closes at offset %d having never opened", len(text)-len(rest)+stray)
		}
		afterOpen := open + len(MetaOpen)
		relClose := strings.Index(rest[afterOpen:], MetaClose)
		if relClose < 0 {
			return "", fmt.Errorf("prompts: an injected span opens at offset %d and never closes", len(text)-len(rest)+open)
		}
		out.WriteString(rest[:open])
		rest = rest[afterOpen+relClose+len(MetaClose):]

		// Collapse the whitespace the removal brought together.
		before := out.String()
		leading, trimmedBefore := splitTrailingSpace(before)
		trailing, trimmedAfter := splitLeadingSpace(rest)
		if leading == "" && trailing == "" {
			continue
		}
		joined := leading + trailing
		separator := " "
		switch {
		case trimmedBefore == "" || trimmedAfter == "":
			// The junction is at the start or the end of the text.
			separator = ""
		case strings.ContainsAny(joined, "\n\r"):
			separator = "\n"
		}
		out.Reset()
		out.WriteString(trimmedBefore)
		out.WriteString(separator)
		rest = trimmedAfter
	}
	if stray := strings.Index(rest, MetaClose); stray >= 0 {
		return "", fmt.Errorf("prompts: an injected span closes at offset %d having never opened", len(text)-len(rest)+stray)
	}
	out.WriteString(rest)
	return out.String(), nil
}

// splitTrailingSpace answers the trailing whitespace run of s and s without it.
func splitTrailingSpace(s string) (space, rest string) {
	i := len(s)
	for i > 0 && isSpace(s[i-1]) {
		i--
	}
	return s[i:], s[:i]
}

// splitLeadingSpace answers the leading whitespace run of s and s without it.
func splitLeadingSpace(s string) (space, rest string) {
	i := 0
	for i < len(s) && isSpace(s[i]) {
		i++
	}
	return s[:i], s[i:]
}

func isSpace(b byte) bool {
	return b == ' ' || b == '\t' || b == '\n' || b == '\r' || b == '\v' || b == '\f'
}
