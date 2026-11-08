// Package namingdigest composes what a fork's naming call reads of the
// conversation it continues: the most recent requests, each bounded, so one
// conversation cannot grow a call that names a workspace in three words
// without limit.
package namingdigest

import (
	"strings"
	"unicode/utf8"

	"agent-repld/internal/runecap"
)

// The digest's bounds.
const (
	// MaxPrompts caps how many of the most recent requests the digest quotes.
	// The most recent requests are the ones a name should reflect, so a
	// caller reads no more than this many.
	MaxPrompts = 40
	// MaxPromptRunes caps one request's length, so a single pasted wall of
	// text cannot dominate the call.
	MaxPromptRunes = 1000
	// MaxDigestTotalRunes caps the WHOLE rendered digest, on top of the
	// per-request caps: those alone still let the pieces sum to 40,000 runes,
	// more than a call that names a workspace needs to spend. When the
	// rendered digest is over this cap the OLDEST request is dropped first,
	// one at a time, so the newest requests are always what survives.
	MaxDigestTotalRunes = 8000
)

// Compose renders the requests, oldest first, as the digest a fork's naming
// call reads.
//
// THE DIGEST IS QUOTED EVIDENCE, NEVER A TEMPLATE. prompts.Prompt.Splice
// refuses any `{{...}}` token surviving in its output — its guard against a
// malformed brief — and a conversation that merely TALKS about templates (this
// repository's own briefs spell `{{prompt}}`) was refused wholesale. So a
// "{{" or "}}" the user typed is opened to "{ {" / "} }" here, where the text
// becomes a splice value, and the brief guard stays whole.
func Compose(requests []string) string {
	recent := requests
	if len(recent) > MaxPrompts {
		recent = recent[len(recent)-MaxPrompts:]
	}
	trimmed := make([]string, len(recent))
	for i, p := range recent {
		trimmed[i] = runecap.Ellipsis(strings.TrimSpace(p), MaxPromptRunes)
	}
	for len(trimmed) > 0 && utf8.RuneCountInString(render(trimmed)) > MaxDigestTotalRunes {
		trimmed = trimmed[1:]
	}
	return quoteTemplateBraces(render(trimmed))
}

// render lays the requests out under one header, one per line.
func render(requests []string) string {
	var b strings.Builder
	b.WriteString("The user's requests:\n")
	for _, p := range requests {
		b.WriteString("- ")
		b.WriteString(p)
		b.WriteString("\n")
	}
	return b.String()
}

// templateBraces opens every template-token delimiter a user typed.
var templateBraces = strings.NewReplacer("{{", "{ {", "}}", "} }")

// quoteTemplateBraces opens the template delimiters in s until no substring of
// it reads as a `{{...}}` token. A run of three braces needs a second pass,
// because one replacement leaves a delimiter where the run's tail meets it.
func quoteTemplateBraces(s string) string {
	for {
		opened := templateBraces.Replace(s)
		if opened == s {
			return s
		}
		s = opened
	}
}
