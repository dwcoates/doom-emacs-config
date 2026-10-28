// Package codespans carries the inline code of an agent's final responses
// into the prompts a person sends next. A person typing a prompt in Emacs
// writes `foo_bar()` as plain text when the response they read showed it as
// code; Extract reads the inline code spans out of the responses, and Markup
// wraps every plain occurrence of one of them in the prompt in backticks.
package codespans

import (
	"sort"
	"strings"
	"unicode"
	"unicode/utf8"
)

// minLength is the shortest span worth marking: a one-character span would
// match inside ordinary prose.
const minLength = 2

// Extract answers the distinct inline code spans of MARKDOWN in order of first
// appearance. Fenced code blocks hold no inline span, and a span too short to
// mark unambiguously (minLength) or containing a backtick is left out.
func Extract(markdown string) []string {
	var spans []string
	seen := map[string]bool{}
	scanLines(markdown, func(line string) {
		for _, seg := range segments(line) {
			if !seg.code {
				continue
			}
			span := strings.TrimSpace(seg.inner)
			if utf8.RuneCountInString(span) < minLength || strings.Contains(span, "`") || seen[span] {
				continue
			}
			seen[span] = true
			spans = append(spans, span)
		}
	}, nil)
	return spans
}

// Markup answers TEXT with every plain occurrence of one of SPANS wrapped in
// backticks. Text already inside an inline code span or a fenced block is left
// alone, the longest span wins where two overlap, and a span that begins or
// ends with a word character matches only where the text beside it is not a
// word character, so `go` does not mark the start of `gopher`.
func Markup(text string, spans []string) string {
	if len(spans) == 0 {
		return text
	}
	ordered := append([]string(nil), spans...)
	sort.SliceStable(ordered, func(i, j int) bool { return len(ordered[i]) > len(ordered[j]) })
	var out strings.Builder
	out.Grow(len(text))
	scanLines(text, func(line string) {
		for _, seg := range segments(line) {
			if seg.code {
				out.WriteString(seg.raw)
				continue
			}
			markupPlain(&out, seg.raw, ordered)
		}
	}, func(line string) { out.WriteString(line) })
	return out.String()
}

// markupPlain writes PLAIN, text outside any code span, to OUT with the spans
// it holds wrapped.
func markupPlain(out *strings.Builder, plain string, spans []string) {
	for i := 0; i < len(plain); {
		if span := spanAt(plain, i, spans); span != "" {
			out.WriteString("`" + span + "`")
			i += len(span)
			continue
		}
		_, size := utf8.DecodeRuneInString(plain[i:])
		out.WriteString(plain[i : i+size])
		i += size
	}
}

// spanAt answers the first of SPANS standing at PLAIN[at:] with a boundary on
// each side, "" for none.
func spanAt(plain string, at int, spans []string) string {
	for _, span := range spans {
		if span == "" || strings.Contains(span, "`") || !strings.HasPrefix(plain[at:], span) {
			continue
		}
		first, _ := utf8.DecodeRuneInString(span)
		last, _ := utf8.DecodeLastRuneInString(span)
		before, _ := utf8.DecodeLastRuneInString(plain[:at])
		after, _ := utf8.DecodeRuneInString(plain[at+len(span):])
		if at > 0 && isWord(first) && isWord(before) {
			continue
		}
		if at+len(span) < len(plain) && isWord(last) && isWord(after) {
			continue
		}
		return span
	}
	return ""
}

func isWord(r rune) bool { return r == '_' || unicode.IsLetter(r) || unicode.IsDigit(r) }

// segment is one stretch of a line: plain text, or an inline code span with
// its backtick runs (raw) and the code between them (inner).
type segment struct {
	raw, inner string
	code       bool
}

// segments splits LINE into plain stretches and inline code spans. A backtick
// run with no closing run of the same length is plain text.
func segments(line string) []segment {
	var out []segment
	plainFrom := 0
	for i := 0; i < len(line); {
		if line[i] != '`' {
			i++
			continue
		}
		run := backtickRun(line, i)
		end := closingRun(line, i+run, run)
		if end < 0 {
			i += run
			continue
		}
		if i > plainFrom {
			out = append(out, segment{raw: line[plainFrom:i]})
		}
		out = append(out, segment{raw: line[i : end+run], inner: line[i+run : end], code: true})
		i = end + run
		plainFrom = i
	}
	if plainFrom < len(line) {
		out = append(out, segment{raw: line[plainFrom:]})
	}
	return out
}

func backtickRun(line string, at int) int {
	n := 0
	for at+n < len(line) && line[at+n] == '`' {
		n++
	}
	return n
}

// closingRun answers where the next backtick run of exactly RUN backticks
// begins at or after FROM, -1 for none.
func closingRun(line string, from, run int) int {
	for i := from; i < len(line); {
		if line[i] != '`' {
			i++
			continue
		}
		n := backtickRun(line, i)
		if n == run {
			return i
		}
		i += n
	}
	return -1
}

// scanLines calls PLAIN for every line of TEXT outside a fenced code block,
// and FENCED, when non-nil, for every line inside or opening or closing one.
func scanLines(text string, plain func(string), fenced func(string)) {
	var fence string
	for _, line := range strings.SplitAfter(text, "\n") {
		trimmed := strings.TrimLeft(line, " ")
		if fence == "" {
			if marker := fenceMarker(trimmed); marker != "" && len(line)-len(trimmed) <= 3 {
				fence = marker
				if fenced != nil {
					fenced(line)
				}
				continue
			}
			plain(line)
			continue
		}
		if strings.Trim(strings.TrimSpace(trimmed), fence[:1]) == "" && len(strings.TrimSpace(trimmed)) >= len(fence) {
			fence = ""
		}
		if fenced != nil {
			fenced(line)
		}
	}
}

// fenceMarker answers the run of three or more backticks or tildes TRIMMED
// opens a fence with, "" for none.
func fenceMarker(trimmed string) string {
	for _, c := range []byte{'`', '~'} {
		n := 0
		for n < len(trimmed) && trimmed[n] == c {
			n++
		}
		if n >= 3 {
			return trimmed[:n]
		}
	}
	return ""
}
