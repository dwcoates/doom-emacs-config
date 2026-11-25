// Package codespans carries the inline code of an agent's final responses
// into the prompts a person sends next, and keeps inline code out of every
// drawn link. A person typing a prompt in Emacs writes `foo_bar()` as plain
// text when the response they read showed it as code; Extract reads the
// inline code spans out of the responses, and Markup wraps every plain
// occurrence of one of them in the prompt in backticks. Unmark draws every
// link of a text, a prompt's or a response's, with no code marker in it.
//
// Both read a text's inline structure with mdinline, the reading the webapp's
// renderer makes, so neither can mistake a link for prose.
package codespans

import (
	"sort"
	"strings"
	"unicode"
	"unicode/utf8"

	"agent-repld/internal/mdinline"
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
	doc := mdinline.Read(markdown)
	for _, line := range doc.Lines {
		if line.Kind != mdinline.Prose {
			continue
		}
		for _, s := range doc.Spans(line.Text) {
			if s.Kind != mdinline.Code {
				continue
			}
			span := strings.TrimSpace(s.Inner)
			if utf8.RuneCountInString(span) < minLength || strings.Contains(span, "`") || seen[span] {
				continue
			}
			seen[span] = true
			spans = append(spans, span)
		}
	}
	return spans
}

// Markup answers TEXT with every plain occurrence of one of SPANS wrapped in
// backticks. Only prose is marked (mdinline.Text): never a fenced block, a
// code span, an escape, or any part of a link (its text, its destination, a
// bare URL, an autolink, a reference definition), since a code span inside a
// link breaks it. The longest span wins where two overlap, and a span that
// begins or ends with a word character matches only where the text beside it
// is not a word character, so `go` does not mark the start of `gopher`.
//
// Regression (2026-11-19): a prompt's bare URL was marked inside, drawing
// https://github.com/`ChessCom`/x with the backticks in its href. Links are
// now read by mdinline, never scanned as prose.
func Markup(text string, spans []string) string {
	if len(spans) == 0 {
		return text
	}
	ordered := append([]string(nil), spans...)
	sort.SliceStable(ordered, func(i, j int) bool { return len(ordered[i]) > len(ordered[j]) })
	return mdinline.RewriteProse(text, false, func(out *strings.Builder, s mdinline.Stretch) {
		markupPlain(out, s.Line, s.From, s.To, ordered)
	})
}

// markupPlain writes LINE[FROM:TO], prose, to OUT with the spans it holds
// wrapped. A span's boundaries are judged against the whole line.
func markupPlain(out *strings.Builder, line string, from, to int, spans []string) {
	for i := from; i < to; {
		if span := spanAt(line, i, to, spans); span != "" {
			out.WriteString("`" + span + "`")
			i += len(span)
			continue
		}
		_, size := utf8.DecodeRuneInString(line[i:])
		out.WriteString(line[i : i+size])
		i += size
	}
}

// spanAt answers the first of SPANS standing at LINE[at:], ending by TO, with
// a boundary on each side, "" for none.
func spanAt(line string, at, to int, spans []string) string {
	for _, span := range spans {
		if span == "" || strings.Contains(span, "`") || !strings.HasPrefix(line[at:to], span) {
			continue
		}
		first, _ := utf8.DecodeRuneInString(span)
		last, _ := utf8.DecodeLastRuneInString(span)
		before, _ := utf8.DecodeLastRuneInString(line[:at])
		after, _ := utf8.DecodeRuneInString(line[at+len(span):])
		// A backslash before the span would escape the backtick that opens it.
		if at > 0 && (line[at-1] == '\\' || isWord(first) && isWord(before)) {
			continue
		}
		if at+len(span) < len(line) && isWord(last) && isWord(after) {
			continue
		}
		return span
	}
	return ""
}

// Unmark answers MARKDOWN with no inline code marker in any link: a link's
// text draws its code spans as plain text, and every backtick is taken out of
// a link's destination, a bare URL, an autolink, and a reference definition's
// destination. So [`foo`](https://x/`y`) draws as [foo](https://x/y) and a
// bare https://x/`y` as https://x/y. A code span holding nothing but one
// `http(s)://` URL (mdinline.SchemeURL) draws as that URL's autolink, so
// `https://x/y` draws as <https://x/y>, a link the reader clicks rather than
// code they copy. Everything else outside a link is left as written.
func Unmark(markdown string) string {
	if !strings.Contains(markdown, "`") {
		return markdown
	}
	var out strings.Builder
	out.Grow(len(markdown))
	doc := mdinline.Read(markdown)
	for _, line := range doc.Lines {
		switch line.Kind {
		case mdinline.Prose:
			for _, s := range doc.Spans(line.Text) {
				if url, ok := codeURL(s); ok {
					out.WriteString("<" + url + ">")
					continue
				}
				unmarkSpan(&out, doc, s)
			}
		case mdinline.Definition:
			unmarkSpan(&out, doc, *line.Def)
		default:
			out.WriteString(line.Text)
		}
	}
	return out.String()
}

// codeURL answers the URL S holds when S is a code span holding nothing but
// one `http(s)://` URL, its surrounding space trimmed.
func codeURL(s mdinline.Span) (string, bool) {
	if s.Kind != mdinline.Code {
		return "", false
	}
	url := strings.TrimSpace(s.Inner)
	return url, mdinline.SchemeURL(url)
}

// unmarkSpan writes S to OUT, without code markers when it is a link.
func unmarkSpan(out *strings.Builder, doc mdinline.Doc, s mdinline.Span) {
	if !s.IsLink() {
		out.WriteString(s.Raw)
		return
	}
	out.WriteString(s.Open)
	for _, inner := range doc.Spans(s.Label) {
		switch {
		case inner.Kind == mdinline.Code:
			out.WriteString(plainCode(inner.Inner))
		case inner.IsLink():
			unmarkSpan(out, doc, inner)
		default:
			out.WriteString(inner.Raw)
		}
	}
	out.WriteString(s.Mid)
	out.WriteString(strings.ReplaceAll(s.Dest, "`", ""))
	out.WriteString(s.Tail)
}

// plainCode is CODE, a code span's content, written to draw as the same
// characters in plain text. Its content is normalized as CommonMark draws a
// code span (one space each side dropped when both are there), and only the
// characters that could open markup where they stand are escaped: an escape
// splits the renderer's text, which would stop a file link's text being read
// as one path (webapp markdown.ts, file-link-label).
func plainCode(code string) string {
	if len(code) >= 2 && code[0] == ' ' && code[len(code)-1] == ' ' && strings.TrimSpace(code) != "" {
		code = code[1 : len(code)-1]
	}
	var out strings.Builder
	for i := 0; i < len(code); i++ {
		c := code[i]
		if needsEscape(code, i) {
			out.WriteByte('\\')
		}
		out.WriteByte(c)
	}
	return out.String()
}

// needsEscape reports whether CODE[I] could open markup in plain text: an
// emphasis or strikethrough delimiter, a bracket, an autolink or entity
// opener, a formula's `$`, a backslash or a backtick. An `_` inside a word, a
// lone `~` and a `!` not before `[` stay as written.
func needsEscape(code string, i int) bool {
	switch c := code[i]; c {
	case '\\', '`', '*', '[', ']', '<', '&', '$':
		return true
	case '_':
		return !(i > 0 && i+1 < len(code) && isWordByte(code[i-1]) && isWordByte(code[i+1]))
	case '~':
		return i+1 < len(code) && code[i+1] == '~' || i > 0 && code[i-1] == '~'
	case '!':
		return i+1 < len(code) && code[i+1] == '['
	}
	return false
}

func isWordByte(c byte) bool {
	return c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c >= '0' && c <= '9'
}

func isWord(r rune) bool { return r == '_' || unicode.IsLetter(r) || unicode.IsDigit(r) }
