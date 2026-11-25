package mdinline

import "strings"

// Stretch is one stretch of prose (a Text span) a rewrite is handed: the
// bytes Line[From:To] of the prose line Line, with its newline. The whole line
// is handed so a rewrite can judge a word's boundaries against what stands
// beside the stretch.
type Stretch struct {
	Line     string
	From, To int
	// AtEnd says the stretch ends a text still ARRIVING: the next push may
	// extend its last word, so a rewrite leaves a word running to To alone.
	AtEnd bool
}

// RewriteProse answers MARKDOWN with every stretch of prose written by WRITE
// and everything else copied as written: fenced code, link reference
// definitions, code spans, escapes, and every part of a link (its text, its
// destination, a bare URL, an autolink). It is THE ONE WALK every daemon
// rewrite of drawn prose takes (codespans.Markup, pullrefs.Linkify,
// slackrefs.Linkify), so no two of them can disagree about what is prose.
// ARRIVING says the text is still arriving, which only its end can show
// (Stretch.AtEnd).
func RewriteProse(markdown string, arriving bool, write func(out *strings.Builder, s Stretch)) string {
	var out strings.Builder
	out.Grow(len(markdown))
	doc := Read(markdown)
	offset := 0
	for _, line := range doc.Lines {
		offset += len(line.Text)
		if line.Kind != Prose {
			out.WriteString(line.Text)
			continue
		}
		lastLine := arriving && offset == len(markdown)
		for _, s := range doc.Spans(line.Text) {
			if s.Kind != Text {
				out.WriteString(s.Raw)
				continue
			}
			to := s.At + len(s.Raw)
			write(&out, Stretch{Line: line.Text, From: s.At, To: to, AtEnd: lastLine && to == len(line.Text)})
		}
	}
	return out.String()
}

// StartsWord reports whether position I of LINE begins a word of prose: the
// line's start, after whitespace, or after an opening parenthesis. A
// parenthesis that opens a link's destination never reaches a rewrite,
// because Spans reads the whole link.
func StartsWord(line string, i int) bool {
	if i == 0 {
		return true
	}
	switch line[i-1] {
	case ' ', '\t', '(':
		return true
	}
	return false
}

// IsWordByte reports whether C continues an ASCII word, so a reference
// followed by it is not a reference.
func IsWordByte(c byte) bool {
	return c == '_' || c >= '0' && c <= '9' || c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z'
}

// WriteLink writes the inline markdown link TEXT to URL, the one spelling a
// rewrite draws a link it adds with.
func WriteLink(out *strings.Builder, text, url string) {
	out.WriteString("[" + text + "](" + url + ")")
}
