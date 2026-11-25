// Package mdinline reads a drawn markdown's inline structure the way the
// webapp's renderer does (markdown-it with its linkify-it linkifier,
// webapp/src/markdown.ts), for the daemon's rewrites of drawn markdown: which
// lines are fenced code or link reference definitions, and which stretches of
// a prose line are code spans, links, autolinks and bare URLs. Every daemon
// rewrite that must leave code or links alone (codespans, pullrefs, slackrefs) reads them
// here, so no two of them can disagree about where a link is.
//
// THE RENDERER'S GRAMMAR IS THE AUTHORITY. A bare URL here is what the
// renderer links: an `http://` or `https://` URL its inline linkify rule
// matches, or a `www.` host its fuzzy linkifier matches, with the extent
// linkify-it's path grammar gives it. The webapp's tests render this package's
// answers for a shared corpus (webapp/test/fixtures/link-code.json, written by
// the prose package's link-code fixture test) and fail when the renderer reads a
// link differently.
//
// A bare host with no scheme and no `www.` (`github.com/x`), which the
// renderer's fuzzy linkifier also links, is NOT read as a URL here: telling it
// from a file name or an identifier takes linkify-it's top-level domain list.
package mdinline

import (
	"strings"
	"unicode"
	"unicode/utf8"

	"agent-repld/internal/mdfence"
)

// LineKind is what a line of a drawn markdown is.
type LineKind int

const (
	// Prose is a line whose inline structure Spans reads.
	Prose LineKind = iota
	// Fenced opens, closes, or lies inside a fenced code block.
	Fenced
	// Definition is a link reference definition (`[label]: <dest> "title"`).
	Definition
)

// Line is one line of a drawn markdown, with its newline.
type Line struct {
	Text string
	Kind LineKind
	// Def is a Definition line's parts, nil on any other line.
	Def *Span
}

// Doc is a drawn markdown read into its lines, with the labels its link
// reference definitions define.
type Doc struct {
	Lines  []Line
	labels map[string]bool
}

// Read reads TEXT into its lines. A fence opens on a line indented at most
// three spaces whose delimiter opens one, and closes on a line indented at
// most three spaces whose delimiter closes it (mdfence). A definition stands
// only where a paragraph cannot be continuing: at the text's start, after a
// blank line, a fence, or another definition.
func Read(text string) Doc {
	doc := Doc{labels: map[string]bool{}}
	var open mdfence.Delimiter
	inFence := false
	blockStart := true
	for _, text := range strings.SplitAfter(text, "\n") {
		if text == "" {
			continue
		}
		trimmed := strings.TrimLeft(text, " ")
		indent := len(text) - len(trimmed)
		d := mdfence.Parse(strings.TrimRight(trimmed, "\r\n"))
		switch {
		case inFence:
			if indent <= 3 && d.Closes(open) {
				inFence = false
			}
			doc.Lines = append(doc.Lines, Line{Text: text, Kind: Fenced})
			blockStart = true
		case indent <= 3 && d.Opens():
			open, inFence = d, true
			doc.Lines = append(doc.Lines, Line{Text: text, Kind: Fenced})
		default:
			if def, label, ok := definitionAt(text); ok && blockStart {
				doc.labels[label] = true
				doc.Lines = append(doc.Lines, Line{Text: text, Kind: Definition, Def: &def})
				continue
			}
			doc.Lines = append(doc.Lines, Line{Text: text, Kind: Prose})
			blockStart = strings.TrimSpace(text) == ""
		}
	}
	return doc
}

// Kind is what a stretch of a prose line is.
type Kind int

const (
	// Text is prose outside every other kind.
	Text Kind = iota
	// Escape is a backslash and the ASCII punctuation it makes literal.
	Escape
	// Code is an inline code span.
	Code
	// Link is an inline link or image, or a reference link to a label the
	// text defines.
	Link
	// Autolink is a `<scheme:...>` autolink.
	Autolink
	// URL is a bare URL the renderer links.
	URL
	// Def is a link reference definition (Line.Def).
	Def
)

// Span is one stretch of a prose line, at byte offset At of it, whose source
// is Raw.
//
// A link kind (Link, Autolink, URL, Def) is taken apart so its text and its
// destination can be rewritten on their own: Raw is Open+Label+Mid+Dest+Tail.
// Label is what the link draws as its text (a Link's bracketed text, empty for
// the others, whose text is their destination), and Dest is its destination,
// empty for a reference link, whose destination is its definition's.
type Span struct {
	Kind                         Kind
	At                           int
	Raw                          string
	Inner                        string // a Code span's code, between its backtick runs
	Open, Label, Mid, Dest, Tail string
}

// IsLink reports whether S is any kind of link.
func (s Span) IsLink() bool {
	switch s.Kind {
	case Link, Autolink, URL, Def:
		return true
	}
	return false
}

// Spans reads LINE, a Prose line, into its stretches, left to right as the
// renderer's inline rules meet them: whichever construct starts first wins,
// so a URL holding a backtick runs through it, and a code span holding a URL
// stays code.
func (d Doc) Spans(line string) []Span {
	var out []Span
	textFrom := 0
	emit := func(s Span) {
		if s.At > textFrom {
			out = append(out, Span{Kind: Text, At: textFrom, Raw: line[textFrom:s.At]})
		}
		out = append(out, s)
		textFrom = s.At + len(s.Raw)
	}
	for i := 0; i < len(line); {
		c := line[i]
		var s Span
		ok := false
		switch {
		case c == '\\':
			if i+1 < len(line) && isASCIIPunct(line[i+1]) {
				emit(Span{Kind: Escape, At: i, Raw: line[i : i+2]})
				i += 2
				continue
			}
		case c == '`':
			run := backtickRun(line, i)
			end := closingRun(line, i+run, run)
			if end < 0 {
				i += run
				continue
			}
			s, ok = Span{Kind: Code, At: i, Raw: line[i : end+run], Inner: line[i+run : end]}, true
		case c == '[' || c == '!' && i+1 < len(line) && line[i+1] == '[':
			s, ok = d.linkAt(line, i)
		case c == '<':
			s, ok = autolinkAt(line, i)
		default:
			s, ok = urlAt(line, i)
		}
		if !ok {
			_, size := utf8.DecodeRuneInString(line[i:])
			i += size
			continue
		}
		emit(s)
		i += len(s.Raw)
	}
	if textFrom < len(line) {
		out = append(out, Span{Kind: Text, At: textFrom, Raw: line[textFrom:]})
	}
	return out
}

// linkAt reads the link opening with the `[` (or `![`) at I: its bracketed
// text up to the matching `]` (brackets nest, code spans and escapes hide
// theirs), then an inline destination in parentheses, a full reference
// `[label]`, or none, the text itself being the label. A reference link is a
// link only when the text defines its label. A destination still unclosed at
// the line's end runs to it, so a link arriving mid-destination stays whole.
func (d Doc) linkAt(line string, i int) (Span, bool) {
	open := "["
	if line[i] == '!' {
		open = "!["
	}
	from := i + len(open)
	close := labelEnd(line, from)
	if close < 0 {
		return Span{}, false
	}
	label := line[from:close]
	j := close + 1
	if j < len(line) && line[j] == '(' {
		dest, tail := destinationAt(line, j+1)
		mid := line[close : j+1+dest[0]]
		return Span{Kind: Link, At: i, Raw: line[i:tail], Open: open, Label: label, Mid: mid,
			Dest: line[j+1+dest[0] : j+1+dest[1]], Tail: line[j+1+dest[1] : tail]}, true
	}
	if j < len(line) && line[j] == '[' {
		if refClose := labelEnd(line, j+1); refClose >= 0 {
			ref := line[j+1 : refClose]
			if ref == "" {
				ref = label
			}
			if d.labels[normalLabel(ref)] {
				return Span{Kind: Link, At: i, Raw: line[i : refClose+1], Open: open, Label: label,
					Mid: line[close : refClose+1]}, true
			}
		}
	}
	if d.labels[normalLabel(label)] {
		return Span{Kind: Link, At: i, Raw: line[i : close+1], Open: open, Label: label, Mid: "]"}, true
	}
	return Span{}, false
}

// labelEnd answers where the `]` closing a link's bracketed text starting at
// FROM stands, -1 for none on the line.
func labelEnd(line string, from int) int {
	depth := 1
	for j := from; j < len(line); {
		switch line[j] {
		case '`':
			run := backtickRun(line, j)
			if end := closingRun(line, j+run, run); end >= 0 {
				j = end + run
			} else {
				j += run
			}
			continue
		case '\\':
			j += 2
			continue
		case '[':
			depth++
		case ']':
			depth--
			if depth == 0 {
				return j
			}
		}
		j++
	}
	return -1
}

// destinationAt reads an inline link's parenthesized part starting at FROM,
// just past its `(`: the destination's bounds relative to FROM (leading
// whitespace before it), and the offset in LINE just past the closing `)`, or
// the line's end when none closes it. A destination is `<...>` or a run with
// no whitespace and balanced parentheses; a title may follow it.
func destinationAt(line string, from int) (dest [2]int, tail int) {
	j := from
	for j < len(line) && (line[j] == ' ' || line[j] == '\t') {
		j++
	}
	start := j
	if j < len(line) && line[j] == '<' {
		for j++; j < len(line) && line[j] != '>' && line[j] != '\n'; j++ {
			if line[j] == '\\' {
				j++
			}
		}
		if j < len(line) && line[j] == '>' {
			j++
		}
	} else {
		parens := 0
	scan:
		for ; j < len(line); j++ {
			switch line[j] {
			case '\\':
				j++
			case '(':
				parens++
			case ')':
				if parens == 0 {
					break scan
				}
				parens--
			case ' ', '\t', '\n', '\r':
				break scan
			}
		}
	}
	end := min(j, len(line))
	dest = [2]int{start - from, end - from}
	parens := 0
	for k := end; k < len(line); k++ {
		switch line[k] {
		case '\\':
			k++
		case '(':
			parens++
		case ')':
			if parens == 0 {
				return dest, k + 1
			}
			parens--
		}
	}
	return dest, len(line)
}

// autolinkAt reads the `<scheme:...>` autolink at I, as CommonMark spells
// one: a scheme of two to thirty-two characters, then no whitespace, `<` or
// `>` up to the closing `>`.
func autolinkAt(line string, i int) (Span, bool) {
	j := i + 1
	if j >= len(line) || !isASCIILetter(line[j]) {
		return Span{}, false
	}
	for j++; j < len(line) && (isASCIIAlnum(line[j]) || line[j] == '+' || line[j] == '.' || line[j] == '-'); j++ {
	}
	if n := j - (i + 1); n < 2 || n > 32 || j >= len(line) || line[j] != ':' {
		return Span{}, false
	}
	for ; j < len(line); j++ {
		switch c := line[j]; {
		case c == '>':
			return Span{Kind: Autolink, At: i, Raw: line[i : j+1], Open: "<", Dest: line[i+1 : j], Tail: ">"}, true
		case c == '<' || c <= ' ' || c == 0x7f:
			return Span{}, false
		}
	}
	return Span{}, false
}

// definitionAt reads LINE as a link reference definition: at most three
// spaces, `[label]:`, whitespace, a destination, then whitespace and a title
// or nothing. It answers the definition's parts and its normalized label.
func definitionAt(line string) (Span, string, bool) {
	indent := len(line) - len(strings.TrimLeft(line, " "))
	if indent > 3 || indent >= len(line) || line[indent] != '[' {
		return Span{}, "", false
	}
	close := labelEnd(line, indent+1)
	if close < 0 || close+1 >= len(line) || line[close+1] != ':' {
		return Span{}, "", false
	}
	label := line[indent+1 : close]
	if strings.TrimSpace(label) == "" {
		return Span{}, "", false
	}
	j := close + 2
	for j < len(line) && (line[j] == ' ' || line[j] == '\t') {
		j++
	}
	start := j
	for j < len(line) && !isSpaceByte(line[j]) {
		j++
	}
	if j == start {
		return Span{}, "", false
	}
	rest := line[j:]
	if r := strings.TrimSpace(rest); r != "" && !strings.ContainsAny(r[:1], "\"'(") {
		return Span{}, "", false
	}
	return Span{Kind: Def, Raw: line, Open: line[:start], Dest: line[start:j], Tail: rest}, normalLabel(label), true
}

// normalLabel is LABEL as reference matching compares it: case folded, with
// its whitespace runs collapsed.
func normalLabel(label string) string {
	return strings.ToLower(strings.Join(strings.Fields(label), " "))
}

// urlAt reads the bare URL starting at I, when one does: `http://`,
// `https://` (any case) not glued to a scheme character before it, as the
// renderer's inline linkify rule reads one, or `www.` not glued to a word or
// to `.:/\-_@`, as its fuzzy linkifier does.
func urlAt(line string, i int) (Span, bool) {
	rest := line[i:]
	var prefix int
	var prev byte
	if i > 0 {
		prev = line[i-1]
	}
	switch {
	case hasFoldPrefix(rest, "https://"):
		prefix = len("https://")
	case hasFoldPrefix(rest, "http://"):
		prefix = len("http://")
	case hasFoldPrefix(rest, "www."):
		if i > 0 && (isASCIIAlnum(prev) || strings.IndexByte(".:/\\-_@", prev) >= 0) {
			return Span{}, false
		}
		end := urlEnd(line, i+len("www."))
		if end == i+len("www.") {
			return Span{}, false
		}
		return Span{Kind: URL, At: i, Raw: line[i:end], Dest: line[i:end]}, true
	default:
		return Span{}, false
	}
	if i > 0 && (isASCIIAlnum(prev) || prev == '.' || prev == '+' || prev == '-') {
		return Span{}, false
	}
	end := urlEnd(line, i+prefix)
	if end == i+prefix {
		return Span{}, false
	}
	return Span{Kind: URL, At: i, Raw: line[i:end], Dest: line[i:end]}, true
}

// SchemeURL reports whether S is, whole, one bare `http://` or `https://`
// URL the renderer links (urlAt), with no character an autolink cannot hold
// (whitespace, `<`, `>`, a backtick). A `www.` URL is not one: it has no
// scheme to stand as an autolink.
func SchemeURL(s string) bool {
	if hasFoldPrefix(s, "www.") || strings.ContainsAny(s, "<>`") {
		return false
	}
	for i := 0; i < len(s); i++ {
		if s[i] <= ' ' || s[i] == 0x7f {
			return false
		}
	}
	span, ok := urlAt(s, 0)
	return ok && span.Raw == s
}

// urlEnd answers where the URL whose host starts at FROM ends: its host, then
// any path, query or fragment, by linkify-it's path grammar (its re.js
// src_path). A backtick is an ordinary character of either, since the
// renderer's linkifier reads it as one.
func urlEnd(line string, from int) int {
	j := from
	for j < len(line) {
		r, size := utf8.DecodeRuneInString(line[j:])
		if r == '/' || r == '?' || r == '#' || !isHostRune(r) {
			break
		}
		j += size
	}
	for j > from && (line[j-1] == '.' || line[j-1] == ':' || line[j-1] == '-') {
		j--
	}
	if j == from {
		return from
	}
	if j >= len(line) || (line[j] != '/' && line[j] != '?' && line[j] != '#') {
		return j
	}
	for j < len(line) {
		n := pathStep(line, j)
		if n == 0 {
			break
		}
		j += n
	}
	return j
}

// isHostRune reports whether R may stand in a URL's host, port or user info.
func isHostRune(r rune) bool {
	if r >= utf8.RuneSelf {
		return unicode.IsLetter(r) || unicode.IsDigit(r)
	}
	c := byte(r)
	return isASCIIAlnum(c) || strings.IndexByte("-._~%@:`", c) >= 0
}

// pathStep answers how many bytes of a URL's path the character at J takes
// (an enclosed pair takes all of it), 0 where the path ends. It follows
// linkify-it's src_path: brackets and quotes only as closed pairs, and `.`,
// `,`, `;`, `!`, `?` only when more of the path follows them.
func pathStep(line string, j int) int {
	r, size := utf8.DecodeRuneInString(line[j:])
	if isSeparator(r) {
		return 0
	}
	more := func(k int) bool {
		if k >= len(line) {
			return false
		}
		next, _ := utf8.DecodeRuneInString(line[k:])
		return !isSeparator(next)
	}
	switch r {
	case '(', '[', '{', '"':
		closer := map[rune]byte{'(': ')', '[': ']', '{': '}', '"': '"'}[r]
		for k := j + 1; k < len(line); {
			c, n := utf8.DecodeRuneInString(line[k:])
			if isSeparator(c) {
				return 0
			}
			if byte(c) == closer && c < utf8.RuneSelf {
				if r == '"' && k == j+1 {
					return 0
				}
				return k + 1 - j
			}
			k += n
		}
		return 0
	case ')', ']', '}':
		return 0
	case apostrophe:
		for k := j + 1; k < len(line); {
			c, n := utf8.DecodeRuneInString(line[k:])
			if isSeparator(c) {
				break
			}
			if c == apostrophe && k > j+1 {
				return k + 1 - j
			}
			k += n
		}
		if j+1 < len(line) {
			next, _ := utf8.DecodeRuneInString(line[j+1:])
			if unicode.IsLetter(next) || unicode.IsDigit(next) || next == '-' {
				return 1
			}
		}
		return 0
	case '.':
		n := 0
		for j+n < len(line) && line[j+n] == '.' {
			n++
		}
		if n >= 2 {
			if j+n < len(line) && (isASCIIAlnum(line[j+n]) || strings.IndexByte("%/&", line[j+n]) >= 0) {
				return n + 1
			}
			return 0
		}
		if more(j+1) && line[j+1] != '.' {
			return 1
		}
		return 0
	case '-':
		n := 0
		for j+n < len(line) && line[j+n] == '-' {
			n++
		}
		if n == 3 {
			return 0
		}
		return n
	case ',', ';':
		if more(j + 1) {
			return 1
		}
		return 0
	case '!':
		n := 0
		for j+n < len(line) && line[j+n] == '!' {
			n++
		}
		if more(j + n) {
			return n
		}
		return 0
	case '?':
		if more(j+1) && line[j+1] != '?' {
			return 1
		}
		return 0
	}
	return size
}

// apostrophe is the single quote linkify-it pairs in a path, spelled by code
// point so no scan for sh quoting mistakes it for one (shellword).
const apostrophe = '\u0027'

// isSeparator reports whether R ends a URL: whitespace, a control character,
// or one of linkify-it's text separators.
func isSeparator(r rune) bool {
	return unicode.IsSpace(r) || unicode.IsControl(r) || r == '<' || r == '>' || r == '｜'
}

// hasFoldPrefix reports whether S starts with PREFIX, ASCII case ignored.
func hasFoldPrefix(s, prefix string) bool {
	return len(s) >= len(prefix) && strings.EqualFold(s[:len(prefix)], prefix)
}

// backtickRun is the length of the backtick run at I of LINE.
func backtickRun(line string, i int) int {
	n := 0
	for i+n < len(line) && line[i+n] == '`' {
		n++
	}
	return n
}

// closingRun answers where the next backtick run of exactly N backticks at or
// after FROM begins, -1 when the span never closes.
func closingRun(line string, from, n int) int {
	for i := from; i < len(line); {
		if line[i] != '`' {
			i++
			continue
		}
		run := backtickRun(line, i)
		if run == n {
			return i
		}
		i += run
	}
	return -1
}

func isASCIILetter(c byte) bool { return c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' }
func isASCIIAlnum(c byte) bool  { return isASCIILetter(c) || c >= '0' && c <= '9' }
func isSpaceByte(c byte) bool   { return c == ' ' || c == '\t' || c == '\n' || c == '\r' }

// isASCIIPunct reports whether C is ASCII punctuation, which a backslash
// makes literal.
func isASCIIPunct(c byte) bool {
	return c >= '!' && c <= '/' || c >= ':' && c <= '@' || c >= '[' && c <= '`' || c >= '{' && c <= '~'
}
