package prose

import (
	"strings"
	"unicode/utf8"
)

// placeholderLabel is the label repairUnlabeledRoots writes in front of a root
// the model left unlabeled. Any label makes the grammar read the line as a
// root; graph.go then rewrites it to the root's place, so its value never
// reaches a reader.
const placeholderLabel = "0 "

// repairUnlabeledRoots gives a label to every root the model wrote without
// one: a column-0 line, outside any fence, that opens on a marker and a space
// (`🔧 text`), and that a branch follows across blank lines. Without a label
// the grammar cannot read such a line as a root, so it would end the tree, or
// keep one from starting. Answers the edits that label them, in order; none
// when there is nothing to repair.
//
// A marker line no branch follows is left as written: until a branch arrives
// it is prose, and while a response streams in it becomes a root on the push
// that brings its first branch.
func repairUnlabeledRoots(d document) []edit {
	var edits []edit
	for i, it := range d.items {
		if it.line == nil || it.line.class != classOther || !opensOnMarker(it.line.text) {
			continue
		}
		if next := nextNonBlank(d.items, i+1); next >= 0 && d.items[next].line != nil && d.items[next].line.class == classConnector {
			edits = append(edits, edit{start: it.start, end: it.start, text: placeholderLabel})
		}
	}
	return edits
}

// opensOnMarker is whether LINE opens, at column 0, on a marker and a space. A
// box-drawing character (U+2500 to U+257F) is a tree's rail, never a marker:
// `│   text` is a wrapped line under a branch, not a root without a label.
func opensOnMarker(line string) bool {
	r, _ := utf8.DecodeRuneInString(line)
	if !opensMarker(line) || (r >= 0x2500 && r <= 0x257f) {
		return false
	}
	marker, _ := splitMarker(line)
	return strings.HasPrefix(line[len(marker):], " ")
}

// nextNonBlank is the index of the first item at or after FROM that is not a
// blank line, or -1.
func nextNonBlank(items []item, from int) int {
	for i := from; i < len(items); i++ {
		if it := items[i]; it.line == nil || it.line.class != classBlank {
			return i
		}
	}
	return -1
}

// repairFencedResponseTree drops the fence a model wrapped its whole response
// tree in: a fence opened on the first line after the `Response (…)` header,
// whatever its language, whose body holds a tree. The response's own tree is
// never fenced, so such a fence is a mistake, never a code block. Left in
// place, it breaks: the grammar closes a fence at any delimiter, a railed one
// included, so the first code block nested under a branch would close it and
// the fences after that would open and close out of step. Answers the edits
// that drop the wrapper's open and close lines; none when there is nothing to
// repair.
//
// The wrapper's close is matched on column-0 delimiters only: one with a
// language opens a code block nested in the tree, a bare one closes the
// innermost. A delimiter after a rail or an indent is the tree's own code and
// never closes the wrapper. While a response is ARRIVING, its last line,
// still unterminated, may be a delimiter whose language has not arrived, so it
// is never taken as the close; an unclosed wrapper loses only its open line.
func repairFencedResponseTree(text string, stage Stage) []edit {
	ls := rawLines(text)
	header := nextNonBlankLine(ls, 0)
	if header < 0 {
		return nil
	}
	open := nextNonBlankLine(ls, header+1)
	if open < 0 || !strings.HasPrefix(ls[open].text, "```") || !onlyHeader(text[:ls[open].start]) {
		return nil
	}
	closing, depth := -1, 0
	for j := open + 1; j < len(ls) && closing < 0; j++ {
		l := ls[j]
		if !strings.HasPrefix(l.text, "```") || (stage == Arriving && l.end == len(text) && !strings.HasSuffix(text, "\n")) {
			continue
		}
		switch {
		case strings.TrimSpace(strings.TrimLeft(l.text, "`")) != "":
			depth++
		case depth > 0:
			depth--
		default:
			closing = j
		}
	}
	bodyEnd := len(text)
	if closing >= 0 {
		bodyEnd = ls[closing].start
	}
	if !holdsTree(strings.TrimSuffix(text[ls[open].end:bodyEnd], "\n")) {
		return nil
	}
	edits := []edit{{start: ls[open].start, end: ls[open].end}}
	if closing >= 0 {
		edits = append(edits, edit{start: ls[closing].start, end: ls[closing].end})
	}
	return edits
}

// holdsTree is whether BODY, a fence's body, reads as a metaprompt tree: the
// one test of whether a fence wraps a tree rather than code.
func holdsTree(body string) bool {
	d := mustRead(body)
	return d.region != nil && isMetapromptTree(d)
}

// onlyHeader is whether TEXT's one non-blank line is the `Response (…)` header.
func onlyHeader(text string) bool {
	var classes []lineClass
	for _, c := range lineClassesOf(mustRead(text).items) {
		if c != classBlank {
			classes = append(classes, c)
		}
	}
	return len(classes) == 1 && classes[0] == classHeader
}

// rawLine is one physical line of a text: its byte span, its final newline
// included when it has one, and its text without the newline.
type rawLine struct {
	start, end int
	text       string
}

// rawLines is every physical line of TEXT, in order, fences or not.
func rawLines(text string) []rawLine {
	var out []rawLine
	for at := 0; at < len(text); {
		end := len(text)
		if nl := strings.IndexByte(text[at:], '\n'); nl >= 0 {
			end = at + nl + 1
		}
		out = append(out, rawLine{start: at, end: end, text: strings.TrimSuffix(text[at:end], "\n")})
		at = end
	}
	return out
}

// nextNonBlankLine is the index of the first of LS at or after FROM that is
// not blank, or -1.
func nextNonBlankLine(ls []rawLine, from int) int {
	for i := from; i < len(ls); i++ {
		if strings.TrimSpace(ls[i].text) != "" {
			return i
		}
	}
	return -1
}
