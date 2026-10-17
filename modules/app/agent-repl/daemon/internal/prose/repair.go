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
	if r < markerFloor || (r >= 0x2500 && r <= 0x257f) {
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
