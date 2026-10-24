package prose

import (
	"regexp"
	"strconv"
	"strings"
	"unicode"
	"unicode/utf8"

	frontendv1 "agentrepl/proto/frontend/v1"
)

// segmentWidth is the width, in code points, of one rail or connector segment
// of a branch's prefix (`│   `, `├── `).
const segmentWidth = 4

// markerFloor is the lowest code point a root's marker may start with: the
// webapp's EMOJI_FLOOR, and the grammar's.
const markerFloor = 0x2190

// edit is one rewrite of the source text: [start, end) becomes text.
type edit struct {
	start, end int
	text       string
}

// node is a tree node while the graph is built.
type node struct {
	depth  int
	parent int // -1 for a root
	label  string
	marker *string // a root's marker; nil on a branch and on a markerless root
	text   string
	code   []*frontendv1.ProseTreeCode
}

// buildTree turns the tree region's items into a ProseTree, and returns the
// edits that rewrite each node's written label in the source to its derived
// one. While the text is ARRIVING and ends mid-line, that last line's partial
// label is not drawn.
func buildTree(d document, stage Stage) (*frontendv1.ProseTree, []edit) {
	tree := &frontendv1.ProseTree{}
	var nodes []node
	var edits []edit
	// stack holds the open ancestors of the next node: indexes into nodes.
	var stack []int
	roots := 0
	children := map[int]int{}
	arriving := stage == Arriving && !strings.HasSuffix(d.text, "\n")

	for i := d.region.first; i <= d.region.last; i++ {
		it := d.items[i]
		if it.fence != nil {
			// An interior fence belongs to the node above it.
			code := &frontendv1.ProseTreeCode{Body: strings.Join(it.fence.code, "\n")}
			if it.fence.lang != "" {
				lang := it.fence.lang
				code.Language = &lang
			}
			last := len(nodes) - 1
			nodes[last].code = append(nodes[last].code, code)
			continue
		}
		l := it.line
		switch l.class {
		case classHeader:
			tree.Header = &frontendv1.ProseTreeHeader{Text: l.text}
			continue
		case classBlank:
			continue
		case classRoot, classConnector:
		default:
			// The region holds only a header, blank lines, core lines and interior
			// fences (findTreeRegion's rule).
			panic("prose: a tree region holds a line that is neither a header, a blank line nor a core line: " + strconv.Quote(l.text))
		}

		n := node{parent: -1}
		var labelAt, labelEnd int
		var bodyText string
		if l.class == classRoot {
			b := l.branch
			labelAt = it.start + len(b.indent) + len(b.prefix)
			labelEnd = labelAt + len(strings.TrimRightFunc(b.label, unicode.IsSpace))
			marker, rest := splitMarker(b.body)
			n.marker = &marker
			bodyText = rest
		} else {
			n.depth, labelAt, labelEnd, bodyText = branchShape(it)
		}

		if n.depth == 0 {
			stack = stack[:0]
			roots++
			n.label = strconv.Itoa(roots)
		} else {
			for len(stack) > 0 && nodes[stack[len(stack)-1]].depth >= n.depth {
				stack = stack[:len(stack)-1]
			}
			if len(stack) == 0 {
				// A branch with no root above it is a root by its place. It keeps
				// its written depth, so a sibling written beside it is another root
				// rather than its child.
				roots++
				n.label = strconv.Itoa(roots)
			} else {
				n.parent = stack[len(stack)-1]
				children[n.parent]++
				n.label = nodes[n.parent].label + "." + strconv.Itoa(children[n.parent])
			}
		}
		if arriving && it.end == len(d.text) && partialLabel.MatchString(bodyText) {
			// The model is still typing this line's own label.
			bodyText = ""
		}
		n.text = bodyText
		if labelEnd == labelAt {
			edits = append(edits, edit{start: labelAt, end: labelAt, text: n.label + " "})
		} else {
			edits = append(edits, edit{start: labelAt, end: labelEnd, text: n.label})
		}
		stack = append(stack, len(nodes))
		nodes = append(nodes, n)
	}

	for i, n := range nodes {
		pn := &frontendv1.ProseTreeNode{
			Id:    &frontendv1.ProseTreeNodeId{Index: uint32(i)},
			Label: &frontendv1.ProseTreeNodeLabel{Text: n.label},
			Text:  &frontendv1.ProseTreeNodeText{Markdown: n.text},
			Code:  n.code,
		}
		if n.parent < 0 {
			root := &frontendv1.ProseTreeNodeRoot{}
			if n.marker != nil {
				root.Marker = &frontendv1.ProseTreeNodeMarker{Text: *n.marker}
			}
			pn.Place = &frontendv1.ProseTreeNode_Root{Root: root}
		} else {
			pn.Place = &frontendv1.ProseTreeNode_Branch{Branch: &frontendv1.ProseTreeNodeBranch{
				Parent: &frontendv1.ProseTreeNodeId{Index: uint32(n.parent)},
			}}
		}
		tree.Nodes = append(tree.Nodes, pn)
	}
	return tree, edits
}

// partialLabel is what a branch's text reads as while the model is still
// typing its label: digits and dots and nothing else.
var partialLabel = regexp.MustCompile(`^[0-9.]+$`)

// branchShape is a connector line's depth, the byte span of its written label
// (empty where it wrote none, at the place a label goes) and its text.
func branchShape(it item) (depth, labelAt, labelEnd int, text string) {
	l := it.line
	if b := l.branch; b != nil {
		depth = utf8.RuneCountInString(b.prefix) / segmentWidth
		labelAt = it.start + len(b.indent) + len(b.prefix)
		labelEnd = labelAt + len(strings.TrimRightFunc(b.label, unicode.IsSpace))
		return depth, labelAt, labelEnd, b.body
	}
	// A connector line no branch parse fits (`├──text`, a connector with no
	// space after it): its depth is the rail before the connector, and its text
	// is what follows the connector.
	rail := strings.IndexFunc(l.text, func(r rune) bool { return r != ' ' && r != '│' })
	depth = utf8.RuneCountInString(l.text[:rail])/segmentWidth + 1
	after := rail + len("├──")
	labelAt = it.start + after
	return depth, labelAt, labelAt, strings.TrimSpace(l.text[after:])
}

// emojiPresentation is the emoji presentation selector. A code point below
// markerFloor that it follows is an emoji too (ℹ️ U+2139, ‼️ U+203C, ©️, the
// keycaps), so it opens a marker as well (2026-10-23: a root opening on ℹ️
// was read as prose, and its branches with it).
const emojiPresentation = 0xfe0f

// opensMarker is whether S opens on a root's marker: a code point at or above
// markerFloor, or one emojiPresentation follows. The grammars read a marker by
// the same rule (tldr's opens_marker, tldr_prompt's scan_heading_label and
// MARKER).
func opensMarker(s string) bool {
	r, size := utf8.DecodeRuneInString(s)
	if r == utf8.RuneError {
		return false
	}
	if r >= markerFloor {
		return true
	}
	next, _ := utf8.DecodeRuneInString(s[size:])
	return next == emojiPresentation
}

// splitMarker splits a root's body into its marker (opensMarker's code point,
// with any variation selector, keycap, skin tone or zero-width joined
// continuation) and the text after it.
func splitMarker(body string) (marker, text string) {
	i := 0
	if !opensMarker(body) {
		// The grammar classifies a root by this very rule.
		panic("prose: a root's body does not open on a marker: " + strconv.Quote(body))
	}
	_, size := utf8.DecodeRuneInString(body)
	i += size
	for i < len(body) {
		r, size := utf8.DecodeRuneInString(body[i:])
		switch {
		case r == emojiPresentation, r == 0x20e3, r >= 0x1f3fb && r <= 0x1f3ff:
			i += size
		case r == 0x200d:
			next, nsize := utf8.DecodeRuneInString(body[i+size:])
			if next < markerFloor {
				return body[:i], strings.TrimSpace(body[i:])
			}
			i += size + nsize
		default:
			return body[:i], strings.TrimSpace(body[i:])
		}
	}
	return body, ""
}

// apply rewrites TEXT by EDITS, which are in order and do not overlap.
func apply(text string, edits []edit) string {
	if len(edits) == 0 {
		return text
	}
	var b strings.Builder
	at := 0
	for _, e := range edits {
		b.WriteString(text[at:e.start])
		b.WriteString(e.text)
		at = e.end
	}
	b.WriteString(text[at:])
	return b.String()
}
