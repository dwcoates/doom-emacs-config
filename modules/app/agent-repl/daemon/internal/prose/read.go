// Package prose is the daemon's one parse of the prose it serves: every text
// the webapp draws as markdown is parsed here with the tree-sitter-tldr
// grammar, its trees repaired and turned into graphs, and served as
// frontend.v1.ParsedProse (proto/src/frontend/v1/prose.proto). No other
// program parses a TLDR tree.
//
// Four layers, each in its own file:
//
//   - read.go reads the grammar's parse into typed lines and fences. It
//     answers exactly what the webapp's line parser answered before the
//     grammar replaced it (tree-sitter-tldr/tldr/test/golden), and is held to
//     those answers by golden_test.go.
//   - repair.go gives a label to a root the model wrote without one, so the
//     grammar sees it as a root.
//   - graph.go turns a tree's lines into nodes related by parent, labelled by
//     their place.
//   - parse.go puts the text together as plain runs and trees.
package prose

import (
	"fmt"
	"strings"
	"sync"

	tldr "agentrepl/tldr"

	ts "github.com/tree-sitter/go-tree-sitter"
)

// lineClass is what a line is by the webapp's old line predicates, tested in
// findTreeRegion's order.
type lineClass int

const (
	classConnector lineClass = iota
	classRoot
	classHeader
	classDotted
	classBlank
	classOther
)

// lineClasses maps a line node's type, and a fence line's class leaf, to its
// class.
var lineClasses = map[string]lineClass{
	"connector_line": classConnector,
	"root_line":      classRoot,
	"header_line":    classHeader,
	"label_line":     classDotted,
	"blank_line":     classBlank,
	"other_line":     classOther,
	"connector":      classConnector,
	"root":           classRoot,
	"header":         classHeader,
	"dotted":         classDotted,
	"blank":          classBlank,
	"other":          classOther,
}

// branch is a branch head (parseBranch): its label keeps its trailing
// whitespace.
type branch struct {
	indent, prefix, label, body string
}

// continuation is a wrapped remainder (parseContinuation), before its column
// is measured.
type continuation struct {
	indent, prefix, padding, text string
}

// item is one line outside a fence, or one fence: exactly one of line and
// fence is set.
type item struct {
	line  *textLine
	fence *fence
	// start and end are the item's byte span in the text, its final newline
	// included when it has one.
	start, end int
}

// textLine is a line outside any fence.
type textLine struct {
	// text is the line as written, without its newline.
	text         string
	class        lineClass
	branch       *branch
	continuation *continuation
}

// fence is a ``` fence, from its open delimiter to its close or the end of
// the text.
type fence struct {
	// lang is the open delimiter's first info word, or "".
	lang string
	// code is the body, each line dedented by the open delimiter's rail.
	code []string
	// classes is each body line's class, which isMetapromptTree counted.
	classes []lineClass
	closed  bool
}

// document is a parsed text: every line in order, and the tree region if
// there is one.
type document struct {
	text  string
	items []item
	// region is set when the text holds a tree, as findTreeRegion found it.
	region *region
}

// region is the tree carved out of a text.
type region struct {
	// before, tree and after are findTreeRegion's three strings.
	before, tree, after string
	// first and last bound the tree's items in document.items, inclusive.
	first, last int
}

var language = ts.NewLanguage(tldr.Language())

// parsers pools tree-sitter parsers, which are not safe for concurrent use.
var parsers = sync.Pool{New: func() any {
	p := ts.NewParser()
	if err := p.SetLanguage(language); err != nil {
		panic(fmt.Sprintf("prose: the tree-sitter-tldr grammar does not load: %v", err))
	}
	return p
}}

// read parses TEXT with the grammar. A parse the grammar could not complete
// is an invariant violation: the grammar accepts every text whole by design.
func read(text string) (document, error) {
	p := parsers.Get().(*ts.Parser)
	defer parsers.Put(p)
	src := []byte(text)
	tree := p.Parse(src, nil)
	if tree == nil {
		return document{}, fmt.Errorf("the grammar returned no tree for a %d-byte text", len(text))
	}
	defer tree.Close()
	root := tree.RootNode()
	if root.HasError() {
		sexp := root.ToSexp()
		if len(sexp) > 500 {
			sexp = sexp[:500]
		}
		return document{}, fmt.Errorf("the grammar could not parse a %d-byte text, which it accepts whole by design: %s", len(text), sexp)
	}
	d := document{text: text}
	r := reader{src: src}
	if err := r.items(root, &d.items); err != nil {
		return document{}, err
	}
	// `text.split("\n")` ends on an empty line after a final newline (and is
	// one empty line for an empty text); the grammar has no line there. It
	// belongs to an unclosed fence's body, else it is a blank line of its own.
	if text == "" || strings.HasSuffix(text, "\n") {
		if n := len(d.items); n > 0 && d.items[n-1].fence != nil && !d.items[n-1].fence.closed {
			last := d.items[n-1].fence
			last.code = append(last.code, "")
			last.classes = append(last.classes, classBlank)
		} else {
			d.items = append(d.items, item{line: &textLine{class: classBlank}, start: len(text), end: len(text)})
		}
	}
	treeNode := childOfKind(root, "tree")
	if treeNode == nil {
		return d, nil
	}
	start, end := int(treeNode.StartByte()), int(treeNode.EndByte())
	treeEnd := end
	if treeEnd > start && text[treeEnd-1] == '\n' {
		treeEnd--
	}
	reg := &region{tree: text[start:treeEnd], after: text[end:], first: -1}
	if start > 0 {
		reg.before = text[:start-1]
	}
	for i, it := range d.items {
		if it.start >= start && it.end <= end && it.end > it.start {
			if reg.first < 0 {
				reg.first = i
			}
			reg.last = i
		}
	}
	if reg.first < 0 {
		return document{}, fmt.Errorf("the tree region [%d, %d) holds no item", start, end)
	}
	d.region = reg
	return d, nil
}

// reader reads one parse's nodes against its source bytes.
type reader struct {
	src []byte
}

func (r reader) text(n *ts.Node) string {
	return string(r.src[n.StartByte():n.EndByte()])
}

// childOfKind is N's first named child of KIND, or nil.
func childOfKind(n *ts.Node, kind string) *ts.Node {
	for i := uint(0); i < n.NamedChildCount(); i++ {
		c := n.NamedChild(i)
		if c != nil && c.Kind() == kind {
			return c
		}
	}
	return nil
}

func (r reader) childText(n *ts.Node, kind string) string {
	if c := childOfKind(n, kind); c != nil {
		return r.text(c)
	}
	return ""
}

// items appends the items of a container's line and fence children, in order.
func (r reader) items(n *ts.Node, into *[]item) error {
	for i := uint(0); i < n.NamedChildCount(); i++ {
		c := n.NamedChild(i)
		if c == nil {
			continue
		}
		span := item{start: int(c.StartByte()), end: int(c.EndByte())}
		switch c.Kind() {
		case "fence":
			f, err := r.fence(c)
			if err != nil {
				return err
			}
			span.fence = f
			*into = append(*into, span)
		case "before", "after", "tree":
			if err := r.items(c, into); err != nil {
				return err
			}
		default:
			l, err := r.line(c)
			if err != nil {
				return err
			}
			span.line = l
			*into = append(*into, span)
		}
	}
	return nil
}

func (r reader) line(n *ts.Node) (*textLine, error) {
	class, ok := lineClasses[n.Kind()]
	if !ok {
		return nil, fmt.Errorf("a line node of kind %q has no class", n.Kind())
	}
	body := n.NamedChild(0)
	if body == nil {
		return nil, fmt.Errorf("a line node of kind %q has no body", n.Kind())
	}
	l := &textLine{text: strings.TrimSuffix(r.text(n), "\n"), class: class}
	switch body.Kind() {
	case "branch":
		l.branch = &branch{
			indent: r.childText(body, "indent"),
			prefix: r.childText(body, "prefix"),
			label:  r.childText(body, "label"),
			body:   r.childText(body, "text"),
		}
	case "continuation":
		l.continuation = &continuation{
			indent:  r.childText(body, "indent"),
			prefix:  r.childText(body, "prefix"),
			padding: r.childText(body, "padding"),
			text:    r.childText(body, "text"),
		}
	case "raw":
	default:
		return nil, fmt.Errorf("a line node of kind %q has an unknown body %q", n.Kind(), body.Kind())
	}
	return l, nil
}

func (r reader) fence(n *ts.Node) (*fence, error) {
	open := childOfKind(n, "fence_open")
	if open == nil {
		return nil, fmt.Errorf("a fence has no open delimiter")
	}
	f := &fence{lang: r.childText(open, "language")}
	for i := uint(0); i < n.NamedChildCount(); i++ {
		c := n.NamedChild(i)
		if c == nil {
			continue
		}
		switch c.Kind() {
		case "fence_line":
			cls := c.ChildByFieldName("class")
			if cls == nil {
				return nil, fmt.Errorf("a fence line has no class")
			}
			class, ok := lineClasses[cls.Kind()]
			if !ok {
				return nil, fmt.Errorf("a fence line's class %q is unknown", cls.Kind())
			}
			f.code = append(f.code, r.childText(c, "text"))
			f.classes = append(f.classes, class)
		case "fence_close":
			f.closed = true
		}
	}
	return f, nil
}

// ---- The old parser's questions, answered from the parse -----------------

// treeLineRatio is the minimum fraction of non-blank lines that must look
// tree-shaped.
const treeLineRatio = 0.6

// classSentinelDelimiter stands for a fence delimiter line in lineClassesOf.
const classSentinelDelimiter lineClass = -1

// lineClassesOf is every line's class, fences' delimiters and bodies included,
// in order.
func lineClassesOf(items []item) []lineClass {
	var out []lineClass
	for _, it := range items {
		if it.line != nil {
			out = append(out, it.line.class)
			continue
		}
		out = append(out, classSentinelDelimiter)
		out = append(out, it.fence.classes...)
		if it.fence.closed {
			out = append(out, classSentinelDelimiter)
		}
	}
	return out
}

// isMetapromptTree is whether D reads as a metaprompt tree: at least two
// non-blank lines, most of them tree-shaped (connector or dotted-label start),
// anchored by a connector line or an emoji-prefixed root. Fences do not matter
// here.
func isMetapromptTree(d document) bool {
	var lines []lineClass
	for _, c := range lineClassesOf(d.items) {
		if c != classBlank {
			lines = append(lines, c)
		}
	}
	if len(lines) < 2 {
		return false
	}
	treeish, anchored := 0, false
	for _, c := range lines {
		if c == classConnector || c == classRoot || c == classDotted {
			treeish++
		}
		if c == classConnector || c == classRoot {
			anchored = true
		}
	}
	return anchored && float64(treeish)/float64(len(lines)) >= treeLineRatio
}

// looksLikeIntendedTree is whether D's first non-blank line is the
// `Response (…)` header.
func looksLikeIntendedTree(d document) bool {
	for _, c := range lineClassesOf(d.items) {
		if c == classBlank {
			continue
		}
		return c == classHeader
	}
	return false
}
