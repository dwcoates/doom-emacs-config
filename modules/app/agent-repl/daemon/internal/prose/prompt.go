package prose

import (
	"fmt"
	"strings"
	"sync"

	"agent-repld/internal/dlog"

	tldrprompt "agentrepl/tldrprompt"

	frontendv1 "agentrepl/proto/frontend/v1"

	ts "github.com/tree-sitter/go-tree-sitter"
)

// promptLanguage is the prompt grammar, which reads a prompt's entries.
var promptLanguage = ts.NewLanguage(tldrprompt.Language())

// promptParsers pools tree-sitter parsers of the prompt grammar, which are not
// safe for concurrent use.
var promptParsers = sync.Pool{New: func() any {
	p := ts.NewParser()
	if err := p.SetLanguage(promptLanguage); err != nil {
		panic(fmt.Sprintf("prose: the tree-sitter-tldr prompt grammar does not load: %v", err))
	}
	return p
}}

// entrySpan is one entry of a prompt's text: the byte span of its one line
// (its newline included), its label, and where what it says begins (after the
// label and any separator).
type entrySpan struct {
	start, end int
	label      string
	textStart  int
}

// ParsePrompt parses a PROMPT's TEXT into the ParsedProse the webapp draws:
// every run of entries answering labels (`3.1.2 - my answer`) as one entry
// tree (entryTree), and the text around the runs exactly as Parse reads it,
// trees and all. A prompt is settled text. Its source is the text as written:
// a prompt's labels are the person's, and nothing renumbers them.
//
// A failure is a bug, recorded on LOG once and then a panic, as Parse's is.
func ParsePrompt(log dlog.Logger, text string) (parsed *frontendv1.ParsedProse) {
	defer func() {
		if r := recover(); r != nil {
			log.Error(invariantOperation, "the daemon could not parse a prompt it serves", dlog.Context{
				"function": "ParsePrompt",
				"length":   len(text),
				"cause":    fmt.Sprint(r),
			})
			panic(r)
		}
	}()
	return parsePromptText(text)
}

// parsePromptText is the parse ParsePrompt records the failures of; a test
// replaces it to reach ParsePrompt's failure path.
var parsePromptText = parsePrompt

// parsePrompt is ParsePrompt without the record: a failure panics.
func parsePrompt(text string) *frontendv1.ParsedProse {
	entries, err := readEntries(text)
	if err != nil {
		panic(err.Error())
	}
	if len(entries) == 0 {
		parsed := parse(text, Settled)
		parsed.Source = &frontendv1.ProseSource{Markdown: text}
		return parsed
	}
	out := &frontendv1.ParsedProse{Source: &frontendv1.ProseSource{Markdown: text}}
	at := 0
	for _, run := range entryRuns(text, entries) {
		if start := run[0].start; start > at {
			out.Blocks = append(out.Blocks, parse(text[at:start], Settled).Blocks...)
		}
		out.Blocks = append(out.Blocks, &frontendv1.ProseBlock{Block: &frontendv1.ProseBlock_Entries{Entries: entryTree(text, run)}})
		at = run[len(run)-1].end
	}
	if at < len(text) {
		out.Blocks = append(out.Blocks, parse(text[at:], Settled).Blocks...)
	}
	return out
}

// entryRuns groups ENTRIES of TEXT into runs: entries with nothing but blank
// lines between them. Any other line between two entries ends a run.
func entryRuns(text string, entries []entrySpan) [][]entrySpan {
	var runs [][]entrySpan
	for i, e := range entries {
		if i > 0 && strings.TrimSpace(text[entries[i-1].end:e.start]) == "" {
			runs[len(runs)-1] = append(runs[len(runs)-1], e)
			continue
		}
		runs = append(runs, []entrySpan{e})
	}
	return runs
}

// entrySays is what entry E of TEXT says: the words on its line after its
// label and separator.
func entrySays(text string, e entrySpan) string {
	return strings.TrimSpace(text[e.textStart:e.end])
}

// entryNode is one node of an entry tree while it is built.
type entryNode struct {
	label    string
	answer   bool
	says     string
	children []int
}

// entryTree is RUN, a run of TEXT's entries, as the tree its labels make
// (frontend.v1.ProseEntryTree): every entry a node, and every ancestor its
// dotted label implies that no entry answers a path node. Nodes and siblings
// keep the order their first entry appeared in; two entries answering one
// label are two sibling nodes, the descendants of either under the first.
// The nodes are served in reading order.
func entryTree(text string, run []entrySpan) *frontendv1.ProseEntryTree {
	var nodes []entryNode
	var roots []int
	first := map[string]int{}
	attach := func(parent int, n entryNode) int {
		nodes = append(nodes, n)
		at := len(nodes) - 1
		if parent < 0 {
			roots = append(roots, at)
		} else {
			nodes[parent].children = append(nodes[parent].children, at)
		}
		return at
	}
	for _, e := range run {
		parent := -1
		parts := strings.Split(e.label, ".")
		for k := 1; k < len(parts); k++ {
			prefix := strings.Join(parts[:k], ".")
			at, ok := first[prefix]
			if !ok {
				at = attach(parent, entryNode{label: prefix})
				first[prefix] = at
			}
			parent = at
		}
		says := entrySays(text, e)
		if at, ok := first[e.label]; ok && !nodes[at].answer {
			nodes[at].answer, nodes[at].says = true, says
			continue
		}
		at := attach(parent, entryNode{label: e.label, answer: true, says: says})
		if _, ok := first[e.label]; !ok {
			first[e.label] = at
		}
	}
	tree := &frontendv1.ProseEntryTree{}
	var emit func(at, parent int)
	emit = func(at, parent int) {
		n := nodes[at]
		id := uint32(len(tree.Nodes))
		node := &frontendv1.ProseEntryTreeNode{
			Id:    &frontendv1.ProseTreeNodeId{Index: id},
			Label: &frontendv1.ProseEntryLabel{Text: n.label},
		}
		if parent < 0 {
			node.Place = &frontendv1.ProseEntryTreeNode_Root{Root: &frontendv1.ProseEntryTreeNodeRoot{}}
		} else {
			node.Place = &frontendv1.ProseEntryTreeNode_Branch{Branch: &frontendv1.ProseEntryTreeNodeBranch{
				Parent: &frontendv1.ProseTreeNodeId{Index: uint32(parent)},
			}}
		}
		if n.answer {
			node.Kind = &frontendv1.ProseEntryTreeNode_Answer{Answer: &frontendv1.ProseEntryTreeNodeAnswer{
				Text: &frontendv1.ProseEntryText{Markdown: n.says},
			}}
		} else {
			node.Kind = &frontendv1.ProseEntryTreeNode_Path{Path: &frontendv1.ProseEntryTreeNodePath{}}
		}
		tree.Nodes = append(tree.Nodes, node)
		for _, c := range n.children {
			emit(c, int(id))
		}
	}
	for _, r := range roots {
		emit(r, -1)
	}
	return tree
}

// readEntries answers TEXT's entries, in order: the heading line of every
// section of the prompt grammar's parse whose heading carries no marker. An
// entry is that one line: the section's lines after it are the prompt's own
// prose. A heading with a marker
// (`1 🔧 text`) opens a pasted tree's root, which is the tree's, not an entry.
// A parse the grammar could not complete is an error: it accepts every text
// whole by design.
func readEntries(text string) ([]entrySpan, error) {
	p := promptParsers.Get().(*ts.Parser)
	defer promptParsers.Put(p)
	src := []byte(text)
	tree := p.Parse(src, nil)
	if tree == nil {
		return nil, fmt.Errorf("the prompt grammar returned no tree for a %d-byte text", len(text))
	}
	defer tree.Close()
	root := tree.RootNode()
	if root.HasError() {
		sexp := root.ToSexp()
		if len(sexp) > 500 {
			sexp = sexp[:500]
		}
		return nil, fmt.Errorf("the prompt grammar could not parse a %d-byte text, which it accepts whole by design: %s", len(text), sexp)
	}
	var entries []entrySpan
	for i := uint(0); i < root.NamedChildCount(); i++ {
		section := root.NamedChild(i)
		if section.Kind() != "section" {
			continue
		}
		heading := childOfKind(section, "heading")
		if heading == nil {
			return nil, fmt.Errorf("a section of the prompt grammar's parse has no heading: %s", section.ToSexp())
		}
		if childOfKind(heading, "marker") != nil {
			continue
		}
		label := childOfKind(heading, "label")
		if label == nil {
			return nil, fmt.Errorf("a heading of the prompt grammar's parse has no label: %s", heading.ToSexp())
		}
		textStart := int(label.EndByte())
		if sep := childOfKind(heading, "separator"); sep != nil {
			textStart = int(sep.EndByte())
		}
		entries = append(entries, entrySpan{
			start:     int(heading.StartByte()),
			end:       int(heading.EndByte()),
			label:     strings.TrimSpace(text[label.StartByte():label.EndByte()]),
			textStart: textStart,
		})
	}
	return entries, nil
}

// ReplaceEntry answers TEXT with the first entry answering LABEL saying
// SAYS instead: its label, its separator and the whitespace after them kept,
// and every other word of TEXT unchanged. ok is false, and TEXT is answered
// unchanged, when no entry of TEXT answers LABEL.
func ReplaceEntry(text, label, says string) (string, bool) {
	entries, err := readEntries(text)
	if err != nil {
		panic(err.Error())
	}
	for _, e := range entries {
		if e.label != label {
			continue
		}
		content := text[e.textStart:e.end]
		lead := content[:len(content)-len(strings.TrimLeft(content, " \t"))]
		trail := content[len(strings.TrimRight(content, "\n")):]
		if lead == "" && says != "" {
			lead = " "
		}
		return text[:e.textStart] + lead + says + trail + text[e.end:], true
	}
	return text, false
}
