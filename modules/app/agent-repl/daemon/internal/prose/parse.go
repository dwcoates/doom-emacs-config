package prose

import (
	"fmt"
	"strings"

	"agent-repld/internal/dlog"

	frontendv1 "agentrepl/proto/frontend/v1"
)

// invariantOperation is the operation an invariant violation in this package
// is recorded under before it panics.
const invariantOperation = "daemon.prose.invariant"

// Stage is whether a text is still arriving or has settled. It is the
// caller's to say: a streaming response's text can end on a newline while more
// is still coming, so the text itself cannot tell.
type Stage int

const (
	// Settled is a text that will not grow: every decision about it is final.
	Settled Stage = iota
	// Arriving is a response still streaming in. Whatever only a later line can
	// decide is left undecided, so a later push only appends to what this one
	// served (ProseTree's guarantee).
	Arriving
)

// Parse parses TEXT, at STAGE, into the ParsedProse the webapp draws: runs of
// plain markdown, and each tree as a graph whose labels are derived from its
// place.
//
// The grammar accepts every text whole, and every tree this package builds
// keeps the guarantees ProseTree documents, so a failure here is a bug: it is
// recorded on LOG once, with the text's length and the cause, and then panics.
func Parse(log dlog.Logger, text string, stage Stage) (parsed *frontendv1.ParsedProse) {
	defer func() {
		if r := recover(); r != nil {
			log.Error(invariantOperation, "the daemon could not parse a text it serves", dlog.Context{
				"function": "Parse",
				"length":   len(text),
				"arriving": stage == Arriving,
				"cause":    fmt.Sprint(r),
			})
			panic(r)
		}
	}()
	return parseText(text, stage)
}

// parseText is the parse Parse records the failures of; a test replaces it to
// reach Parse's failure path, which a correct parse never takes.
var parseText = parse

// parse is Parse without the record: a failure panics.
func parse(text string, stage Stage) *frontendv1.ParsedProse {
	d := mustRead(text)
	// Labelling a root can let the grammar see a tree that only now begins or
	// continues, and so a further unlabelled root under it; each round repairs
	// at least one line, so the rounds end.
	for {
		edits := repairUnlabeledRoots(d)
		if len(edits) == 0 {
			break
		}
		text = apply(text, edits)
		d = mustRead(text)
	}

	out := &frontendv1.ParsedProse{}
	source := text
	if d.region == nil {
		out.Blocks = plainBlocks(d, 0, len(d.items)-1, stage)
	} else {
		tree, edits := buildTree(d, stage)
		mustKeepGuarantees(tree)
		out.Blocks = append(out.Blocks, plainBlocks(d, 0, d.region.first-1, stage)...)
		out.Blocks = append(out.Blocks, &frontendv1.ProseBlock{Block: &frontendv1.ProseBlock_Tree{Tree: tree}})
		out.Blocks = append(out.Blocks, plainBlocks(d, d.region.last+1, len(d.items)-1, stage)...)
		source = apply(text, edits)
	}
	out.Source = &frontendv1.ProseSource{Markdown: source}
	return out
}

func mustRead(text string) document {
	d, err := read(text)
	if err != nil {
		panic(err.Error())
	}
	return d
}

// plainBlocks is D's items FIRST through LAST, inclusive, as blocks: runs of
// plain markdown, split around every closed, language-less fence whose body is
// a metaprompt tree, which becomes that body's own blocks. A run of only blank
// lines is not served.
//
// An UNCLOSED fence is never read as a tree: it can only be the text's last
// item, a code block still arriving or one the model never closed. While a
// response is ARRIVING, neither is a closed fence that nothing but blank lines
// follows yet: the next line decides whether it is nested under the tree above
// it (a branch follows) or stands after it (anything else follows), and
// reading its body as a tree of its own would draw a tree that vanishes on the
// push that brings that line.
func plainBlocks(d document, first, last int, stage Stage) []*frontendv1.ProseBlock {
	var out []*frontendv1.ProseBlock
	runStart := -1
	flush := func(end int) {
		if runStart < 0 {
			return
		}
		run := strings.TrimSuffix(d.text[d.items[runStart].start:end], "\n")
		runStart = -1
		if strings.TrimSpace(run) == "" {
			return
		}
		out = append(out, &frontendv1.ProseBlock{Block: &frontendv1.ProseBlock_Plain{
			Plain: &frontendv1.ProsePlain{Markdown: run},
		}})
	}
	for i := first; i <= last; i++ {
		it := d.items[i]
		undecided := stage == Arriving && fenceUndecided(d.items, i)
		if it.fence != nil && it.fence.closed && !undecided && it.fence.lang == "" {
			body := strings.Join(it.fence.code, "\n")
			inner := mustRead(body)
			if inner.region != nil && isMetapromptTree(inner) {
				flush(it.start)
				out = append(out, parse(body, Settled).Blocks...)
				continue
			}
		}
		if runStart < 0 {
			runStart = i
		}
	}
	if runStart >= 0 {
		flush(d.items[last].end)
	}
	return out
}

// fenceUndecided is whether the fence at ITEMS[I] cannot yet be placed while
// its text is still arriving: nothing but blank lines follows it, or the only
// line that follows is one a later branch would make a root (repair.go), which
// would make the fence a code block nested under the tree above it.
func fenceUndecided(items []item, i int) bool {
	next := nextNonBlank(items, i+1)
	if next < 0 {
		return true
	}
	l := items[next].line
	return l != nil && l.class == classOther && opensOnMarker(l.text) && nextNonBlank(items, next+1) < 0
}

// mustKeepGuarantees checks the guarantees ProseTree documents: nodes in
// reading order, each id its index, each parent an earlier node, and each
// label its parent's label and its place among its siblings.
func mustKeepGuarantees(tree *frontendv1.ProseTree) {
	children := map[uint32]int{}
	roots := 0
	for i, n := range tree.GetNodes() {
		if n.GetId().GetIndex() != uint32(i) {
			panic(fmt.Sprintf("prose: node %d carries id %d", i, n.GetId().GetIndex()))
		}
		var want string
		switch place := n.GetPlace().(type) {
		case *frontendv1.ProseTreeNode_Root:
			roots++
			want = fmt.Sprint(roots)
		case *frontendv1.ProseTreeNode_Branch:
			parent := place.Branch.GetParent().GetIndex()
			if parent >= uint32(i) {
				panic(fmt.Sprintf("prose: node %d names parent %d, which is not earlier", i, parent))
			}
			children[parent]++
			want = fmt.Sprintf("%s.%d", tree.GetNodes()[parent].GetLabel().GetText(), children[parent])
		default:
			panic(fmt.Sprintf("prose: node %d has no place", i))
		}
		if got := n.GetLabel().GetText(); got != want {
			panic(fmt.Sprintf("prose: node %d is labelled %q, want %q", i, got, want))
		}
	}
}
