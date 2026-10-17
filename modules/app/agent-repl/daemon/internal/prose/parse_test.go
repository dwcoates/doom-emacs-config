package prose

import (
	"fmt"
	"reflect"
	"strings"
	"testing"

	"agent-repld/internal/dlog"

	frontendv1 "agentrepl/proto/frontend/v1"
)

// shape is a parsed text in a form a test can state: each block on its own
// line, a tree's header and nodes indented beneath it. A node reads
// `label [marker] ^parent text`, with `^-` for a root, then `  code <lang>:
// <body>` for each code block.
func shape(p *frontendv1.ParsedProse) string {
	var b strings.Builder
	for _, block := range p.GetBlocks() {
		if plain := block.GetPlain(); plain != nil {
			fmt.Fprintf(&b, "plain %q\n", plain.GetMarkdown())
			continue
		}
		tree := block.GetTree()
		b.WriteString("tree\n")
		if h := tree.Header; h != nil {
			fmt.Fprintf(&b, "  header %q\n", h.GetText())
		}
		for _, n := range tree.GetNodes() {
			parent, marker := "-", ""
			if br := n.GetBranch(); br != nil {
				parent = fmt.Sprint(br.GetParent().GetIndex())
			} else if m := n.GetRoot().Marker; m != nil {
				marker = " " + m.GetText()
			}
			fmt.Fprintf(&b, "  %s%s ^%s %q\n", n.GetLabel().GetText(), marker, parent, n.GetText().GetMarkdown())
			for _, c := range n.GetCode() {
				fmt.Fprintf(&b, "    code %s: %q\n", c.GetLanguage(), c.GetBody())
			}
		}
	}
	return b.String()
}

func lines(ls ...string) string { return strings.Join(ls, "\n") }

func TestParseRepairsAndShapesTrees(t *testing.T) {
	tests := []struct {
		name  string
		text  string
		stage Stage
		want  string
	}{
		{
			name: "a clean tree keeps its labels",
			text: lines("1 🔧 Root", "├── 1.1 A", "│   └── 1.1.1 B", "└── 1.2 C"),
			want: lines("tree", `  1 🔧 ^- "Root"`, `  1.1 ^0 "A"`, `  1.1.1 ^1 "B"`, `  1.2 ^0 "C"`, ""),
		},
		{
			name: "branches without labels get the labels of their place, nested",
			text: lines(
				"5 🔁 Repeatedly starting the test services",
				"└── e2e creates 127 worlds",
				"    ├── Each NewWorld() starts a store",
				"    └── Shims are started per session",
				"        └── The code is world_test.go:219",
			),
			want: lines("tree",
				`  1 🔁 ^- "Repeatedly starting the test services"`,
				`  1.1 ^0 "e2e creates 127 worlds"`,
				`  1.1.1 ^1 "Each NewWorld() starts a store"`,
				`  1.1.2 ^1 "Shims are started per session"`,
				`  1.1.2.1 ^3 "The code is world_test.go:219"`, ""),
		},
		{
			name: "a wrong label is replaced by the label of its place",
			text: lines("1 🔧 Root", "├── Unlabelled", "└── 1.1 Labelled first by the model"),
			want: lines("tree", `  1 🔧 ^- "Root"`, `  1.1 ^0 "Unlabelled"`, `  1.2 ^0 "Labelled first by the model"`, ""),
		},
		{
			name: "duplicated labels become distinct",
			text: lines("1 🔧 Root", "├── 1.1 A", "└── 1.1 B"),
			want: lines("tree", `  1 🔧 ^- "Root"`, `  1.1 ^0 "A"`, `  1.2 ^0 "B"`, ""),
		},
		{
			name: "skipped root numbers are counted by position",
			text: lines("1 🔧 A", "└── 1.1 a", "", "2 ✅ B", "└── 2.1 b", "", "4 ❓ D", "└── 4.1 d"),
			want: lines("tree", `  1 🔧 ^- "A"`, `  1.1 ^0 "a"`, `  2 ✅ ^- "B"`, `  2.1 ^2 "b"`, `  3 ❓ ^- "D"`, `  3.1 ^4 "d"`, ""),
		},
		{
			name: "a branch's label counts within its own parent",
			text: lines("1 🔧 A", "├── 1.1 a", "│   └── 1.1.1 x", "└── 1.2 b", "    └── 1.2.1 y"),
			want: lines("tree", `  1 🔧 ^- "A"`, `  1.1 ^0 "a"`, `  1.1.1 ^1 "x"`, `  1.2 ^0 "b"`, `  1.2.1 ^3 "y"`, ""),
		},
		{
			name: "a root without a label becomes a root when a branch follows it",
			text: lines("1 🔧 A", "└── 1.1 a", "", "🔁 B", "└── b"),
			want: lines("tree", `  1 🔧 ^- "A"`, `  1.1 ^0 "a"`, `  2 🔁 ^- "B"`, `  2.1 ^2 "b"`, ""),
		},
		{
			name: "the first root without a label starts the tree",
			text: lines("🔧 A", "├── a", "└── b"),
			want: lines("tree", `  1 🔧 ^- "A"`, `  1.1 ^0 "a"`, `  1.2 ^0 "b"`, ""),
		},
		{
			name: "a marker line no branch follows stays prose",
			text: lines("1 🔧 A", "└── 1.1 a", "", "🎉 Done"),
			want: lines("tree", `  1 🔧 ^- "A"`, `  1.1 ^0 "a"`, `plain "\n🎉 Done"`, ""),
		},
		{
			name: "a rail line before a branch is not a root without a label",
			text: lines("1 🔧 A", "├── 1.1 a", "│   wrapped", "│   └── 1.1.1 b"),
			want: lines("tree", `  1 🔧 ^- "A"`, `  1.1 ^0 "a"`, `plain "│   wrapped\n│   └── 1.1.1 b"`, ""),
		},
		{
			name: "a branch with no root above it is a root without a marker",
			text: lines("├── 1.1 Detail", "└── 1.2 More"),
			want: lines("tree", `  1 ^- "Detail"`, `  2 ^- "More"`, ""),
		},
		{
			name: "a branch deeper than its parent's child hangs under the nearest shallower node",
			text: lines("1 🔧 A", "│   │   └── too deep", "└── 1.2 b"),
			want: lines("tree", `  1 🔧 ^- "A"`, `  1.1 ^0 "too deep"`, `  1.2 ^0 "b"`, ""),
		},
		{
			name: "a branch shallower than the one above it returns to its own level",
			text: lines("1 🔧 A", "├── a", "│   └── a1", "└── b"),
			want: lines("tree", `  1 🔧 ^- "A"`, `  1.1 ^0 "a"`, `  1.1.1 ^1 "a1"`, `  1.2 ^0 "b"`, ""),
		},
		{
			name: "a line drawn with ASCII connectors ends the tree, as it always did",
			text: lines("1 🔧 A", "├── a", "|-- b"),
			want: lines("tree", `  1 🔧 ^- "A"`, `  1.1 ^0 "a"`, `plain "|-- b"`, ""),
		},
		{
			name: "a branch beside a root-less branch is another root",
			text: lines("├── a", "│   └── a1", "└── b"),
			want: lines("tree", `  1 ^- "a"`, `  1.1 ^0 "a1"`, `  2 ^- "b"`, ""),
		},
		{
			name: "a stray indent before a connector does not change its depth",
			text: lines("1 🔧 A", " ├── a", "  └── b"),
			want: lines("tree", `  1 🔧 ^- "A"`, `  1.1 ^0 "a"`, `  1.2 ^0 "b"`, ""),
		},
		{
			name: "a connector with no space after it is still a branch",
			text: lines("1 🔧 A", "├──squashed", "└── b"),
			want: lines("tree", `  1 🔧 ^- "A"`, `  1.1 ^0 "squashed"`, `  1.2 ^0 "b"`, ""),
		},
		{
			name: "the header line heads the tree",
			text: lines("Response (✏️ changes made)", "", "1 🔧 A", "└── 1.1 a"),
			want: lines("tree", `  header "Response (✏️ changes made)"`, `  1 🔧 ^- "A"`, `  1.1 ^0 "a"`, ""),
		},
		{
			name: "a fence beneath a branch belongs to that branch",
			text: lines("1 🔧 A", "├── 1.1 a", "│   ```go", "│   x := 1", "│   ```", "└── 1.2 b"),
			want: lines("tree", `  1 🔧 ^- "A"`, `  1.1 ^0 "a"`, `    code go: "x := 1"`, `  1.2 ^0 "b"`, ""),
		},
		{
			name: "a fence with no language carries none",
			text: lines("1 🔧 A", "├── 1.1 a", "    ```", "    plain", "    ```", "└── 1.2 b"),
			want: lines("tree", `  1 🔧 ^- "A"`, `  1.1 ^0 "a"`, `    code : "plain"`, `  1.2 ^0 "b"`, ""),
		},
		{
			name: "prose around the tree is plain markdown",
			text: lines("Some preamble.", "", "1 🔧 A", "└── 1.1 a", "", "A closing note."),
			want: lines(`plain "Some preamble.\n"`, "tree", `  1 🔧 ^- "A"`, `  1.1 ^0 "a"`, `plain "\nA closing note."`, ""),
		},
		{
			name: "a text with no tree is one plain block",
			text: "Just prose.\nTwo lines.",
			want: lines(`plain "Just prose.\nTwo lines."`, ""),
		},
		{
			name: "an empty text has no blocks",
			text: "",
			want: "",
		},
		{
			name: "a language-less fence holding a tree is drawn as a tree",
			text: lines("> quoted:", "```", "1 🔧 A", "└── 1.1 a", "```", "my reply"),
			want: lines(`plain "> quoted:"`, "tree", `  1 🔧 ^- "A"`, `  1.1 ^0 "a"`, `plain "my reply"`, ""),
		},
		{
			name: "an unclosed fence is code, even when it holds a tree",
			text: lines("```", "1 🔧 A", "└── 1.1 a"),
			want: lines(`plain "`+"```\\n1 🔧 A\\n└── 1.1 a"+`"`, ""),
		},
		{
			name:  "while a response arrives, a closed fence nothing follows yet is code",
			text:  lines("1 🔧 A", "```", "1 🔧 B", "└── 1.1 b", "```", ""),
			stage: Arriving,
			want:  lines(`plain "1 🔧 A\n`+"```\\n1 🔧 B\\n└── 1.1 b\\n```"+`"`, ""),
		},
		{
			name:  "while a response arrives, a closed fence a lone marker line follows is code",
			text:  lines("1 🔧 A", "├── 1.1 a", "```", "1 🔧 B", "└── 1.1 b", "```", "🔁 C", ""),
			stage: Arriving,
			want:  lines("tree", `  1 🔧 ^- "A"`, `  1.1 ^0 "a"`, `plain "`+"```\\n1 🔧 B\\n└── 1.1 b\\n```\\n🔁 C"+`"`, ""),
		},
		{
			name: "once settled, that closed fence's tree is drawn",
			text: lines("1 🔧 A", "```", "1 🔧 B", "└── 1.1 b", "```", ""),
			want: lines(`plain "1 🔧 A"`, "tree", `  1 🔧 ^- "B"`, `  1.1 ^0 "b"`, ""),
		},
		{
			name: "a fence with a language is code, even when it holds a tree",
			text: lines("```text", "1 🔧 A", "└── 1.1 a", "```"),
			want: lines(`plain "`+"```text\\n1 🔧 A\\n└── 1.1 a\\n```"+`"`, ""),
		},
		{
			name: "a marker with a variation selector is one marker",
			text: lines("1 ✏️ Edited", "└── 1.1 a"),
			want: lines("tree", `  1 ✏️ ^- "Edited"`, `  1.1 ^0 "a"`, ""),
		},
		{
			name: "a zero-width-joined emoji is one marker",
			text: lines("1 🧑‍💻 Coded", "└── 1.1 a"),
			want: lines("tree", `  1 🧑‍💻 ^- "Coded"`, `  1.1 ^0 "a"`, ""),
		},
		{
			name:  "the label the model is still typing on the last line is not drawn",
			text:  lines("1 🔧 A", "├── 1.1 a", "└── 1"),
			stage: Arriving,
			want:  lines("tree", `  1 🔧 ^- "A"`, `  1.1 ^0 "a"`, `  1.2 ^0 ""`, ""),
		},
		{
			name: "digits ending a settled text are the branch's text",
			text: lines("1 🔧 A", "├── 1.1 a", "└── 2026"),
			want: lines("tree", `  1 🔧 ^- "A"`, `  1.1 ^0 "a"`, `  1.2 ^0 "2026"`, ""),
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := shape(Parse(dlog.NewTestLogger(), tt.text, tt.stage))

			// Assert.
			if got != tt.want {
				t.Errorf("Parse(%q) =\n%s\nwant\n%s", tt.text, got, tt.want)
			}
		})
	}
}

func TestParseRepairsLabelsInTheSource(t *testing.T) {
	tests := []struct {
		name string
		text string
		want string
	}{
		{
			name: "a clean tree's source is unchanged",
			text: lines("1 🔧 A", "└── 1.1 a"),
			want: lines("1 🔧 A", "└── 1.1 a"),
		},
		{
			name: "missing labels are written in after the connector",
			text: lines("1 🔧 A", "├── a", "│   └── a1", "└── b"),
			want: lines("1 🔧 A", "├── 1.1 a", "│   └── 1.1.1 a1", "└── 1.2 b"),
		},
		{
			name: "wrong labels are rewritten in place",
			text: lines("4 🔧 A", "├── 4.3 a", "└── 9 b"),
			want: lines("1 🔧 A", "├── 1.1 a", "└── 1.2 b"),
		},
		{
			name: "an unlabelled root gets its label",
			text: lines("🔧 A", "└── a"),
			want: lines("1 🔧 A", "└── 1.1 a"),
		},
		{
			name: "prose around the tree is left as written",
			text: lines("See 3.2 below.", "", "1 🔧 A", "└── a", "", "1. a list item"),
			want: lines("See 3.2 below.", "", "1 🔧 A", "└── 1.1 a", "", "1. a list item"),
		},
		{
			name: "a text with no tree is unchanged",
			text: "├── one stray connector",
			want: "├── one stray connector",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := Parse(dlog.NewTestLogger(), tt.text, Settled).GetSource().GetMarkdown()

			// Assert.
			if got != tt.want {
				t.Errorf("Parse(%q).Source = %q, want %q", tt.text, got, tt.want)
			}
		})
	}
}

// nodeFacts is what a served node promises never to change while the response
// streams in: its id, label and parent.
func nodeFacts(p *frontendv1.ParsedProse) []string {
	var out []string
	for _, block := range p.GetBlocks() {
		for _, n := range block.GetTree().GetNodes() {
			out = append(out, fmt.Sprintf("%d %s %d %v", n.GetId().GetIndex(), n.GetLabel().GetText(), n.GetBranch().GetParent().GetIndex(), n.GetRoot() != nil))
		}
	}
	return out
}

func TestParseNeverChangesAServedNodeWhileTheResponseStreams(t *testing.T) {
	// Arrange: every golden well-formed response, cut after each of its lines,
	// as a streaming response arrives.
	g := loadGolden(t)
	checked := 0
	for i, c := range g.Cases {
		if !strings.HasPrefix(c.Text, "Response (") {
			continue
		}
		full := Parse(dlog.NewTestLogger(), c.Text, Settled)
		if len(full.GetBlocks()) == 0 {
			continue
		}
		for cut := strings.Index(c.Text, "\n"); cut >= 0; {
			// Act.
			prefix := Parse(dlog.NewTestLogger(), c.Text[:cut+1], Arriving)

			// Assert: the prefix's nodes are a prefix of the whole text's nodes,
			// whenever both parse the same tree.
			got, want := nodeFacts(prefix), nodeFacts(full)
			if len(got) <= len(want) && len(got) > 0 && !reflect.DeepEqual(got, want[:len(got)]) {
				t.Errorf("case %d cut at %d: nodes %v are not a prefix of %v", i, cut, got, want)
			}
			next := strings.Index(c.Text[cut+1:], "\n")
			if next < 0 {
				break
			}
			cut += next + 1
			checked++
		}
	}
	if checked < 1000 {
		t.Fatalf("checked %d cuts, want at least 1000: the corpus no longer exercises streaming", checked)
	}
}

func TestParseRecordsAFailureOnceAndPanics(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	defer func(saved func(string, Stage) *frontendv1.ParsedProse) { parseText = saved }(parseText)
	parseText = func(string, Stage) *frontendv1.ParsedProse { panic("prose: a broken guarantee") }

	// Act.
	recovered := func() (r any) {
		defer func() { r = recover() }()
		Parse(log, "1 🔧 A", Arriving)
		return nil
	}()

	// Assert.
	if recovered != "prose: a broken guarantee" {
		t.Fatalf("Parse recovered %v, want the parse's own panic re-raised", recovered)
	}
	errors := dlog.FilterRecords(log.Records(), dlog.RecordQuery{Level: "error", Operation: invariantOperation})
	if len(errors) != 1 {
		t.Fatalf("Parse recorded %d errors under %s, want 1: %+v", len(errors), invariantOperation, log.Records())
	}
	if ctx := errors[0].Context; ctx["length"] != len("1 🔧 A") || ctx["arriving"] != true || ctx["cause"] != "prose: a broken guarantee" {
		t.Errorf("the record's context = %v, want the text's length and the cause", ctx)
	}
}

func TestMustKeepGuaranteesRefusesABrokenTree(t *testing.T) {
	root := func(i uint32, label string) *frontendv1.ProseTreeNode {
		return &frontendv1.ProseTreeNode{
			Id:    &frontendv1.ProseTreeNodeId{Index: i},
			Place: &frontendv1.ProseTreeNode_Root{Root: &frontendv1.ProseTreeNodeRoot{}},
			Label: &frontendv1.ProseTreeNodeLabel{Text: label},
		}
	}
	branch := func(i, parent uint32, label string) *frontendv1.ProseTreeNode {
		return &frontendv1.ProseTreeNode{
			Id:    &frontendv1.ProseTreeNodeId{Index: i},
			Place: &frontendv1.ProseTreeNode_Branch{Branch: &frontendv1.ProseTreeNodeBranch{Parent: &frontendv1.ProseTreeNodeId{Index: parent}}},
			Label: &frontendv1.ProseTreeNodeLabel{Text: label},
		}
	}
	tests := []struct {
		name  string
		nodes []*frontendv1.ProseTreeNode
	}{
		{name: "an id that is not its index", nodes: []*frontendv1.ProseTreeNode{root(1, "1")}},
		{name: "a parent that is not earlier", nodes: []*frontendv1.ProseTreeNode{root(0, "1"), branch(1, 1, "1.1")}},
		{name: "a label that is not its place", nodes: []*frontendv1.ProseTreeNode{root(0, "1"), branch(1, 0, "1.2")}},
		{name: "a node with no place", nodes: []*frontendv1.ProseTreeNode{{Id: &frontendv1.ProseTreeNodeId{}, Label: &frontendv1.ProseTreeNodeLabel{Text: "1"}}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			recovered := func() (r any) {
				defer func() { r = recover() }()
				mustKeepGuarantees(&frontendv1.ProseTree{Nodes: tt.nodes})
				return nil
			}()

			// Assert.
			if recovered == nil {
				t.Fatal("mustKeepGuarantees accepted a broken tree")
			}
		})
	}
}
