package prose

import (
	"encoding/json"
	"os"
	"reflect"
	"strings"
	"testing"
	"unicode/utf8"

	"agent-repld/internal/mdfence"
)

// The old webapp line parser's own answers, recorded before the grammar
// replaced it (tree-sitter-tldr/tldr/test/golden/generate.mjs). read.go is
// held to every one of them.
const goldenPath = "../../../tree-sitter-tldr/tldr/test/golden/golden.json"

type goldenRegion struct {
	Top   int `json:"top"`
	Count int `json:"count"`
}

type goldenSegment struct {
	Kind   string   `json:"kind"`
	Count  int      `json:"count"`
	Lang   string   `json:"lang"`
	Code   []string `json:"code"`
	Closed bool     `json:"closed"`
}

type goldenCase struct {
	Text                  string          `json:"text"`
	Region                *goldenRegion   `json:"region"`
	IsMetapromptTree      bool            `json:"isMetapromptTree"`
	LooksLikeIntendedTree bool            `json:"looksLikeIntendedTree"`
	Segments              []goldenSegment `json:"segments"`
}

type goldenBranch struct {
	Indent string `json:"indent"`
	Prefix string `json:"prefix"`
	Label  string `json:"label"`
	Body   string `json:"body"`
}

type goldenContinuation struct {
	Column int    `json:"column"`
	Text   string `json:"text"`
}

type goldenLine struct {
	Connector    bool                `json:"connector"`
	Root         bool                `json:"root"`
	Dotted       bool                `json:"dotted"`
	Header       bool                `json:"header"`
	Fence        bool                `json:"fence"`
	Branch       *goldenBranch       `json:"branch"`
	Continuation *goldenContinuation `json:"continuation"`
}

type goldenFile struct {
	Cases []goldenCase          `json:"cases"`
	Lines map[string]goldenLine `json:"lines"`
}

func loadGolden(t *testing.T) goldenFile {
	t.Helper()
	raw, err := os.ReadFile(goldenPath)
	if err != nil {
		t.Fatalf("read %s: %v", goldenPath, err)
	}
	var g goldenFile
	if err := json.Unmarshal(raw, &g); err != nil {
		t.Fatalf("decode %s: %v", goldenPath, err)
	}
	return g
}

// segment is one run of tree lines or one fence, as splitTreeSegments made
// them: the comparable form of both the golden answer and read's.
type segment struct {
	Kind   string
	Lines  []string
	Lang   string
	Code   []string
	Closed bool
}

func expectedSegments(c goldenCase) []segment {
	lines := strings.Split(c.Text, "\n")
	i := 0
	var out []segment
	for _, s := range c.Segments {
		if s.Kind == "tree" {
			out = append(out, segment{Kind: "tree", Lines: lines[i : i+s.Count]})
			i += s.Count
			continue
		}
		i += 1 + len(s.Code)
		if s.Closed {
			i++
		}
		out = append(out, segment{Kind: "fence", Lang: s.Lang, Code: nonNil(s.Code), Closed: s.Closed})
	}
	return out
}

func actualSegments(d document) []segment {
	var out []segment
	var run []string
	flush := func() {
		if len(run) > 0 {
			out = append(out, segment{Kind: "tree", Lines: run})
			run = nil
		}
	}
	for _, it := range d.items {
		if it.line != nil {
			run = append(run, it.line.text)
			continue
		}
		flush()
		out = append(out, segment{Kind: "fence", Lang: it.fence.lang, Code: nonNil(it.fence.code), Closed: it.fence.closed})
	}
	flush()
	return out
}

func nonNil(s []string) []string {
	if s == nil {
		return []string{}
	}
	return s
}

// fenceRole is what a line is to the fences around it.
type fenceRole int

const (
	fenceNone fenceRole = iota
	fenceOpens
	fenceCloses
)

// railedTicks is the backtick run after LINE's leading rail of spaces and │
// (the scanner's fence_ticks), and whether nothing but whitespace follows it.
func railedTicks(line string) (ticks int, bare bool) {
	rest := strings.TrimLeft(line, " │")
	run := strings.TrimLeft(rest, "`")
	return len(rest) - len(run), strings.TrimSpace(run) == ""
}

// departsFromLengthRule is whether the old parser placed TEXT's fence
// delimiters other than markdown's length rule does (2026-11-10). The old
// parser toggled a fence at every line of three backticks or more, so a
// delimiter carrying a language, or a shorter run, closed the open fence;
// the grammar's scanner now closes a fence only on a bare run at least as
// long as the one that opened it, so such a line is the fence's content.
// And the old parser read an open's backticks past the third as its
// language, where the scanner reads the whole run as the delimiter. The old
// parser's answer for such a text is no longer the grammar's, and
// the length rule's own cases hold the grammar's answer instead
// (tree-sitter-tldr/tldr/test/corpus/fence.txt, mdfence's tests).
func departsFromLengthRule(text string) bool {
	oldOpen, open := false, 0
	for _, line := range strings.Split(text, "\n") {
		ticks, bare := railedTicks(line)
		oldRole, role := fenceNone, fenceNone
		if ticks >= mdfence.Min {
			oldRole = fenceOpens
			if oldOpen {
				oldRole = fenceCloses
			}
			oldOpen = !oldOpen
		}
		switch {
		case open > 0 && bare && ticks >= open:
			role, open = fenceCloses, 0
		case open == 0 && ticks >= mdfence.Min:
			role, open = fenceOpens, ticks
		}
		if role != oldRole || (role == fenceOpens && ticks > mdfence.Min) {
			return true
		}
	}
	return false
}

// goldenLengthRuleDepartures is how many golden cases departsFromLengthRule
// picks out, pinned so that a change to the rule's reach is made on purpose.
const goldenLengthRuleDepartures = 391

func TestReadAnswersTheOldParsersGoldenCases(t *testing.T) {
	// Arrange.
	g := loadGolden(t)

	departed := 0
	for i, c := range g.Cases {
		if departsFromLengthRule(c.Text) {
			departed++
			continue
		}

		// Act.
		d, err := read(c.Text)

		// Assert.
		if err != nil {
			t.Fatalf("case %d: read: %v", i, err)
		}
		var want *[3]string
		if c.Region != nil {
			lines := strings.Split(c.Text, "\n")
			top, n := c.Region.Top, c.Region.Count
			want = &[3]string{
				strings.Join(lines[:top], "\n"),
				strings.Join(lines[top:top+n], "\n"),
				strings.Join(lines[top+n:], "\n"),
			}
		}
		var got *[3]string
		if d.region != nil {
			got = &[3]string{d.region.before, d.region.tree, d.region.after}
		}
		if !reflect.DeepEqual(got, want) {
			t.Errorf("case %d %q: region = %q, want %q", i, c.Text, got, want)
		}
		if isMetapromptTree(d) != c.IsMetapromptTree {
			t.Errorf("case %d %q: isMetapromptTree = %v, want %v", i, c.Text, !c.IsMetapromptTree, c.IsMetapromptTree)
		}
		if looksLikeIntendedTree(d) != c.LooksLikeIntendedTree {
			t.Errorf("case %d %q: looksLikeIntendedTree = %v, want %v", i, c.Text, !c.LooksLikeIntendedTree, c.LooksLikeIntendedTree)
		}
		if gotS, wantS := actualSegments(d), expectedSegments(c); !reflect.DeepEqual(gotS, wantS) {
			t.Errorf("case %d %q: segments = %#v, want %#v", i, c.Text, gotS, wantS)
		}
	}
	if departed != goldenLengthRuleDepartures {
		t.Errorf("%d golden cases depart from the length rule, want %d", departed, goldenLengthRuleDepartures)
	}
}

func TestDepartsFromLengthRule(t *testing.T) {
	tests := []struct {
		name string
		text string
		want bool
	}{
		{name: "one fence", text: lines("```go", "x := 1", "```"), want: false},
		{name: "a railed fence", text: lines("│   ```go", "│   x := 1", "│   ```"), want: false},
		{name: "two fences in turn", text: lines("```", "a", "```", "```go", "b", "```"), want: false},
		{name: "an unclosed fence", text: lines("```go", "x := 1"), want: false},
		{name: "no fence", text: lines("1 🔧 A", "└── 1.1 a"), want: false},
		{name: "a delimiter carrying a language inside a fence", text: lines("```", "```go", "x := 1", "```"), want: true},
		{name: "a shorter run inside a longer fence", text: lines("````", "```", "x := 1", "````"), want: true},
		{name: "a longer bare run closing a fence", text: lines("```", "x := 1", "````"), want: false},
		{name: "a fence opened on more than three backticks", text: lines("````", "x := 1", "````"), want: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := departsFromLengthRule(tt.text)

			// Assert.
			if got != tt.want {
				t.Errorf("departsFromLengthRule(%q) = %v, want %v", tt.text, got, tt.want)
			}
		})
	}
}

func TestReadAnswersTheOldParsersGoldenLines(t *testing.T) {
	// Arrange.
	g := loadGolden(t)

	for line, want := range g.Lines {
		// Act.
		d, err := read(line)

		// Assert.
		if err != nil {
			t.Fatalf("line %q: read: %v", line, err)
		}
		it := d.items[0]
		if it.fence != nil {
			if !want.Fence {
				t.Errorf("line %q: read a fence, want none", line)
			}
			continue
		}
		if want.Fence {
			t.Errorf("line %q: read no fence, want one", line)
			continue
		}
		l := it.line
		gotClass := [4]bool{l.class == classConnector, l.class == classRoot, l.class == classRoot || l.class == classDotted, l.class == classHeader}
		if wantClass := [4]bool{want.Connector, want.Root, want.Dotted, want.Header}; gotClass != wantClass {
			t.Errorf("line %q: connector/root/dotted/header = %v, want %v", line, gotClass, wantClass)
		}
		var gotB *goldenBranch
		if l.branch != nil {
			gotB = &goldenBranch{Indent: l.branch.indent, Prefix: l.branch.prefix, Label: l.branch.label, Body: l.branch.body}
		}
		if !reflect.DeepEqual(gotB, want.Branch) {
			t.Errorf("line %q: branch = %+v, want %+v", line, gotB, want.Branch)
		}
		var gotC *goldenContinuation
		if c := l.continuation; c != nil {
			// The indent, prefix and padding are spaces, tabs and rails, each one
			// column wide in the webapp's width model.
			column := utf8.RuneCountInString(c.indent) + utf8.RuneCountInString(c.prefix) + utf8.RuneCountInString(c.padding)
			gotC = &goldenContinuation{Column: column, Text: c.text}
		}
		if !reflect.DeepEqual(gotC, want.Continuation) {
			t.Errorf("line %q: continuation = %+v, want %+v", line, gotC, want.Continuation)
		}
	}
}
