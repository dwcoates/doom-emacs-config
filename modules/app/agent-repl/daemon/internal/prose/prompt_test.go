package prose

import (
	"fmt"
	"strings"
	"testing"

	"google.golang.org/protobuf/proto"

	"agent-repld/internal/dlog"

	frontendv1 "agentrepl/proto/frontend/v1"
)

// promptShape is a parsed prompt in a form a test can state: each block on
// its own line, an entry tree's nodes indented beneath it. A node reads
// `label ^parent = "words"` for an entry, `label ^parent ·` for a path node,
// with `^-` for a root.
func promptShape(p *frontendv1.ParsedProse) string {
	var b strings.Builder
	for _, block := range p.GetBlocks() {
		switch {
		case block.GetPlain() != nil:
			fmt.Fprintf(&b, "plain %q\n", block.GetPlain().GetMarkdown())
		case block.GetTree() != nil:
			b.WriteString("tree\n")
		case block.GetEntries() != nil:
			b.WriteString("entries\n")
			for _, n := range block.GetEntries().GetNodes() {
				parent := "-"
				if br := n.GetBranch(); br != nil {
					parent = fmt.Sprint(br.GetParent().GetIndex())
				}
				if a := n.GetAnswer(); a != nil {
					fmt.Fprintf(&b, "  %s ^%s = %q\n", n.GetLabel().GetText(), parent, a.GetText().GetMarkdown())
				} else {
					fmt.Fprintf(&b, "  %s ^%s ·\n", n.GetLabel().GetText(), parent)
				}
			}
		}
	}
	return b.String()
}

func TestParsePromptReadsEntries(t *testing.T) {
	tests := []struct {
		name string
		text string
		want string
	}{
		{name: "a label and a dash", text: "3.1.2 - my answer", want: lines("entries", "  3 ^- ·", "  3.1 ^0 ·", `  3.1.2 ^1 = "my answer"`, "")},
		{name: "a label and a colon", text: "3.1: my answer", want: lines("entries", "  3 ^- ·", `  3.1 ^0 = "my answer"`, "")},
		{name: "a label and an em dash", text: "2 — my answer", want: lines("entries", `  2 ^- = "my answer"`, "")},
		{name: "a dotted label alone, then words", text: "3.1 my answer", want: lines("entries", "  3 ^- ·", `  3.1 ^0 = "my answer"`, "")},
		{name: "a label with nothing after it", text: "3.1 -", want: lines("entries", "  3 ^- ·", `  3.1 ^0 = ""`, "")},
		{
			name: "an entry is its one line, and the line after it is prose",
			text: "3.1 - first\nmore of it\n",
			want: lines("entries", "  3 ^- ·", `  3.1 ^0 = "first"`, `plain "more of it"`, ""),
		},
		{
			name: "a label on its own line says nothing, and the line after it is prose",
			text: "3.1\nmy answer",
			want: lines("entries", "  3 ^- ·", `  3.1 ^0 = ""`, `plain "my answer"`, ""),
		},
		{
			name: "entries a blank line apart are one tree",
			text: "3.1 - first\n\n3.2 - second",
			want: lines("entries", "  3 ^- ·", `  3.1 ^0 = "first"`, `  3.2 ^0 = "second"`, ""),
		},
		{
			name: "entries answering far-apart labels make the tree their labels imply",
			text: "3.1 - a\n4.2 - b\n5.5.1 - c",
			want: lines("entries",
				"  3 ^- ·", `  3.1 ^0 = "a"`,
				"  4 ^- ·", `  4.2 ^2 = "b"`,
				"  5 ^- ·", "  5.5 ^4 ·", `  5.5.1 ^5 = "c"`, ""),
		},
		{
			name: "a label written after another branch joins its own parent",
			text: "3.1 - a\n4.2 - b\n3.2 - c",
			want: lines("entries", "  3 ^- ·", `  3.1 ^0 = "a"`, `  3.2 ^0 = "c"`, "  4 ^- ·", `  4.2 ^3 = "b"`, ""),
		},
		{
			name: "an entry answering an implied ancestor answers that node",
			text: "3.1 - a\n3 - b",
			want: lines("entries", `  3 ^- = "b"`, `  3.1 ^0 = "a"`, ""),
		},
		{
			name: "two entries answering one label are sibling nodes",
			text: "3.1 - a\n3.1 - b",
			want: lines("entries", "  3 ^- ·", `  3.1 ^0 = "a"`, `  3.1 ^0 = "b"`, ""),
		},
		{
			name: "a line of prose between entries ends the run",
			text: "3.1 - a\na note\n3.2 - b",
			want: lines("entries", "  3 ^- ·", `  3.1 ^0 = "a"`, `plain "a note"`, "entries", "  3 ^- ·", `  3.2 ^0 = "b"`, ""),
		},
		{
			name: "words before the first entry",
			text: "thanks, a few notes\n3.1 - first",
			want: lines(`plain "thanks, a few notes"`, "entries", "  3 ^- ·", `  3.1 ^0 = "first"`, ""),
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := promptShape(ParsePrompt(dlog.NewTestLogger(), tc.text))

			// Assert
			if got != tc.want {
				t.Fatalf("ParsePrompt(%q) =\n%s\nwant\n%s", tc.text, got, tc.want)
			}
		})
	}
}

func TestParsePromptLeavesTextWithNoEntryAsParseReadsIt(t *testing.T) {
	tests := []struct {
		name string
		text string
	}{
		{name: "plain words", text: "please fix the footer"},
		{name: "a bare number opening a line", text: "10 items are left"},
		{name: "a markdown ordered list", text: "1. First\n2. Second"},
		{name: "a pasted tree, whose roots open on a marker", text: "1 🔧 Fixed it\n└── 1.1 a detail"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := ParsePrompt(dlog.NewTestLogger(), tc.text)

			// Assert
			if want := parse(tc.text, Settled).GetBlocks(); !proto.Equal(&frontendv1.ParsedProse{Blocks: got.GetBlocks()}, &frontendv1.ParsedProse{Blocks: want}) {
				t.Fatalf("blocks of %q = %v, want Parse's %v", tc.text, got.GetBlocks(), want)
			}
		})
	}
}

func TestParsePromptKeepsTheTextAsWrittenForItsSource(t *testing.T) {
	tests := []struct {
		name string
		text string
	}{
		{name: "entries", text: "3.2 - second\n3.1 - first"},
		{name: "a pasted tree whose labels Parse would renumber", text: "1 🔧 Fixed it\n└── 1.4 a detail"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := ParsePrompt(dlog.NewTestLogger(), tc.text)

			// Assert
			if src := got.GetSource().GetMarkdown(); src != tc.text {
				t.Fatalf("source = %q, want the text as written %q", src, tc.text)
			}
		})
	}
}

func TestParsePromptDrawsAPastedTreeAfterAnEntryAsATree(t *testing.T) {
	// Arrange — an entry, then a pasted tree's root and its branch.
	text := "3.1 - see this\n1 🔧 Fixed it\n└── 1.1 a detail"

	// Act
	got := ParsePrompt(dlog.NewTestLogger(), text).GetBlocks()

	// Assert
	if len(got) != 2 || got[0].GetEntries() == nil || got[1].GetTree() == nil {
		t.Fatalf("blocks = %v, want the entries and then the tree", got)
	}
}

// TestParsePromptReadsAFenceThatHoldsAShorterFence pins the shape that took
// the daemon down on 2026-10-24: a create-failure notice wraps the asked-for
// prompt in a ```` fence, and the prompt's own indented ```yaml block sat
// inside it. The grammar closed the outer fence on that ``` and could not
// parse `yaml`, and the daemon panicked on the invariant.
func TestParsePromptReadsAFenceThatHoldsAShorterFence(t *testing.T) {
	tests := []struct {
		name string
		text string
	}{
		{name: "an indented info-string run inside a longer fence", text: "What the create asked for:\n````\n1. a step\n   ```yaml\n   key: value\n   ```\n````\nafter"},
		{name: "a bare shorter run inside a longer fence", text: "````\n```\nstill inside\n````"},
		{name: "a run with text after it", text: "```\n```go not a close\n```"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := ParsePrompt(dlog.NewTestLogger(), tc.text)

			// Assert
			if src := got.GetSource().GetMarkdown(); src != tc.text {
				t.Fatalf("source = %q, want the text as written", src)
			}
		})
	}
}

func TestParsePromptRecordsAFailureOnceAndPanics(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	defer func(saved func(string) *frontendv1.ParsedProse) { parsePromptText = saved }(parsePromptText)
	parsePromptText = func(string) *frontendv1.ParsedProse { panic("prose: a broken prompt parse") }

	// Act
	recovered := func() (r any) {
		defer func() { r = recover() }()
		ParsePrompt(log, "3.1 - x")
		return nil
	}()

	// Assert
	if recovered != "prose: a broken prompt parse" {
		t.Fatalf("ParsePrompt recovered %v, want the parse's own panic re-raised", recovered)
	}
	errors := dlog.FilterRecords(log.Records(), dlog.RecordQuery{Level: "error", Operation: invariantOperation})
	if len(errors) != 1 {
		t.Fatalf("ParsePrompt recorded %d errors under %s, want 1: %+v", len(errors), invariantOperation, log.Records())
	}
	if ctx := errors[0].Context; ctx["function"] != "ParsePrompt" || ctx["length"] != len("3.1 - x") || ctx["cause"] != "prose: a broken prompt parse" {
		t.Errorf("the record's context = %v, want the function, the text's length and the cause", ctx)
	}
}
