package feed

import (
	"fmt"
	"strings"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/prose"

	frontendv1 "agentrepl/proto/frontend/v1"
)

func TestEachDrawnMarkdown(t *testing.T) {
	plain := func(md string) *frontendv1.ProseBlock {
		return &frontendv1.ProseBlock{Block: &frontendv1.ProseBlock_Plain{Plain: &frontendv1.ProsePlain{Markdown: md}}}
	}
	tree := &frontendv1.ProseBlock{Block: &frontendv1.ProseBlock_Tree{Tree: &frontendv1.ProseTree{
		Nodes: []*frontendv1.ProseTreeNode{
			{Text: &frontendv1.ProseTreeNodeText{Markdown: "t1"}},
			{Text: &frontendv1.ProseTreeNodeText{Markdown: "t2"}},
		},
	}}}
	entries := &frontendv1.ProseBlock{Block: &frontendv1.ProseBlock_Entries{Entries: &frontendv1.ProseEntryTree{
		Nodes: []*frontendv1.ProseEntryTreeNode{{Kind: &frontendv1.ProseEntryTreeNode_Answer{
			Answer: &frontendv1.ProseEntryTreeNodeAnswer{Text: &frontendv1.ProseEntryText{Markdown: "e1"}},
		}}},
	}}}
	cases := []struct {
		name   string
		blocks []*frontendv1.ProseBlock
		trees  bool
		want   []string
	}{
		{name: "the last plain block ends the text", blocks: []*frontendv1.ProseBlock{plain("p1"), plain("p2")}, trees: true,
			want: []string{"p1:false", "p2:true"}},
		{name: "the last tree node ends the text", blocks: []*frontendv1.ProseBlock{plain("p1"), tree}, trees: true,
			want: []string{"p1:false", "t1:false", "t2:true"}},
		{name: "trees are skipped when asked", blocks: []*frontendv1.ProseBlock{tree, plain("p1")},
			want: []string{"p1:true"}},
		{name: "an entry answer never ends the text", blocks: []*frontendv1.ProseBlock{plain("p1"), entries}, trees: true,
			want: []string{"p1:false", "e1:false"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			parsed := &frontendv1.ParsedProse{Blocks: tc.blocks}
			var seen []string

			// Act.
			eachDrawnMarkdown(parsed, tc.trees, func(markdown string, ends bool) string {
				seen = append(seen, fmt.Sprintf("%s:%v", markdown, ends))
				return markdown + "!"
			})

			// Assert.
			if strings.Join(seen, ",") != strings.Join(tc.want, ",") {
				t.Fatalf("visited %v, want %v", seen, tc.want)
			}
			if got := tc.blocks[len(tc.blocks)-1].GetPlain().GetMarkdown(); tc.blocks[len(tc.blocks)-1].GetPlain() != nil && !strings.HasSuffix(got, "!") {
				t.Fatalf("last plain block = %q, want it replaced", got)
			}
		})
	}
}

func TestMarkCodeSpansAndLinkPullsShareTheDrawnMarkdownWalk(t *testing.T) {
	// Arrange: both rewrites visit the same plain and entry markdown, so a
	// hand-rolled walk in either that diverges fails here.
	parsed := prose.ParsePrompt(dlog.NewTestLogger(), "run foo() for #12\n\n1 - run foo() for #12")
	h := newHarness(t)
	h.resolver.SetPullTarget(testWorkspace, chessComTarget)

	// Act.
	markCodeSpans(parsed, []string{"foo()"})
	h.resolver.linkPulls(testWorkspace, parsed, false)

	// Assert.
	var got []string
	eachDrawnMarkdown(parsed, false, func(markdown string, _ bool) string {
		got = append(got, markdown)
		return markdown
	})
	want := "run `foo()` for " + linked12
	for _, md := range got {
		if !strings.Contains(md, want) {
			t.Fatalf("drawn markdown = %q, want every one to read %q", got, want)
		}
	}
	if len(got) < 2 {
		t.Fatalf("drawn markdown = %q, want a plain block and an entry answer", got)
	}
}
