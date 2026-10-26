package feed

import (
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/pullrefs"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

var chessComTarget = &pullrefs.Target{Owner: "ChessCom", Repo: "explanation-engine"}

const linked12 = "[#12](https://github.com/ChessCom/explanation-engine/pull/12)"

// settleResponse draws one settled response bubble saying MARKDOWN and
// answers its prose.
func settleResponse(h *harness, markdown string) *frontendv1.ParsedProse {
	h.t.Helper()
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: markdown},
		}, nil), nil, nil)
	return h.response().GetSuccess().GetProse().GetProse()
}

// plainMarkdown answers the markdown of PARSED's only plain block.
func plainMarkdown(t *testing.T, parsed *frontendv1.ParsedProse) string {
	t.Helper()
	if len(parsed.GetBlocks()) != 1 || parsed.GetBlocks()[0].GetPlain() == nil {
		t.Fatalf("blocks = %+v, want one plain block", parsed.GetBlocks())
	}
	return parsed.GetBlocks()[0].GetPlain().GetMarkdown()
}

func TestAResponseLinksItsPullReferences(t *testing.T) {
	cases := []struct {
		name   string
		target *pullrefs.Target
		want   string
	}{
		{name: "a workspace with a target links them", target: chessComTarget, want: "landed in " + linked12},
		{name: "a workspace with no target leaves them", want: "landed in #12"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.resolver.SetPullTarget(testWorkspace, tc.target)

			// Act.
			parsed := settleResponse(h, "landed in #12")

			// Assert.
			if got := plainMarkdown(t, parsed); got != tc.want {
				t.Fatalf("drawn markdown = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestAResponsesSourceStaysAsWritten(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.resolver.SetPullTarget(testWorkspace, chessComTarget)

	// Act.
	parsed := settleResponse(h, "landed in #12")

	// Assert: a reply quotes and a search reads what the agent wrote.
	if got := parsed.GetSource().GetMarkdown(); got != "landed in #12" {
		t.Fatalf("source = %q, want the text as written", got)
	}
}

func TestAnArrivingResponseLeavesItsTrailingReference(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.resolver.SetPullTarget(testWorkspace, chessComTarget)

	// Act: the text so far ends on a reference more digits may extend.
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseUpdate{NewMarkdown: "#12 then #3"}, nil), nil, nil)

	// Assert.
	if got := plainMarkdown(t, h.response().GetUpdate().GetProse().GetProse()); got != linked12+" then #3" {
		t.Fatalf("drawn markdown = %q, want only the earlier reference linked", got)
	}
}

func TestAWithdrawnTargetLinksNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.resolver.SetPullTarget(testWorkspace, chessComTarget)

	// Act.
	h.resolver.SetPullTarget(testWorkspace, nil)
	parsed := settleResponse(h, "landed in #12")

	// Assert.
	if got := plainMarkdown(t, parsed); got != "landed in #12" {
		t.Fatalf("drawn markdown = %q, want nothing linked", got)
	}
	if h.resolver.PullTarget(testWorkspace) != nil {
		t.Fatal("PullTarget answers a target after it was withdrawn")
	}
}

func TestAResetKeepsThePullTarget(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.resolver.SetPullTarget(testWorkspace, chessComTarget)

	// Act.
	h.resolver.ResetWorkspace(testWorkspace, "test")

	// Assert.
	if got := h.resolver.PullTarget(testWorkspace); got == nil || *got != *chessComTarget {
		t.Fatalf("target after a reset = %v, want %v", got, chessComTarget)
	}
}

func TestSetPullTargetRecordsTheTarget(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.resolver.SetPullTarget(testWorkspace, chessComTarget)

	// Assert.
	if !h.hasRecordWith("debug", "daemon.feed.pull_target", "owner", "ChessCom") {
		t.Fatalf("records = %+v, want the installed target recorded", h.records())
	}
}

func TestLinkPullsLinksEveryDrawnMarkdown(t *testing.T) {
	cases := []struct {
		name  string
		block *frontendv1.ProseBlock
		read  func(*frontendv1.ProseBlock) string
	}{
		{
			name: "a tree node's text",
			block: &frontendv1.ProseBlock{Block: &frontendv1.ProseBlock_Tree{Tree: &frontendv1.ProseTree{
				Nodes: []*frontendv1.ProseTreeNode{{Text: &frontendv1.ProseTreeNodeText{Markdown: "see #12"}}},
			}}},
			read: func(b *frontendv1.ProseBlock) string { return b.GetTree().GetNodes()[0].GetText().GetMarkdown() },
		},
		{
			name: "an entry's answer",
			block: &frontendv1.ProseBlock{Block: &frontendv1.ProseBlock_Entries{Entries: &frontendv1.ProseEntryTree{
				Nodes: []*frontendv1.ProseEntryTreeNode{{Kind: &frontendv1.ProseEntryTreeNode_Answer{
					Answer: &frontendv1.ProseEntryTreeNodeAnswer{Text: &frontendv1.ProseEntryText{Markdown: "see #12"}},
				}}},
			}}},
			read: func(b *frontendv1.ProseBlock) string {
				return b.GetEntries().GetNodes()[0].GetAnswer().GetText().GetMarkdown()
			},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			parsed := &frontendv1.ParsedProse{Blocks: []*frontendv1.ProseBlock{tc.block}}

			// Act.
			linkPulls(parsed, chessComTarget, false)

			// Assert.
			if got := tc.read(tc.block); got != "see "+linked12 {
				t.Fatalf("drawn markdown = %q, want the reference linked", got)
			}
		})
	}
}

func TestDrawUserBlocksLinksAPromptsReferences(t *testing.T) {
	// Arrange.
	content := &conversationv1.UserContent{Blocks: []*conversationv1.UserContentBlock{
		{Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: "review #12"}}},
	}}

	// Act.
	blocks := DrawUserBlocks(content, func(s string) string { return s }, refusingResolver, chessComTarget, dlog.NewTestLogger())

	// Assert.
	parsed := blocks[0].GetText().GetProse()
	if got := plainMarkdown(t, parsed); got != "review "+linked12 {
		t.Fatalf("drawn prompt = %q, want the reference linked", got)
	}
	if got := parsed.GetSource().GetMarkdown(); got != "review #12" {
		t.Fatalf("prompt source = %q, want the text as written", got)
	}
}
