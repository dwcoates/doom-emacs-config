package feed

import (
	"testing"

	"agent-repld/internal/dlog"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

func TestAResponseDrawsItsLinksWithoutCodeMarkers(t *testing.T) {
	tests := []struct {
		name string
		text string
		want string
	}{
		{"link text that is inline code", "see [`foo`](https://x.com/a)", "see [foo](https://x.com/a)"},
		{"backticks in a link destination", "see [foo](https://x.com/`a`)", "see [foo](https://x.com/a)"},
		{"backticks in a bare URL", "see https://x.com/`a`/b now", "see https://x.com/a/b now"},
		{"code beside a link kept", "run `go` at https://x.com", "run `go` at https://x.com"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			h.resolver.OnActivity(testWorkspace, mainAgent(),
				responseFrame("unit-1", &conversationv1.AgentResponseUpdate{NewMarkdown: tc.text}, nil), nil, nil)

			// Assert
			parsed := h.response().GetUpdate().GetProse().GetProse()
			if got := plainMarkdown(t, parsed); got != tc.want {
				t.Fatalf("drawn markdown = %q, want %q", got, tc.want)
			}
			if got := parsed.GetSource().GetMarkdown(); got != tc.text {
				t.Fatalf("source = %q, want the text as written", got)
			}
		})
	}
}

func TestAPromptDrawsItsLinksWithoutCodeMarkers(t *testing.T) {
	tests := []struct {
		name  string
		text  string
		spans []string
		want  string
	}{
		{
			// The prompt that drew https://github.com/`ChessCom`/explanation-engine
			// after a response showed `ChessCom` as code (2026-11-19).
			name:  "the recent spans never mark a URL",
			text:  "i dont seem to have access to https://github.com/ChessCom/explanation-engine either. I can see https://github.com/ChessCom",
			spans: []string{"ChessCom", "explanation-engine"},
			want:  "i dont seem to have access to https://github.com/ChessCom/explanation-engine either. I can see https://github.com/ChessCom",
		},
		{
			name: "code written in a link is taken out",
			text: "[`a`](https://x.com/`b`) and https://x.com/`c`",
			want: "[a](https://x.com/b) and https://x.com/c",
		},
		{
			name:  "a span beside a URL is still marked",
			text:  "is ChessCom https://github.com/ChessCom",
			spans: []string{"ChessCom"},
			want:  "is `ChessCom` https://github.com/ChessCom",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			content := &conversationv1.UserContent{Blocks: []*conversationv1.UserContentBlock{
				{Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: tc.text}}},
			}}
			draw := func(parsed *frontendv1.ParsedProse) { h.resolver.DrawLinks(testWorkspace, parsed) }

			// Act
			blocks := DrawUserBlocks(content, func(s string) string { return s }, refusingResolver, draw, tc.spans, dlog.NewTestLogger())

			// Assert
			parsed := blocks[0].GetText().GetProse()
			if got := plainMarkdown(t, parsed); got != tc.want {
				t.Fatalf("drawn prompt = %q, want %q", got, tc.want)
			}
			if got := parsed.GetSource().GetMarkdown(); got != tc.text {
				t.Fatalf("prompt source = %q, want the text as written", got)
			}
		})
	}
}

func TestUnmarkLinksReachesEveryDrawnMarkdown(t *testing.T) {
	// Arrange: a tree node's text, which the prompt's code span marking skips,
	// and an entry's answer.
	node := &frontendv1.ProseTreeNodeText{Markdown: "[`a`](https://x.com/`b`)"}
	answer := &frontendv1.ProseEntryText{Markdown: "https://x.com/`c`"}
	parsed := &frontendv1.ParsedProse{Blocks: []*frontendv1.ProseBlock{
		{Block: &frontendv1.ProseBlock_Tree{Tree: &frontendv1.ProseTree{Nodes: []*frontendv1.ProseTreeNode{{Text: node}}}}},
		{Block: &frontendv1.ProseBlock_Entries{Entries: &frontendv1.ProseEntryTree{
			Nodes: []*frontendv1.ProseEntryTreeNode{{Kind: &frontendv1.ProseEntryTreeNode_Answer{
				Answer: &frontendv1.ProseEntryTreeNodeAnswer{Text: answer},
			}}},
		}}},
	}}

	// Act
	unmarkLinks(parsed)

	// Assert
	if got := node.GetMarkdown(); got != "[a](https://x.com/b)" {
		t.Fatalf("tree node = %q, want its link unmarked", got)
	}
	if got := answer.GetMarkdown(); got != "https://x.com/c" {
		t.Fatalf("entry answer = %q, want its URL unmarked", got)
	}
}
