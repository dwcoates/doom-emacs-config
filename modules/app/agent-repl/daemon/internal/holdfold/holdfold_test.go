package holdfold

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/feedid"
	"claude-repld/internal/wsm"
)

// textBlock is one text block.
func textBlock(text string) *conversationv1.UserContentBlock {
	return &conversationv1.UserContentBlock{Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: text}}}
}

// imageBlock is one image block, which carries no text.
func imageBlock() *conversationv1.UserContentBlock {
	return &conversationv1.UserContentBlock{Block: &conversationv1.UserContentBlock_Image{Image: &conversationv1.ImageBlock{}}}
}

// said is a submission of BLOCKS, in order.
func said(blocks ...*conversationv1.UserContentBlock) *conversationv1.UserSaid {
	return &conversationv1.UserSaid{Content: &conversationv1.UserContent{Blocks: blocks}}
}

func TestSaidText(t *testing.T) {
	tests := []struct {
		name string
		said *conversationv1.UserSaid
		want string
	}{
		{name: "one text block is its text", said: said(textBlock("fix it")), want: "fix it"},
		{name: "text blocks join with a newline", said: said(textBlock("first"), textBlock("second")), want: "first\nsecond"},
		{name: "an image contributes no text", said: said(textBlock("look"), imageBlock(), textBlock("here")), want: "look\nhere"},
		{name: "nothing said is empty", said: nil, want: ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := SaidText(tt.said)

			// Assert
			if got != tt.want {
				t.Fatalf("SaidText = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestContextCut(t *testing.T) {
	tests := []struct {
		name    string
		target  *feedid.Ref
		said    *conversationv1.UserSaid
		wantCut bool
		want    conversationv1.SessionCommand
	}{
		{name: "a session-addressed /compact is a cut", said: said(textBlock("/compact")), wantCut: true, want: conversationv1.SessionCommand_SESSION_COMMAND_COMPACT},
		{name: "a session-addressed /clear is a cut", said: said(textBlock("/clear")), wantCut: true, want: conversationv1.SessionCommand_SESSION_COMMAND_CLEAR},
		{name: "an ordinary prompt is not", said: said(textBlock("fix the test")), wantCut: false},
		{name: "a bubble-addressed /clear is not", target: &feedid.Ref{}, said: said(textBlock("/clear")), wantCut: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			command, _, cut := ContextCut(tt.target, tt.said)

			// Assert
			if cut != tt.wantCut || (cut && command != tt.want) {
				t.Fatalf("ContextCut = (%v, %v), want (%v, %v)", command, cut, tt.want, tt.wantCut)
			}
		})
	}
}

func TestSessionAct(t *testing.T) {
	tests := []struct {
		name string
		hold wsm.HeldPrompt
		want bool
	}{
		{name: "a held model change is an act", hold: wsm.HeldPrompt{Said: said(textBlock("model")), Act: &wsm.HeldAct{Kind: wsm.ActModel, Value: "opus"}}, want: true},
		{name: "a held /compact is an act", hold: wsm.HeldPrompt{Said: said(textBlock("/compact"))}, want: true},
		{name: "an ordinary prompt is not", hold: wsm.HeldPrompt{Said: said(textBlock("fix the test"))}, want: false},
		{name: "a bubble-addressed /clear is not", hold: wsm.HeldPrompt{Said: said(textBlock("/clear")), Target: &feedid.Ref{}}, want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := SessionAct(tt.hold)

			// Assert
			if got != tt.want {
				t.Fatalf("SessionAct = %v, want %v", got, tt.want)
			}
		})
	}
}
