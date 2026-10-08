package holdfold

import (
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
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
		{name: "a reply's quote is not the person's words", said: said(quoteBlock(), textBlock("and its population?")), want: "and its population?"},
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

// prompt is an ordinary held prompt under TURN.
func prompt(turn ids.TurnID) wsm.HeldPrompt {
	return wsm.HeldPrompt{Turn: turn, Said: said(textBlock("words of " + string(turn)))}
}

func TestAhead(t *testing.T) {
	standing := []wsm.HeldPrompt{prompt("a"), prompt("b"), prompt("c")}
	tests := []struct {
		name     string
		turn     ids.TurnID
		want     ids.TurnID
		wantSeen bool
	}{
		{name: "the entry before it in queue order", turn: "c", want: "b", wantSeen: true},
		{name: "nothing ahead of the first entry", turn: "a", wantSeen: false},
		{name: "nothing ahead of an entry that does not stand", turn: "gone", wantSeen: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, ok := Ahead(standing, tt.turn)

			// Assert
			if ok != tt.wantSeen || (ok && got.Turn != tt.want) {
				t.Fatalf("Ahead(%s) = (%s, %v), want (%s, %v)", tt.turn, got.Turn, ok, tt.want, tt.wantSeen)
			}
		})
	}
}

func TestFoldable(t *testing.T) {
	act := wsm.HeldPrompt{Turn: "act", Said: said(textBlock("model")), Act: &wsm.HeldAct{Kind: wsm.ActPermissionMode, Value: "plan"}}
	cut := wsm.HeldPrompt{Turn: "cut", Said: said(textBlock("/compact"))}
	tests := []struct {
		name    string
		folded  wsm.HeldPrompt
		ahead   wsm.HeldPrompt
		editing ids.TurnID
		want    error
	}{
		{name: "two prompts with no edit standing fold", folded: prompt("b"), ahead: prompt("a"), want: nil},
		{name: "an edit on a third entry does not matter", folded: prompt("b"), ahead: prompt("a"), editing: "z", want: nil},
		{name: "a folded session act is refused", folded: act, ahead: prompt("a"), want: ErrNotAPrompt},
		{name: "a folded context cut is refused", folded: cut, ahead: prompt("a"), want: ErrNotAPrompt},
		{name: "an act ahead is refused", folded: prompt("b"), ahead: act, want: ErrAboveNotAPrompt},
		{name: "a context cut ahead is refused", folded: prompt("b"), ahead: cut, want: ErrAboveNotAPrompt},
		{name: "the folded entry being edited is refused", folded: prompt("b"), ahead: prompt("a"), editing: "b", want: ErrEdited},
		{name: "the entry ahead being edited is refused", folded: prompt("b"), ahead: prompt("a"), editing: "a", want: ErrEdited},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := Foldable(tt.folded, tt.ahead, tt.editing)

			// Assert
			if !errors.Is(got, tt.want) || (tt.want == nil && got != nil) {
				t.Fatalf("Foldable = %v, want %v", got, tt.want)
			}
		})
	}
}

// quoteBlock is a reply's quote of an earlier bubble.
func quoteBlock() *conversationv1.UserContentBlock {
	return &conversationv1.UserContentBlock{Block: &conversationv1.UserContentBlock_Quote{
		Quote: &conversationv1.UserQuoteBlock{Text: "⟢ Replying to an earlier response of yours:\n\n```\nParis.\n```\n\n⟢ My message:\n"},
	}}
}
