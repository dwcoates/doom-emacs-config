package prompthandler

import (
	"context"
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// compactionSubmitter records the one submission a compaction makes, or fails it.
type compactionSubmitter struct {
	ws       ids.WorkspaceID
	text     string
	key      string
	origin   conversationv1.PromptOrigin
	delivery wsm.Delivery
	calls    int
	err      error
}

func (s *compactionSubmitter) Submit(_ context.Context, ws ids.WorkspaceID, said *conversationv1.UserSaid, key string, origin conversationv1.PromptOrigin, _ *ids.WorkspaceID, delivery wsm.Delivery, _ *feedid.Ref) (Outcome, error) {
	s.calls++
	s.ws, s.key, s.origin, s.delivery = ws, key, origin, delivery
	s.text = said.GetContent().GetBlocks()[0].GetText().GetText()
	return Outcome{Turn: "turn-1"}, s.err
}

func TestCompactionText(t *testing.T) {
	tests := []struct {
		name, instructions, want string
	}{
		{name: "no instructions", want: "/compact"},
		{name: "instructions", instructions: "  keep the design  ", want: "/compact keep the design"},
		{name: "blank instructions", instructions: "   ", want: "/compact"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act / Assert
			if got := CompactionText(tt.instructions); got != tt.want {
				t.Fatalf("CompactionText(%q) = %q, want %q", tt.instructions, got, tt.want)
			}
		})
	}
}

func TestSubmitCompactionSubmitsTheDeferredCompactionUnderAMintedKey(t *testing.T) {
	// Arrange
	prompts := &compactionSubmitter{}
	mint := func(prefix string) (string, error) { return prefix + "-1", nil }

	// Act
	outcome, err := SubmitCompaction(context.Background(), prompts, "ws-1", "keep it", conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_COMPACT, mint, "compact")

	// Assert
	if err != nil || outcome.Turn != "turn-1" {
		t.Fatalf("SubmitCompaction = %+v, %v; want the submission's outcome", outcome, err)
	}
	if prompts.ws != "ws-1" || prompts.text != "/compact keep it" || prompts.key != "compact-1" ||
		prompts.origin != conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_COMPACT || prompts.delivery != wsm.DeliveryDeferred {
		t.Fatalf("submitted %+v, want the deferred compaction of ws-1 under compact-1", prompts)
	}
}

func TestSubmitCompactionFails(t *testing.T) {
	tests := []struct {
		name      string
		mintErr   error
		submitErr error
		wantKey   bool
		wantCalls int
	}{
		{name: "a key that cannot be minted submits nothing", mintErr: errors.New("no entropy"), wantKey: true},
		{name: "a refused submission is returned as it is", submitErr: errors.New("closed"), wantCalls: 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			prompts := &compactionSubmitter{err: tt.submitErr}
			mint := func(prefix string) (string, error) { return prefix + "-1", tt.mintErr }

			// Act
			_, err := SubmitCompaction(context.Background(), prompts, "ws-1", "", conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_COMPACT, mint, "compact")

			// Assert
			if err == nil || errors.Is(err, ErrCompactionKey) != tt.wantKey || prompts.calls != tt.wantCalls {
				t.Fatalf("SubmitCompaction = %v (calls %d); want key failure %v with %d calls", err, prompts.calls, tt.wantKey, tt.wantCalls)
			}
		})
	}
}
