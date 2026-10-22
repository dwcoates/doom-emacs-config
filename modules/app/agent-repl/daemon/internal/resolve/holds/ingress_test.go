package holds_test

import (
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/resolve/holds"
)

func TestIngressPromptDrawsTheBodyItsDrawerResolves(t *testing.T) {
	// Arrange
	said := &conversationv1.UserSaid{Content: &conversationv1.UserContent{Blocks: []*conversationv1.UserContentBlock{
		{Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: "hi"}}},
	}}}

	// Act
	card := holds.IngressPrompt(holds.IngressEntry{Key: "k-1", Said: said, Wait: holds.IngressWait{Kind: holds.IngressAwaitingIntake}}, textBody)

	// Assert
	if got := card.GetBody().GetBlocks()[0].GetText().GetProse().GetSource().GetMarkdown(); got != "drawn:hi" {
		t.Fatalf("body = %q, want the drawer's resolution of the said", got)
	}
}

func TestIngressPromptDrawsEachWaitAsItsBadge(t *testing.T) {
	tests := []struct {
		name      string
		kind      holds.IngressWaitKind
		wantLabel string
		wantArm   func(*frontendv1.HeldIngressPromptBadge) bool
	}{
		{"awaiting intake", holds.IngressAwaitingIntake, "waiting to be queued", func(b *frontendv1.HeldIngressPromptBadge) bool { return b.GetAwaitingIntake() != nil }},
		{"no session", holds.IngressNoSession, "waiting for the session", func(b *frontendv1.HeldIngressPromptBadge) bool { return b.GetNoSession() != nil }},
		{"merge", holds.IngressMerge, "merge running", func(b *frontendv1.HeldIngressPromptBadge) bool { return b.GetMerge() != nil }},
		{"cold gate", holds.IngressColdGate, "cold gate", func(b *frontendv1.HeldIngressPromptBadge) bool { return b.GetColdGate() != nil }},
		{"moved away", holds.IngressMovedAway, "workspace moving", func(b *frontendv1.HeldIngressPromptBadge) bool { return b.GetMovedAway() != nil }},
		{"refused", holds.IngressRefused, "not yet queued", func(b *frontendv1.HeldIngressPromptBadge) bool { return b.GetRefused() != nil }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			card := holds.IngressPrompt(holds.IngressEntry{Key: "k-1", Wait: holds.IngressWait{Kind: tc.kind}}, textBody)

			// Assert
			badge := card.GetBadge()
			if badge.GetLabel() != tc.wantLabel || !tc.wantArm(badge) {
				t.Fatalf("badge = %v, want %q under its own arm", badge, tc.wantLabel)
			}
			if badge.Detail != nil {
				t.Fatalf("detail = %q, want none for a wait with no sentence", badge.GetDetail())
			}
		})
	}
}

func TestIngressPromptCarriesTheSaidTheQueuedAtAndTheDetail(t *testing.T) {
	// Arrange
	said := &conversationv1.UserSaid{Content: &conversationv1.UserContent{}}
	at := time.UnixMilli(1_700_000_000_123)

	// Act
	card := holds.IngressPrompt(holds.IngressEntry{Key: "k-1", Said: said, QueuedAt: at,
		Wait: holds.IngressWait{Kind: holds.IngressAwaitingIntake, Detail: "waits behind an earlier prompt"}}, textBody)

	// Assert
	if card.GetSaid() != said {
		t.Fatal("said = another message, want the entry's own")
	}
	if card.GetQueuedAt().GetAtMs() != at.UnixMilli() {
		t.Fatalf("queued at = %d, want %d", card.GetQueuedAt().GetAtMs(), at.UnixMilli())
	}
	if card.GetBadge().GetDetail() != "waits behind an earlier prompt" {
		t.Fatalf("detail = %q, want the wait's sentence", card.GetBadge().GetDetail())
	}
}

func TestIngressPromptPanicsOnAWaitWithNoBadge(t *testing.T) {
	// Arrange
	defer func() {
		// Assert
		if recover() == nil {
			t.Fatal("holds.IngressPrompt returned for an unknown wait kind, want a panic")
		}
	}()

	// Act
	holds.IngressPrompt(holds.IngressEntry{Key: "k-1", Wait: holds.IngressWait{Kind: holds.IngressWaitKind(99)}}, textBody)
}
