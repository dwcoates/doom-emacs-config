package holds

import (
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

func TestIngressPromptDrawsEachWaitAsItsBadge(t *testing.T) {
	tests := []struct {
		name      string
		kind      IngressWaitKind
		wantLabel string
		wantArm   func(*frontendv1.HeldIngressPromptBadge) bool
	}{
		{"awaiting intake", IngressAwaitingIntake, "waiting to be queued", func(b *frontendv1.HeldIngressPromptBadge) bool { return b.GetAwaitingIntake() != nil }},
		{"no session", IngressNoSession, "waiting for the session", func(b *frontendv1.HeldIngressPromptBadge) bool { return b.GetNoSession() != nil }},
		{"merge", IngressMerge, "merge running", func(b *frontendv1.HeldIngressPromptBadge) bool { return b.GetMerge() != nil }},
		{"cold gate", IngressColdGate, "cold gate", func(b *frontendv1.HeldIngressPromptBadge) bool { return b.GetColdGate() != nil }},
		{"moved away", IngressMovedAway, "workspace moving", func(b *frontendv1.HeldIngressPromptBadge) bool { return b.GetMovedAway() != nil }},
		{"refused", IngressRefused, "not yet queued", func(b *frontendv1.HeldIngressPromptBadge) bool { return b.GetRefused() != nil }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			card := IngressPrompt(IngressEntry{Key: "k-1", Wait: IngressWait{Kind: tc.kind}})

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
	card := IngressPrompt(IngressEntry{Key: "k-1", Said: said, QueuedAt: at,
		Wait: IngressWait{Kind: IngressAwaitingIntake, Detail: "waits behind an earlier prompt"}})

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
			t.Fatal("IngressPrompt returned for an unknown wait kind, want a panic")
		}
	}()

	// Act
	IngressPrompt(IngressEntry{Key: "k-1", Wait: IngressWait{Kind: IngressWaitKind(99)}})
}
