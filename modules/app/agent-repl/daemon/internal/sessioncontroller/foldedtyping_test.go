// The LIVE TYPING RELAY against the WINDOW FOLD: a preview may be opened on the
// top-level feed only when the record it previews will land there to retire it.
package sessioncontroller

import (
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"

	"claude-repld/internal/frontend"
)

// openTestSkillWindow opens one skill window on a consumer's bubble store.
func openTestSkillWindow(t *testing.T, c *consumer, toolUseID string) {
	t.Helper()
	_, fault, err := c.bubbles.openSkillWindow(frontend.SkillInvocation{
		ToolUseID: toolUseID,
		SkillName: "some-skill",
		Label:     "/some-skill",
	}, 1000)
	if err != nil {
		t.Fatalf("openSkillWindow(%s): %v", toolUseID, err)
	}
	if fault != nil {
		t.Fatalf("openSkillWindow(%s) faulted: %v", toolUseID, fault)
	}
}

// inputDelta is one tool-input chunk on a stable tool identity.
func inputDelta(toolUseID, chunk string) *corev1.Event {
	id := toolUseID
	return &corev1.Event{
		SessionId: "s1",
		Payload: &corev1.Event_ContentDelta{ContentDelta: &corev1.ContentDelta{
			Uuid:      "u1",
			Delta:     &corev1.ContentDelta_InputJson{InputJson: chunk},
			ToolUseId: &id,
		}},
	}
}

func TestTypingRelayVerdict(t *testing.T) {
	tests := []struct {
		name         string
		windows      []string // skill windows to open, outermost first
		toolUseID    string
		wantSuppress bool
		wantReason   string
	}{
		{
			name:         "no window open relays the preview",
			toolUseID:    "toolu_call",
			wantSuppress: false,
			wantReason:   typingRelayKeptNoWindow,
		},
		{
			name:         "outermost window's own call keeps its preview",
			windows:      []string{"toolu_skill"},
			toolUseID:    "toolu_skill",
			wantSuppress: false,
			wantReason:   typingRelayKeptWindowOwnCard,
		},
		{
			name:         "a call made inside the window is suppressed",
			windows:      []string{"toolu_skill"},
			toolUseID:    "toolu_inner",
			wantSuppress: true,
			wantReason:   typingRelaySuppressedFold,
		},
		{
			name:         "a nested window's own call is suppressed, its card folds into the outer one",
			windows:      []string{"toolu_outer", "toolu_inner_skill"},
			toolUseID:    "toolu_inner_skill",
			wantSuppress: true,
			wantReason:   typingRelaySuppressedFold,
		},
		{
			name:         "prose carries no tool identity and always folds with the window",
			windows:      []string{"toolu_skill"},
			toolUseID:    "",
			wantSuppress: true,
			wantReason:   typingRelaySuppressedFold,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			c := newTestConsumer(&fakePusher{}, &fakeApplier{})
			for _, w := range tt.windows {
				openTestSkillWindow(t, c, w)
			}

			// Act.
			got := c.bubbles.typingRelayVerdict(tt.toolUseID)

			// Assert.
			if got.Suppress != tt.wantSuppress {
				t.Errorf("Suppress = %v, want %v (reason %q)", got.Suppress, tt.wantSuppress, got.Reason)
			}
			if got.Reason != tt.wantReason {
				t.Errorf("Reason = %q, want %q", got.Reason, tt.wantReason)
			}
			if got.Suppress && got.BubbleID == "" {
				t.Error("a suppressed verdict names no destination bubble")
			}
		})
	}
}

func TestConsumeSuppressesInputPreviewThatWouldFoldAway(t *testing.T) {
	// Arrange: a skill window is open, and the agent inside it starts a call.
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})
	openTestSkillWindow(t, c, "toolu_skill")

	// Act.
	if err := c.Consume(inputDelta("toolu_inner", `{"comm`)); err != nil {
		t.Fatalf("Consume: %v", err)
	}

	// Assert: no preview the feed could never retire was opened.
	if len(push.typing) != 0 {
		t.Fatalf("typing pushes = %d, want 0 — the previewed record folds into the bubble", len(push.typing))
	}
}

func TestConsumeRelaysInputPreviewForTheWindowsOwnCall(t *testing.T) {
	// Arrange: the outermost window's own card stays on the feed, so its
	// input preview is retired there and is honest to open.
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})
	openTestSkillWindow(t, c, "toolu_skill")

	// Act.
	if err := c.Consume(inputDelta("toolu_skill", `{"skill`)); err != nil {
		t.Fatalf("Consume: %v", err)
	}

	// Assert.
	if len(push.typing) != 1 {
		t.Fatalf("typing pushes = %d, want 1", len(push.typing))
	}
	if got := push.typing[0].GetDelta().GetInputJson(); got != `{"skill` {
		t.Errorf("relayed chunk = %q, want %q", got, `{"skill`)
	}
}

func TestConsumeRelaysPreviewOnceTheWindowIsGone(t *testing.T) {
	// Arrange: the window settles, so the feed carries the records again.
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})
	openTestSkillWindow(t, c, "toolu_skill")
	settled := frontend.AsyncVerdict{Status: corev1.TerminalStatus_TERMINAL_STATUS_DONE, AtMs: 1001}
	if _, err := c.bubbles.settleWindows(settled, "user_prompt"); err != nil {
		t.Fatalf("settleWindows: %v", err)
	}

	// Act.
	if err := c.Consume(inputDelta("toolu_inner", `{"comm`)); err != nil {
		t.Fatalf("Consume: %v", err)
	}

	// Assert.
	if len(push.typing) != 1 {
		t.Fatalf("typing pushes = %d, want 1 once no window folds the record away", len(push.typing))
	}
}

func TestFoldedTypingLedgerAnnouncesFirstThenPeriodically(t *testing.T) {
	// Arrange.
	var l foldedTypingLedger

	// Act + Assert.
	if count, announce := l.note("bubble:a"); count != 1 || !announce {
		t.Errorf("first note = (%d, %v), want (1, true)", count, announce)
	}
	for i := 2; i < foldedTypingAnnounceEvery; i++ {
		if _, announce := l.note("bubble:a"); announce {
			t.Fatalf("note %d announced, want silent between announcements", i)
		}
	}
	if count, announce := l.note("bubble:a"); count != foldedTypingAnnounceEvery || !announce {
		t.Errorf("note %d = (%d, %v), want (%d, true)", foldedTypingAnnounceEvery, count, announce, foldedTypingAnnounceEvery)
	}
}

func TestFoldedTypingLedgerCountsPerBubble(t *testing.T) {
	// Arrange.
	var l foldedTypingLedger

	// Act.
	l.note("bubble:a")
	l.note("bubble:a")
	l.note("bubble:b")

	// Assert: two windows folding at once are two separate accounts.
	if got := l.suppressed("bubble:a"); got != 2 {
		t.Errorf("bubble:a suppressed = %d, want 2", got)
	}
	if got := l.suppressed("bubble:b"); got != 1 {
		t.Errorf("bubble:b suppressed = %d, want 1", got)
	}
}
