// The LIVE TYPING RELAY against the WINDOW FOLD: a preview opens on whichever
// surface the record it previews will land on to retire it — the top-level feed
// when nothing folds it, and the window's own bubble when something does.
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

func TestConsumeStillRelaysTypingInsideAnOpenWindow(t *testing.T) {
	// Arrange: a skill window is open, and the agent inside it starts a call.
	// Refusing the relay here is what silenced a whole turn's live typing.
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})
	openTestSkillWindow(t, c, "toolu_skill")

	// Act.
	if err := c.Consume(inputDelta("toolu_inner", `{"comm`)); err != nil {
		t.Fatalf("Consume: %v", err)
	}

	// Assert.
	if len(push.typing) != 1 {
		t.Fatalf("typing pushes = %d, want 1 — an open window must not silence live typing", len(push.typing))
	}
}

func TestConsumeScopesFoldedTypingToItsBubble(t *testing.T) {
	// Arrange: the record this delta previews folds into the window's bubble,
	// so a TOP-LEVEL preview of it could never be retired.
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})
	openTestSkillWindow(t, c, "toolu_skill")

	// Act.
	if err := c.Consume(inputDelta("toolu_inner", `{"comm`)); err != nil {
		t.Fatalf("Consume: %v", err)
	}

	// Assert: it is addressed to the bubble that will retire it.
	if len(push.typing) != 1 {
		t.Fatalf("typing pushes = %d, want 1", len(push.typing))
	}
	want := c.bubbles.windowFoldTargets()
	if len(want) == 0 {
		t.Fatal("no window fold target open")
	}
	if got := push.typing[0].GetBubbleId(); got != want[len(want)-1].bubbleID {
		t.Errorf("bubble_id = %q, want %q", got, want[len(want)-1].bubbleID)
	}
}

func TestConsumeLeavesTopLevelPreviewsUnscoped(t *testing.T) {
	// Arrange: nothing folds, so the record lands on the feed and retires its
	// preview there. A bubble id here would hide a real preview in a bubble.
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})

	// Act.
	if err := c.Consume(inputDelta("toolu_call", `{"comm`)); err != nil {
		t.Fatalf("Consume: %v", err)
	}

	// Assert.
	if len(push.typing) != 1 {
		t.Fatalf("typing pushes = %d, want 1", len(push.typing))
	}
	if got := push.typing[0].GetBubbleId(); got != "" {
		t.Errorf("bubble_id = %q, want empty for a top-level preview", got)
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
	if got := push.typing[0].GetBubbleId(); got != "" {
		t.Errorf("bubble_id = %q, want empty — this card stays on the feed", got)
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
