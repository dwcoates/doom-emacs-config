package feed

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

// A DIRECTIVE IS RECOGNISED FROM ITS OWN PROMPT TEXT, so the suppression of a
// /clear or /compact turn's bubbles holds on a fresh resolver's history replay —
// where directiveTurns starts empty and the prompt is drawn before the cut.

func TestIsContextCutDirectiveRecognisesTheCommandLiterals(t *testing.T) {
	tests := []struct {
		name string
		text string
		want bool
	}{
		{name: "a bare /clear", text: "/clear", want: true},
		{name: "a bare /compact", text: "/compact", want: true},
		{name: "a /compact with instructions", text: "/compact focus on the tests", want: true},
		{name: "a /clear with trailing text", text: "/clear foo", want: true},
		{name: "a /compact with instructions on the next line", text: "/compact\nfocus on the tests", want: true},
		{name: "an ordinary prompt", text: "clear the build please", want: false},
		{name: "a prompt merely containing the word", text: "please /clear it", want: false},
		{name: "a look-alike command", text: "/clearing", want: false},
		{name: "empty text", text: "", want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange, Act.
			got := isContextCutDirective(textSaid(tt.text))

			// Assert.
			if got != tt.want {
				t.Fatalf("isContextCutDirective(%q) = %v, want %v", tt.text, got, tt.want)
			}
		})
	}
}

// textSaid builds a one-block user prompt, the shape the daemon composes a
// directive's said in.
func textSaid(text string) *conversationv1.UserSaid {
	return &conversationv1.UserSaid{Content: &conversationv1.UserContent{
		Blocks: []*conversationv1.UserContentBlock{{
			Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: text}},
		}},
	}}
}

// replayClearTurn drives a /clear turn's frames through the resolver in STORE
// ORDER — prompt, empty response, cut, interrupted terminal — with NO prior
// OnClearReceived, which is exactly a fresh resolver replaying recorded history.
func (h *harness) replayClearTurn(turn, command string) {
	h.t.Helper()
	h.resolver.OnPrompt(testWorkspace, mainAgent(), &conversationv1.AgentPrompt{
		Id:     &conversationv1.TurnId{Value: turn},
		Agent:  mainAgent(),
		Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
		Said:   textSaid(command),
	}, nil)
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-"+turn, &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: ""},
		}, nil), nil, nil)
}

// A REPLAYED /clear SHOWS ONLY ITS DIVIDER. On a fresh resolver the prompt is
// drawn before the cut, yet its own text registers the directive, so no prompt,
// response, or terminal bubble is drawn — only the divider.
func TestAReplayedClearShowsOnlyItsDivider(t *testing.T) {
	// Arrange, Act: replay the /clear turn's frames in store order.
	h := newHarness(t)
	h.replayClearTurn("turn-2", "/clear")
	h.cutAt("entry-clear", clearedContextCut())
	h.terminal("turn-2", interruptedByUser(), nil)

	// Assert: exactly one divider, and no prompt/response/terminal bubble.
	if got := len(h.separationRows()); got != 1 {
		t.Fatalf("separation rows = %d, want the divider", got)
	}
	if got := len(h.userPromptRows()); got != 0 {
		t.Fatalf("user-prompt rows = %d, want none on replay of a /clear", got)
	}
	if got := len(h.responseRows()); got != 0 {
		t.Fatalf("response rows = %d, want none on replay of a /clear", got)
	}
	if h.hasTerminalRow("turn-2") {
		t.Fatal("a replayed /clear drew a terminal bubble")
	}
}

// A REPLAYED /compact SHOWS ITS DIVIDER AND SUMMARY, NO PROMPT OR RESPONSE. The
// compaction's summary rides its own divider; the directive's prompt and empty
// response draw nothing.
func TestAReplayedCompactShowsOnlyItsDividerAndSummary(t *testing.T) {
	// Arrange, Act: replay the /compact turn's frames in store order.
	h := newHarness(t)
	h.replayClearTurn("turn-2", "/compact")
	h.cutAt("entry-compact", compactedCut("what survived"))

	// Assert: the compacted divider carries its summary, and no prompt/response.
	sep := h.separationRow().GetSeparation()
	if sep.GetCompacted().GetSummary().GetMarkdown() != "what survived" {
		t.Fatalf("compacted summary = %q, want the survived account", sep.GetCompacted().GetSummary().GetMarkdown())
	}
	if got := len(h.userPromptRows()); got != 0 {
		t.Fatalf("user-prompt rows = %d, want none on replay of a /compact", got)
	}
	if got := len(h.responseRows()); got != 0 {
		t.Fatalf("response rows = %d, want none on replay of a /compact", got)
	}
}

// A LATE PROMPT/RESPONSE FOR A REPLAYED DIRECTIVE STAYS SUPPRESSED. The other
// plane re-delivers them after the terminal, with no turn in flight; the
// per-turn and per-unit marks the first delivery left keep them dropped.
func TestAReplayedDirectivesLateFramesStaySuppressed(t *testing.T) {
	// Arrange: a replayed /clear turn that has ended.
	h := newHarness(t)
	h.replayClearTurn("turn-2", "/clear")
	h.cutAt("entry-clear", clearedContextCut())
	h.terminal("turn-2", interruptedByUser(), nil)

	// Act: the file plane re-delivers the prompt and response after the terminal.
	h.resolver.OnPrompt(testWorkspace, mainAgent(), &conversationv1.AgentPrompt{
		Id:     &conversationv1.TurnId{Value: "turn-2"},
		Agent:  mainAgent(),
		Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
		Said:   textSaid("/clear"),
	}, nil)
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-turn-2", &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: ""},
		}, nil), nil, nil)

	// Assert: still nothing but the divider.
	if got := len(h.userPromptRows()); got != 0 {
		t.Fatalf("user-prompt rows = %d, want none however late the plane delivers", got)
	}
	if got := len(h.responseRows()); got != 0 {
		t.Fatalf("response rows = %d, want none however late the plane delivers", got)
	}
}
