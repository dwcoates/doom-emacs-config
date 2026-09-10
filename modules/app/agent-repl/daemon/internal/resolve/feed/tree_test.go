package feed

import (
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

// THE SETTLED TREE IS WRAPPED ONCE, HERE. A response under the metaprompt is
// one bare Unicode tree; the daemon wraps it to the column limit before
// either client sees it, with the owner's formatter ported one to one.

// settle delivers one settled response carrying markdown and answers what the
// bubble now holds.
func (h *harness) settle(markdown string) string {
	h.t.Helper()
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseSuccess{
			Prose: &conversationv1.AgentResponseProse{Markdown: markdown},
		}, nil), noAddress())
	return h.response().GetSuccess().GetProse().GetMarkdown()
}

func TestASettledTreeWiderThanTheLimitIsServedWrapped(t *testing.T) {
	// Arrange: one branch whose body runs past 105 columns.
	h := newHarness(t)
	long := "├── 1.1. " + strings.Repeat("word ", 30)

	// Act.
	got := h.settle("1. 🎯 Root\n" + strings.TrimSpace(long))

	// Assert: the branch broke into continuation lines under its own text
	// column, every line inside the limit, and nothing was lost.
	lines := strings.Split(got, "\n")
	if len(lines) < 3 {
		t.Fatalf("settled tree was not wrapped: %q", got)
	}
	if !strings.HasPrefix(lines[2], "│        word") {
		t.Fatalf("continuation does not hang under the text column: %q", lines[2])
	}
	if strings.Count(got, "word") != 30 {
		t.Fatalf("words lost or duplicated in %q", got)
	}
}

func TestASettledTreeInsideTheLimitIsServedUntouched(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	tree := "1. 🎯 Root\n├── 1.1. Short.\n└── 1.2. Also short."
	got := h.settle(tree)

	// Assert.
	if got != tree {
		t.Fatalf("settled tree changed: %q", got)
	}
}

func TestAStreamingDeltaIsNotWrapped(t *testing.T) {
	// Arrange: a start, then one delta wider than the limit.
	h := newHarness(t)
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseStart{}, nil), noAddress())
	delta := "├── 1.1. " + strings.TrimSpace(strings.Repeat("word ", 30))

	// Act.
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseUpdate{NewMarkdown: delta}, nil), noAddress())

	// Assert: a fragment of a line the formatter has not seen the end of is
	// drawn as it arrived.
	if got := h.response().GetUpdate().GetProse().GetMarkdown(); got != delta {
		t.Fatalf("delta was rewritten: %q", got)
	}
}

func TestAFencedCodeBlockBeneathABulletPassesThroughOpaque(t *testing.T) {
	// Arrange: a code line that would parse as a branch if it were not code.
	h := newHarness(t)
	code := "1. " + strings.TrimSpace(strings.Repeat("code ", 30))
	tree := "1. 🎯 Root\n└── 1.1. The block:\n```go\n" + code + "\n```"

	// Act.
	got := h.settle(tree)

	// Assert: the code line is exactly as it arrived, fence and all.
	if !strings.Contains(got, "\n"+code+"\n") {
		t.Fatalf("code line was wrapped: %q", got)
	}
	if !strings.HasSuffix(got, "\n```") {
		t.Fatalf("fence lost: %q", got)
	}
}

func TestATreeTheFormatterRefusesIsServedAsItArrivedAndWarned(t *testing.T) {
	// Arrange: a prefix that alone exceeds the limit — the one condition the
	// formatter refuses outright.
	h := newHarness(t)
	tree := strings.Repeat("│   ", 26) + "├── 1.1. text"

	// Act.
	got := h.settle(tree)

	// Assert: served verbatim, and the refusal is on the record as a WARN
	// carrying the formatter's own sentence.
	if got != tree {
		t.Fatalf("refused tree was altered: %q", got)
	}
	if !h.hasRecord("warn", "daemon.feed.response_tree_unformattable") {
		t.Fatalf("records = %+v, want a WARN daemon.feed.response_tree_unformattable", h.records())
	}
}

func TestAWordWiderThanTheLimitIsServedWiderAndRecordedAtDebug(t *testing.T) {
	// Arrange: one unsplittable word past the limit.
	h := newHarness(t)
	word := strings.Repeat("a", 120)

	// Act.
	got := h.settle("1. " + word)

	// Assert: the word is intact, the line is wider than the limit, and the
	// only record is DEBUG — nothing was truncated, so nothing warns.
	if !strings.Contains(got, word) {
		t.Fatalf("word truncated: %q", got)
	}
	if !h.hasRecord("debug", "daemon.feed.response_tree_overflow") {
		t.Fatalf("records = %+v, want a DEBUG daemon.feed.response_tree_overflow", h.records())
	}
	if h.hasRecord("warn", "daemon.feed.response_tree_unformattable") {
		t.Fatalf("an overflow word must not warn: %+v", h.records())
	}
}
