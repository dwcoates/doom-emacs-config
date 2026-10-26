package feed

import (
	"context"
	"os"
	"slices"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/replyquote"
)

// A COMPOSER'S ENTRIES target responses by the rule a sent prompt's entries
// are bound by (entryTarget), read as if the prompt were the next row.

// composerTree is a response tree whose nodes give every neighbor case:
// 1 has children 1.1 and 1.2, 1.2 has 1.2.1, and 2 has 2.1.
const composerTree = "1 🔧 One\n├── 1.1 a\n└── 1.2 b\n    └── 1.2.1 c\n\n2 ✅ Two\n└── 2.1 d"

// twoAnsweredHarness draws two turns, answer-1 holding answeredTree and
// answer-2 holding composerTree, both of which hold the labels 1, 1.1, 1.2.
func twoAnsweredHarness(t *testing.T) *harness {
	t.Helper()
	h := answeredHarness(t)
	h.deliverPrompt("turn-2", "and more")
	h.send(responseSuccessActivity("answer-2", composerTree))
	return h
}

// targetValues is TARGETS as row id values, "" for none.
func targetValues(targets []*frontendv1.FeedId) []string {
	out := make([]string, len(targets))
	for i, t := range targets {
		out[i] = t.GetValue()
	}
	return out
}

func TestComposerTargetsAreTheNewestResponseHoldingEachLabel(t *testing.T) {
	// Arrange
	h := twoAnsweredHarness(t)

	// Act
	got := targetValues(h.resolver.ComposerTargets(testWorkspace, nil, []string{"1.1", "2.1", "9.9"}))

	// Assert: answer-2 is newest and holds 1.1 and 2.1; nothing holds 9.9.
	want := []string{h.responseRowID("answer-2"), h.responseRowID("answer-2"), ""}
	if !slices.Equal(got, want) {
		t.Fatalf("targets = %v, want %v", got, want)
	}
}

func TestComposerTargetsReachPastANewerResponseWithoutTheLabel(t *testing.T) {
	// Arrange: answer-2 holds no tree.
	h := answeredHarness(t)
	h.deliverPrompt("turn-2", "and more")
	h.send(responseSuccessActivity("answer-2", "just words"))

	// Act
	got := targetValues(h.resolver.ComposerTargets(testWorkspace, nil, []string{"1.1"}))

	// Assert
	if want := []string{h.responseRowID("answer-1")}; !slices.Equal(got, want) {
		t.Fatalf("targets = %v, want the older response holding the label %v", got, want)
	}
}

func TestComposerTargetsOfAReplyAreTheRepliedResponse(t *testing.T) {
	// Arrange
	h := twoAnsweredHarness(t)
	reply := &frontendv1.FeedId{Value: h.responseRowID("answer-1")}

	// Act: 1.1 is in both; 2.1 only in the newer answer-2.
	got := targetValues(h.resolver.ComposerTargets(testWorkspace, reply, []string{"1.1", "2.1"}))

	// Assert: the reply's response answers what it holds, and nothing else does.
	if want := []string{h.responseRowID("answer-1"), ""}; !slices.Equal(got, want) {
		t.Fatalf("targets = %v, want the replied response or nothing %v", got, want)
	}
}

func TestComposerTargetsOfAReplyToAPromptAreBoundAsAnyPromptIs(t *testing.T) {
	// Arrange: a reply to a prompt, which holds no tree.
	h := twoAnsweredHarness(t)
	reply := &frontendv1.FeedId{Value: h.promptRowID("turn-2")}

	// Act
	got := targetValues(h.resolver.ComposerTargets(testWorkspace, reply, []string{"1.1"}))

	// Assert
	if want := []string{h.responseRowID("answer-2")}; !slices.Equal(got, want) {
		t.Fatalf("targets = %v, want the newest response holding the label %v", got, want)
	}
}

func TestComposerTargetsOfAnUnknownWorkspaceAreNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	got := targetValues(h.resolver.ComposerTargets("ws-unknown", nil, []string{"1.1"}))

	// Assert
	if !slices.Equal(got, []string{""}) {
		t.Fatalf("targets = %v, want nothing", got)
	}
}

func TestEntryNeighborWalksTheTargetTree(t *testing.T) {
	tests := []struct {
		name   string
		label  string
		way    NeighborWay
		want   string
		wantOK bool
	}{
		{name: "next is the next sibling", label: "1.1", way: NeighborNext, want: "1.2", wantOK: true},
		{name: "next climbs to the parent's next sibling", label: "1.2.1", way: NeighborNext, want: "2", wantOK: true},
		{name: "next of a root is the next root", label: "1", way: NeighborNext, want: "2", wantOK: true},
		{name: "next of the tree's last node is none", label: "2.1", way: NeighborNext},
		{name: "previous is the previous sibling", label: "1.2", way: NeighborPrevious, want: "1.1", wantOK: true},
		{name: "previous of a first child is its parent", label: "1.1", way: NeighborPrevious, want: "1", wantOK: true},
		{name: "previous of the first root is none", label: "1", way: NeighborPrevious},
		{name: "following a root is its first child", label: "1", way: NeighborFollowing, want: "1.1", wantOK: true},
		{name: "following a branch is its first child", label: "1.2", way: NeighborFollowing, want: "1.2.1", wantOK: true},
		{name: "following a leaf is its next sibling", label: "1.1", way: NeighborFollowing, want: "1.2", wantOK: true},
		{name: "following a last leaf climbs to its aunt", label: "1.2.1", way: NeighborFollowing, want: "2", wantOK: true},
		{name: "following the tree's last node is none", label: "2.1", way: NeighborFollowing},
		{name: "the parent of a branch's child", label: "1.2.1", way: NeighborParent, want: "1.2", wantOK: true},
		{name: "the parent of a root's child is the root", label: "2.1", way: NeighborParent, want: "2", wantOK: true},
		{name: "a root has no parent", label: "1", way: NeighborParent},
		{name: "a label no response holds has no neighbor", label: "9.9", way: NeighborNext},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := twoAnsweredHarness(t)

			// Act
			got, ok := h.resolver.EntryNeighbor(testWorkspace, nil, tc.label, tc.way)

			// Assert
			if got != tc.want || ok != tc.wantOK {
				t.Fatalf("EntryNeighbor(%q, %v) = (%q, %v), want (%q, %v)", tc.label, tc.way, got, ok, tc.want, tc.wantOK)
			}
		})
	}
}

func TestEntryNeighborOfAReplyWalksTheRepliedTree(t *testing.T) {
	// Arrange: answer-1's tree is 1, 1.1, 1.2; answer-2's has 1.2.1 under 1.2.
	h := twoAnsweredHarness(t)
	reply := &frontendv1.FeedId{Value: h.responseRowID("answer-1")}

	// Act
	got, ok := h.resolver.EntryNeighbor(testWorkspace, reply, "1.2", NeighborNext)

	// Assert: answer-1 has no node after 1.2.
	if ok {
		t.Fatalf("EntryNeighbor in the replied tree = (%q, true), want none", got)
	}
}

func TestAReplysEntriesAnswerTheQuotedResponse(t *testing.T) {
	// Arrange
	h := twoAnsweredHarness(t)
	quoted := h.responseRowID("answer-1")

	// Act: a reply quoting answer-1 answers 1.1, which answer-2 holds too.
	h.promptWith("turn-3", conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
		replyquote.Block("the old tree", false, quoted), textBlock("1.1 - this one"))

	// Assert
	older := answerPairs(h.responseOn(rootFeed(), "answer-1").GetAnswers())
	newer := answerPairs(h.responseOn(rootFeed(), "answer-2").GetAnswers())
	if want := [][2]string{{"1.1", h.promptRowID("turn-3")}}; !equalPairs(older, want) || len(newer) != 0 {
		t.Fatalf("answer-1's answers = %v, answer-2's = %v; want the quoted one alone answered %v", older, newer, want)
	}
}

func TestAQuoteDrawsTheRowItQuotes(t *testing.T) {
	// Arrange
	h := answeredHarness(t)
	quoted := h.responseRowID("answer-1")

	// Act
	h.promptWith("turn-2", conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
		replyquote.Block("the old tree", false, quoted), textBlock("thanks"))

	// Assert
	if got := promptQuotedRow(h.promptRow("turn-2")); got != quoted {
		t.Fatalf("the drawn quote's row = %q, want the quoted response %q", got, quoted)
	}
}

func TestAQuoteRecordedWithoutItsRowDrawsNone(t *testing.T) {
	// Arrange
	h := answeredHarness(t)

	// Act
	h.promptWith("turn-2", conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT, quoteBlock("the old tree"), textBlock("thanks"))

	// Assert
	if got := promptQuotedRow(h.promptRow("turn-2")); got != "" {
		t.Fatalf("the drawn quote's row = %q, want none", got)
	}
}

func TestALabelChangeIsAnnouncedOnTheRootFeed(t *testing.T) {
	// Arrange
	h := answeredHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	changes := h.resolver.WatchLabelChanges(ctx)

	// Act
	h.deliverPrompt("turn-2", "and more")
	h.send(responseSuccessActivity("answer-2", composerTree))

	// Assert
	if ws := <-changes; ws != testWorkspace {
		t.Fatalf("announced %q, want the workspace whose response tree was drawn", ws)
	}
}

func TestEveryComposerReadingTargetsThroughComposerTarget(t *testing.T) {
	// Arrange
	src, err := os.ReadFile("composer.go")
	if err != nil {
		t.Fatalf("read composer.go: %v", err)
	}

	// Act
	direct := strings.Count(string(src), "entryTarget(f, len(f.order)")
	shared := strings.Count(string(src), "composerTarget(f, reply, label)")

	// Assert: composerTarget's own call is the one direct reading.
	if direct != 1 || shared != 2 {
		t.Fatalf("composer.go reads entryTarget directly %d times and through composerTarget %d times, want 1 and 2", direct, shared)
	}
}

func TestResponseNodeAnswersWhetherAResponsesTreeHoldsALabel(t *testing.T) {
	tests := []struct {
		name  string
		row   func(*harness) string
		label string
		want  bool
	}{
		{name: "a node the response's tree holds", row: func(h *harness) string { return h.responseRowID("answer-1") }, label: "1.1", want: true},
		{name: "a label the response's tree does not hold", row: func(h *harness) string { return h.responseRowID("answer-1") }, label: "9.9"},
		{name: "a prompt row", row: func(h *harness) string { return h.promptRowID("turn-1") }, label: "1.1"},
		{name: "a row the feed does not hold", row: func(*harness) string { return "missing" }, label: "1.1"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := answeredHarness(t)

			// Act
			got := h.resolver.ResponseNode(testWorkspace, &frontendv1.FeedId{Value: tc.row(h)}, tc.label)

			// Assert
			if got != tc.want {
				t.Fatalf("ResponseNode(%q) = %v, want %v", tc.label, got, tc.want)
			}
		})
	}
}

func TestNeighborWayNamesEachWayAndRefusesAnyOther(t *testing.T) {
	tests := []struct {
		way  NeighborWay
		want string
	}{
		{NeighborNext, "next"},
		{NeighborPrevious, "previous"},
		{NeighborFollowing, "following"},
		{NeighborParent, "parent"},
	}
	for _, tc := range tests {
		t.Run(tc.want, func(t *testing.T) {
			// Act + Assert
			if got := tc.way.String(); got != tc.want {
				t.Fatalf("String() = %q, want %q", got, tc.want)
			}
		})
	}
	t.Run("an unnamed way panics", func(t *testing.T) {
		// Arrange
		defer func() {
			if recover() == nil {
				t.Fatal("NeighborWay(9).String() returned, want a panic")
			}
		}()

		// Act
		_ = NeighborWay(9).String()
	})
}
