package feed

import (
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"
)

// A RESPONSE'S ANSWERS are the prompts whose entries answer its tree's labels:
// an entry answers the nearest response before its prompt, on the same feed,
// whose tree holds the entry's label (answers.go).

// answeredTree is a response tree with the labels 1, 1.1 and 1.2.
const answeredTree = "1 🔧 Did it\n├── 1.1 first\n└── 1.2 second"

// answerPairs is ANSWERS as (label, prompt row id) pairs, for comparison.
func answerPairs(answers []*frontendv1.FeedResponseAnswer) [][2]string {
	var out [][2]string
	for _, a := range answers {
		out = append(out, [2]string{a.GetLabel(), a.GetPrompt().GetValue()})
	}
	return out
}

// answeredHarness draws one turn whose answer is answeredTree.
func answeredHarness(t *testing.T) *harness {
	t.Helper()
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")
	h.send(responseSuccessActivity("answer-1", answeredTree))
	return h
}

func TestAnEntryLinksTheLabelItAnswers(t *testing.T) {
	// Arrange
	h := answeredHarness(t)

	// Act
	h.deliverPrompt("turn-2", "1.1 - yes, do that")

	// Assert
	got := answerPairs(h.responseOn(rootFeed(), "answer-1").GetAnswers())
	if want := [][2]string{{"1.1", h.promptRowID("turn-2")}}; !equalPairs(got, want) {
		t.Fatalf("answers = %v, want %v", got, want)
	}
}

func TestEveryEntryOfAPromptIsAnAnswerInOrder(t *testing.T) {
	// Arrange
	h := answeredHarness(t)

	// Act
	h.deliverPrompt("turn-2", "1.2 - second first\n1.1 - then the first")

	// Assert
	got := answerPairs(h.responseOn(rootFeed(), "answer-1").GetAnswers())
	if want := [][2]string{{"1.2", h.promptRowID("turn-2")}, {"1.1", h.promptRowID("turn-2")}}; !equalPairs(got, want) {
		t.Fatalf("answers = %v, want %v", got, want)
	}
}

func TestAnEntryAnswersTheNearestResponseHoldingItsLabel(t *testing.T) {
	// Arrange: a second turn answers with a tree holding 1.1 too.
	h := answeredHarness(t)
	h.deliverPrompt("turn-2", "go on")
	h.send(responseSuccessActivity("answer-2", "1 🔎 More\n└── 1.1 again"))

	// Act
	h.deliverPrompt("turn-3", "1.1 - this one")

	// Assert
	older, newer := answerPairs(h.responseOn(rootFeed(), "answer-1").GetAnswers()), answerPairs(h.responseOn(rootFeed(), "answer-2").GetAnswers())
	if len(older) != 0 || !equalPairs(newer, [][2]string{{"1.1", h.promptRowID("turn-3")}}) {
		t.Fatalf("answers = %v on the older response and %v on the newer, want the newer alone answered", older, newer)
	}
}

func TestAnEntryReachesPastANewerResponseWithoutItsLabel(t *testing.T) {
	// Arrange: a second turn answers with plain words.
	h := answeredHarness(t)
	h.deliverPrompt("turn-2", "go on")
	h.send(responseSuccessActivity("answer-2", "done, nothing more"))

	// Act
	h.deliverPrompt("turn-3", "1.2 - about that")

	// Assert
	if got := answerPairs(h.responseOn(rootFeed(), "answer-1").GetAnswers()); !equalPairs(got, [][2]string{{"1.2", h.promptRowID("turn-3")}}) {
		t.Fatalf("answers = %v, want the older response answered", got)
	}
}

func TestAnEntryNoTreeHoldsLinksNothing(t *testing.T) {
	// Arrange
	h := answeredHarness(t)

	// Act
	h.deliverPrompt("turn-2", "9.9 - nowhere")

	// Assert
	if got := h.responseOn(rootFeed(), "answer-1").GetAnswers(); len(got) != 0 {
		t.Fatalf("answers = %v, want none", got)
	}
}

func TestAResponseWithNoTreeIsAnsweredByNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")
	h.send(responseSuccessActivity("answer-1", "plain words, no tree"))

	// Act
	h.deliverPrompt("turn-2", "1.1 - yes")

	// Assert
	if got := h.responseOn(rootFeed(), "answer-1").GetAnswers(); len(got) != 0 {
		t.Fatalf("answers = %v, want none", got)
	}
}

func TestRemovingAnAnsweringPromptDropsItsAnswer(t *testing.T) {
	// Arrange
	h := answeredHarness(t)
	h.deliverPrompt("turn-2", "1.1 - yes")

	// Act: a rollback removes the prompt's row.
	h.resolver.mu.Lock()
	h.resolver.retireRolledBack(h.resolver.state(testWorkspace), rootFeed(), h.promptRowID("turn-2"))
	h.resolver.mu.Unlock()

	// Assert
	if got := h.responseOn(rootFeed(), "answer-1").GetAnswers(); len(got) != 0 {
		t.Fatalf("answers = %v, want none once the prompt is gone", got)
	}
}

func TestRestatingAnswersIsRecordedAtDebug(t *testing.T) {
	// Arrange
	h := answeredHarness(t)

	// Act
	h.deliverPrompt("turn-2", "1.1 - yes")

	// Assert
	if !h.hasRecord("debug", "daemon.feed.answers_restated") {
		t.Fatal("restating answers recorded no DEBUG daemon.feed.answers_restated")
	}
}

// equalPairs reports whether GOT and WANT hold the same pairs in order.
func equalPairs(got, want [][2]string) bool {
	if len(got) != len(want) {
		return false
	}
	for i := range got {
		if got[i] != want[i] {
			return false
		}
	}
	return true
}
