package feed

import (
	"os"
	"slices"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/prose"
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

// entryRow is a user prompt row whose one text block is the daemon's parse
// of a line of prose and then ENTRIES, each a (label, words) pair, one line
// each.
func entryRow(entries ...[2]string) *frontendv1.FeedRow {
	text := "intro\n"
	for _, e := range entries {
		text += e[0] + " - " + e[1] + "\n"
	}
	return &frontendv1.FeedRow{Row: &frontendv1.FeedRow_UserPrompt{UserPrompt: &frontendv1.FeedUserPrompt{
		Result: &frontendv1.FeedUserPrompt_Success{Success: &frontendv1.FeedUserPromptSuccess{
			Body: &frontendv1.FeedUserPromptBody{Blocks: []*frontendv1.FeedUserPromptBlock{{
				Block: &frontendv1.FeedUserPromptBlock_Text{Text: &frontendv1.FeedTextBlock{Prose: prose.ParsePrompt(dlog.NewTestLogger(), text)}},
			}}},
		}},
	}}}
}

func TestPromptEntriesReadsAPromptsEntriesInOrder(t *testing.T) {
	tests := []struct {
		name string
		row  *frontendv1.FeedRow
		want []string
	}{
		{"a prompt's entries, plain blocks left out", entryRow([2]string{"1.2", "b"}, [2]string{"1.1", "a"}), []string{"1.2", "1.1"}},
		{"a prompt holding no entry", entryRow(), nil},
		{"a row that is no prompt", &frontendv1.FeedRow{}, nil},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			var got []string
			for _, e := range promptEntries(tc.row) {
				got = append(got, e.GetLabel().GetText())
			}

			// Assert
			if !slices.Equal(got, tc.want) || !slices.Equal(promptEntryLabels(tc.row), tc.want) {
				t.Fatalf("promptEntries labels = %v, promptEntryLabels = %v, want %v", got, promptEntryLabels(tc.row), tc.want)
			}
		})
	}
}

func TestPromptEntryTextReadsTheFirstEntryAnsweringALabel(t *testing.T) {
	tests := []struct {
		name   string
		label  string
		want   string
		wantOK bool
	}{
		{"an entry answering the label", "1.1", "first", true},
		{"a label two entries answer", "1.2", "second", true},
		{"a label no entry answers", "9.9", "", false},
	}
	row := entryRow([2]string{"1.1", "first"}, [2]string{"1.2", "second"}, [2]string{"1.2", "again"})
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, ok := promptEntryText(row, tc.label)

			// Assert
			if got != tc.want || ok != tc.wantOK {
				t.Fatalf("promptEntryText(%q) = (%q, %v), want (%q, %v)", tc.label, got, ok, tc.want, tc.wantOK)
			}
		})
	}
}

func TestPromptEntryReadsAPublishedPromptsEntry(t *testing.T) {
	// Arrange
	h := answeredHarness(t)
	h.deliverPrompt("turn-2", "1.1 - yes, do that")
	row := &frontendv1.FeedId{Value: h.promptRowID("turn-2")}

	// Act
	got, ok := h.resolver.PromptEntry(testWorkspace, row, "1.1")
	_, missing := h.resolver.PromptEntry(testWorkspace, row, "1.2")

	// Assert
	if got != "yes, do that" || !ok || missing {
		t.Fatalf("PromptEntry = (%q, %v), absent label ok = %v; want (\"yes, do that\", true) and false", got, ok, missing)
	}
}

func TestEveryReaderOfAPromptsEntriesWalksThemThroughPromptEntries(t *testing.T) {
	// Arrange
	src, err := os.ReadFile("answers.go")
	if err != nil {
		t.Fatalf("read answers.go: %v", err)
	}

	// Act
	walks := strings.Count(string(src), ".GetEntries()")
	calls := strings.Count(string(src), "promptEntries(row)")

	// Assert: the one walk is promptEntries' own, and both readers call it.
	if walks != 1 || calls != 2 {
		t.Fatalf("answers.go reads GetEntries() %d times and calls promptEntries %d times, want 1 and 2", walks, calls)
	}
}
