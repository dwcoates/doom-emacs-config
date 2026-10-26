package chat

import (
	"context"
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// A THREAD'S NAME: its first question's abbreviation, applied off the lock in
// the order it was wanted.

// named waits for the name drains and answers every name handed to Deps.Name.
func (f *fixture) named() []string {
	f.t.Helper()
	f.svc.WaitNames()
	f.namer.mu.Lock()
	defer f.namer.mu.Unlock()
	return append([]string(nil), f.namer.names...)
}

func TestOwnName(t *testing.T) {
	cases := []struct {
		question string
		want     string
		own      bool
	}{
		{"What are quarks?", "What are quarks?", true},
		{"  spaced   out  ", "spaced out", true},
		{longQuestion, "", false},
	}
	for _, tc := range cases {
		t.Run(tc.question, func(t *testing.T) {
			// Act
			got, own := OwnName(tc.question)

			// Assert
			if got != tc.want || own != tc.own {
				t.Fatalf("OwnName(%q) = %q, %v; want %q, %v", tc.question, got, own, tc.want, tc.own)
			}
		})
	}
}

func TestAShortFirstQuestionNamesTheThread(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.open()

	// Act
	f.answer("turn-1", "What are quarks?", "")

	// Assert
	if got := f.named(); !equal(got, []string{"What are quarks?"}) {
		t.Fatalf("names = %v, want the question itself", got)
	}
}

func TestALongFirstQuestionNamesTheThreadByItsAbbreviation(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.open()

	// Act
	f.answer("turn-1", longQuestion, "")

	// Assert
	if got := f.named(); !equal(got, []string{"short: Why"}) {
		t.Fatalf("names = %v, want the abbreviation alone", got)
	}
}

func TestTheNameArrivesBeforeTheAnswer(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.open()
	f.turns.ask("turn-1", longQuestion, "", "")

	// Act: asked, and the abbreviation minted, but nothing answered.
	if err := f.svc.Asked(context.Background(), thr, nil); err != nil {
		t.Fatal(err)
	}

	// Assert
	if got := f.named(); !equal(got, []string{"short: Why"}) {
		t.Fatalf("names = %v, want the thread named while its first answer is in flight", got)
	}
}

func TestAHeldShortFirstQuestionNamesTheThread(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.turns.held = []wsm.HeldPrompt{heldAsk("What are quarks?", "")}

	// Act
	f.open()

	// Assert
	if got := f.named(); !equal(got, []string{"What are quarks?"}) {
		t.Fatalf("names = %v, want the held question itself", got)
	}
}

func TestLaterQuestionsDoNotRenameTheThread(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.open()
	f.answer("turn-1", "What are quarks?", "")

	// Act
	f.answer("turn-2", longQuestion, "n1")

	// Assert
	if got := f.named(); !equal(got, []string{"What are quarks?"}) {
		t.Fatalf("names = %v, want only the first question's", got)
	}
}

func TestAStoredThreadIsNamedAfterItsRootWhenLoaded(t *testing.T) {
	// Arrange: a root a stopped daemon recorded with its abbreviation.
	f := newFixture(t)
	f.zettel.seed("", 0)
	short := "Fusion at iron"
	f.zettel.nodes[0].Question.Text = longQuestion
	f.zettel.nodes[0].Question.Abbreviation = &short

	// Act
	f.open()

	// Assert
	if got := f.named(); !equal(got, []string{"Fusion at iron"}) || len(f.abbrev.asked) != 0 {
		t.Fatalf("names = %v, asked = %v; want the stored abbreviation, with no call", got, f.abbrev.asked)
	}
}

func TestAnUnabbreviatedFirstQuestionNamesTheThreadAsAsked(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.abbrev.err = errors.New("the model is down")
	f.open()

	// Act
	f.answer("turn-1", "Why does fusion in a star\nstop once it reaches iron?", "")

	// Assert
	if got := f.named(); !equal(got, []string{"Why does fusion in a star stop once it reaches iron?"}) {
		t.Fatalf("names = %v, want the question on one line", got)
	}
}

func TestAFailedNameIsRecorded(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.namer.err = errors.New("the registry is down")
	f.open()

	// Act
	f.answer("turn-1", "What are quarks?", "")
	f.named()

	// Assert
	r, ok := dlog.FindRecord(f.log.Records(), dlog.RecordQuery{Level: "error", Operation: opName})
	if !ok || r.Context["thread"] != string(thr) || r.Context["name"] != "What are quarks?" || r.Context["cause"] != "the registry is down" {
		t.Fatalf("record = %+v, found %v; want the failure with the thread, name and cause", r, ok)
	}
}

// heldAsk is a held thread question saying TEXT, asked of PARENT ("" for the
// thread's start).
func heldAsk(text, parent string) wsm.HeldPrompt {
	return wsm.HeldPrompt{
		Turn: "turn-h", Ask: &wsm.ThreadAsk{Parent: parentNode(parent, "")},
		Said: &conversationv1.UserSaid{Content: &conversationv1.UserContent{Blocks: []*conversationv1.UserContentBlock{
			{Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: text}}},
		}}},
	}
}
