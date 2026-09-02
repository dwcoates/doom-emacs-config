package promptqueue

import (
	"context"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

func TestNewRefusesEachMissingCollaborator(t *testing.T) {
	// Arrange: one complete set of dependencies, then each one blanked.
	full := func() Deps {
		h := &harness{db: newFakeDB(), sender: newFakeSender(), watcher: &fakeWatcher{},
			feed: &fakeFeed{}, footer: &fakeFooter{}, holds: &fakeHolds{}, judge: &scriptedJudge{}}
		return Deps{
			DB: h.db, Judge: h.judge, Feed: h.feed, Footer: h.footer, Holds: h.holds,
			Client:  func(ids.WorkspaceID) (Sender, bool) { return h.sender, true },
			Watcher: func(ids.WorkspaceID) (Watcher, bool) { return h.watcher, true },
			Log:     dlog.NewTestSurfaces(),
		}
	}
	tests := []struct {
		name  string
		blank func(*Deps)
	}{
		{"log surfaces", func(d *Deps) { d.Log = nil }},
		{"state client", func(d *Deps) { d.DB = nil }},
		{"classifier", func(d *Deps) { d.Judge = nil }},
		{"feed resolver", func(d *Deps) { d.Feed = nil }},
		{"footer resolver", func(d *Deps) { d.Footer = nil }},
		{"holds resolver", func(d *Deps) { d.Holds = nil }},
		{"client resolver", func(d *Deps) { d.Client = nil }},
		{"watcher resolver", func(d *Deps) { d.Watcher = nil }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			deps := full()
			tc.blank(&deps)
			// Act
			_, err := New(deps)
			// Assert
			if err == nil {
				t.Fatalf("New with no %s must refuse", tc.name)
			}
		})
	}
}

func TestNewAcceptsACompleteWiring(t *testing.T) {
	// Arrange / Act
	h := newHarness(t)
	// Assert
	if h.q == nil {
		t.Fatal("a complete wiring must build a queue")
	}
}

func TestSaidTextJoinsEveryTextBlock(t *testing.T) {
	// Arrange
	said := &conversationv1.UserSaid{Content: &conversationv1.UserContent{
		Blocks: []*conversationv1.UserContentBlock{
			{Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: "first"}}},
			{Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: "second"}}},
		},
	}}
	// Act
	got := saidText(said)
	// Assert
	if got != "first\nsecond" {
		t.Fatalf("saidText = %q, want both blocks joined", got)
	}
}

func TestDispositionParkedReadsTheThreeWayAnswer(t *testing.T) {
	tests := []struct {
		name string
		d    Disposition
		want bool
	}{
		{"delivered", Disposition{Delivered: true}, false},
		{"refused", Disposition{RefusedArm: ArmMerging}, false},
		{"held", Disposition{}, true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			if got := tc.d.Parked(); got != tc.want {
				t.Fatalf("Parked() = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestStandingHoldReportsAnUnknownTurn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	_, err := h.q.standingHold(context.Background(), theWorkspace, "never-held")
	// Assert
	if err != ErrNoSuchHold {
		t.Fatalf("err = %v, want ErrNoSuchHold", err)
	}
}
