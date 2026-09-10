package holds_test

import (
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/holds"
	"claude-repld/internal/wsm"
)

// testMergeDequeueOffer builds a merge-dequeue offer shaped the way the merge
// orchestrator's own dequeueOffer composes one, for tests whose subject is the
// tray rather than the offer's prose.
func testMergeDequeueOffer() *frontendv1.HeldOffer {
	return &frontendv1.HeldOffer{
		Offer: &frontendv1.HeldOffer_MergeDequeue{
			MergeDequeue: &frontendv1.HeldOfferMergeDequeue{
				Headline: &frontendv1.HeldOfferHeadline{Text: "interrupting — keep your merge's queue slot, or release it?"},
			},
		},
	}
}

func TestNewRefusesWithoutLogSurfaces(t *testing.T) {
	// Arrange, Act.
	_, err := holds.New(nil)

	// Assert.
	if err == nil {
		t.Fatal("holds.New accepted nil log surfaces")
	}
}

func TestTrayComposesItsHeading(t *testing.T) {
	tests := []struct {
		name  string
		held  []wsm.HeldPrompt
		offer *frontendv1.HeldOffer
		want  string
	}{
		{name: "empty", want: "held (0)"},
		{name: "one prompt", held: []wsm.HeldPrompt{hold("t1", "one")}, want: "held (1)"},
		{
			name: "two prompts",
			held: []wsm.HeldPrompt{hold("t1", "one"), hold("t2", "two")},
			want: "held (2)",
		},
		{
			name:  "an offer counts as a held thing",
			held:  []wsm.HeldPrompt{hold("t1", "one")},
			offer: testMergeDequeueOffer(),
			want:  "held (2)",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			r, _ := newResolver(t)

			// Act.
			r.SetHeldPrompts(testWS, tc.held)
			r.SetOffer(testWS, tc.offer)

			// Assert.
			if got := latest(t, r).GetHeading().GetText(); got != tc.want {
				t.Fatalf("heading = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestTrayPublishesTheEmptyTray(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)

	// Act: an empty list is a meaningful value, not an absent one.
	r.SetHeldPrompts(testWS, nil)

	// Assert.
	if got := latest(t, r).GetItems(); len(got) != 0 {
		t.Fatalf("the empty tray carried %d items", len(got))
	}
}

func TestTrayDrawsTheOfferAfterEveryPrompt(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)

	// Act.
	r.SetHeldPrompts(testWS, []wsm.HeldPrompt{hold("t1", "one")})
	r.SetOffer(testWS, testMergeDequeueOffer())

	// Assert.
	items := latest(t, r).GetItems()
	if len(items) != 2 {
		t.Fatalf("tray carried %d items, want the prompt and the offer", len(items))
	}
	if _, ok := items[1].GetItem().(*frontendv1.DaemonHoldItem_Offer); !ok {
		t.Fatal("the offer was not drawn last")
	}
}

func TestTrayClearsTheOffer(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	r.SetOffer(testWS, testMergeDequeueOffer())

	// Act.
	r.SetOffer(testWS, nil)

	// Assert.
	if got := latest(t, r).GetItems(); len(got) != 0 {
		t.Fatalf("clearing the offer left %d items", len(got))
	}
}

func TestTrayRecordsAnOfferWithNoArm(t *testing.T) {
	// Arrange.
	r, surfaces := newResolver(t)

	// Act.
	r.SetOffer(testWS, &frontendv1.HeldOffer{})

	// Assert.
	if !hasError(surfaces.Records(), "daemon.holds.set_offer") {
		t.Fatal("an armless offer was not recorded as an invariant violation")
	}
}

func TestTrayKeepsWorkspacesApart(t *testing.T) {
	// Arrange.
	const other = ids.WorkspaceID("ws-other")
	r, _ := newResolver(t)
	if err := r.SetWorkspaceDir(other, t.TempDir()); err != nil {
		t.Fatalf("SetWorkspaceDir: %v", err)
	}

	// Act.
	r.SetHeldPrompts(testWS, []wsm.HeldPrompt{hold("t1", "mine")})
	r.SetHeldPrompts(other, nil)

	// Assert.
	tray, ok := r.Topic(other).Latest()
	if !ok || len(tray.GetItems()) != 0 {
		t.Fatal("one workspace's holds reached another workspace's tray")
	}
}

func TestTrayRepublishesWhenTheHoldsChange(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	ch := subscribe(t, r)

	// Act.
	r.SetHeldPrompts(testWS, nil)
	r.SetHeldPrompts(testWS, []wsm.HeldPrompt{hold("t1", "one")})

	// Assert.
	if got := (<-ch).GetHeading().GetText(); got != "held (0)" {
		t.Fatalf("first push heading = %q", got)
	}
	if got := (<-ch).GetHeading().GetText(); got != "held (1)" {
		t.Fatalf("second push heading = %q", got)
	}
}

func TestTrayDoesNotRepublishAnIdenticalRender(t *testing.T) {
	// Arrange. Binding publishes the EMPTY tray, which a subscriber is
	// replayed on open; the assertions here are about what follows it.
	r, _ := newResolver(t)
	ch := subscribe(t, r)
	<-ch
	r.SetHeldPrompts(testWS, []wsm.HeldPrompt{hold("t1", "one")})
	<-ch

	// Act: the same holds again, then a change that must still arrive.
	r.SetHeldPrompts(testWS, []wsm.HeldPrompt{hold("t1", "one")})
	r.SetHeldPrompts(testWS, nil)

	// Assert: the duplicate never reached the wire, so the next value is the change.
	if got := (<-ch).GetHeading().GetText(); got != "held (0)" {
		t.Fatalf("next delivery = %q, want the change — a duplicate render reached the wire", got)
	}
}

func TestTrayRecordsAHoldForAnUnboundWorkspace(t *testing.T) {
	// Arrange.
	surfaces := dlog.NewTestSurfaces()
	r, err := holds.New(surfaces)
	if err != nil {
		t.Fatalf("holds.New: %v", err)
	}

	// Act.
	r.SetHeldPrompts(testWS, []wsm.HeldPrompt{hold("t1", "one")})

	// Assert.
	var found bool
	for _, rec := range surfaces.Records() {
		if rec.Context["invariant_violation"] == "hold-tray fact for a workspace with no bound directory" {
			found = true
		}
	}
	if !found {
		t.Fatal("a hold for an unbound workspace was not recorded as an invariant violation")
	}
}

func TestSetWorkspaceDirRefusesADirectoryItCannotResolve(t *testing.T) {
	// Arrange.
	surfaces := &failingSurfaces{TestSurfaces: dlog.NewTestSurfaces()}
	r, err := holds.New(surfaces)
	if err != nil {
		t.Fatalf("holds.New: %v", err)
	}

	// Act.
	err = r.SetWorkspaceDir(testWS, "/nowhere")

	// Assert: a sink that cannot be resolved fails rather than falling back.
	if err == nil {
		t.Fatal("SetWorkspaceDir accepted a directory whose sink could not be opened")
	}
}

// failingSurfaces is TestSurfaces whose workspace sink never resolves.
type failingSurfaces struct {
	*dlog.TestSurfaces
}

// Workspace implements dlog.Surfaces by refusing.
func (f *failingSurfaces) Workspace(string) (dlog.Logger, error) {
	return nil, errNoSink
}

// errNoSink is the refusal failingSurfaces answers with.
var errNoSink = errSink("no sink")

// errSink is a string error, so the double needs no extra dependency.
type errSink string

func (e errSink) Error() string { return string(e) }
