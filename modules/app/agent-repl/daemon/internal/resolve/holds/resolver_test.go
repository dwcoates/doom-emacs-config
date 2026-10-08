package holds_test

import (
	"fmt"
	"sync"
	"testing"

	"google.golang.org/protobuf/proto"

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

// The "held (N)" heading is RETIRED from the proto (owner ruling 5,
// 2026-09-13). What the counter used to state — an offer is a held thing and
// is counted alongside the prompts — is now stated by the items list itself.
func TestTrayCountsAnOfferAmongItsItems(t *testing.T) {
	tests := []struct {
		name  string
		held  []wsm.HeldPrompt
		offer *frontendv1.HeldOffer
		want  int
	}{
		{name: "empty", want: 0},
		{name: "one prompt", held: []wsm.HeldPrompt{hold("t1", "one")}, want: 1},
		{
			name: "two prompts",
			held: []wsm.HeldPrompt{hold("t1", "one"), hold("t2", "two")},
			want: 2,
		},
		{
			name:  "an offer counts as a held thing",
			held:  []wsm.HeldPrompt{hold("t1", "one")},
			offer: testMergeDequeueOffer(),
			want:  2,
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
			if got := len(latest(t, r).GetItems()); got != tc.want {
				t.Fatalf("tray items = %d, want %d", got, tc.want)
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
	if got := len((<-ch).GetItems()); got != 0 {
		t.Fatalf("first push items = %d, want 0", got)
	}
	if got := len((<-ch).GetItems()); got != 1 {
		t.Fatalf("second push items = %d, want 1", got)
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
	if got := len((<-ch).GetItems()); got != 0 {
		t.Fatalf("next delivery held %d items, want the change (0) — a duplicate render reached the wire", got)
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

func TestTrayMarksThePromptBeingEdited(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	r.SetHeldPrompts(testWS, []wsm.HeldPrompt{hold("t1", "one"), hold("t2", "two")})

	// Act.
	r.SetEditing(testWS, "t2")

	// Assert.
	items := latest(t, r).GetItems()
	if items[0].GetPrompt().GetEditing() != nil {
		t.Fatal("a prompt nobody is editing carried the editing marker")
	}
	if items[1].GetPrompt().GetEditing() == nil {
		t.Fatal("the prompt being edited carried no editing marker")
	}
}

func TestTrayClearsTheEditingMarker(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	r.SetHeldPrompts(testWS, []wsm.HeldPrompt{hold("t1", "one")})
	r.SetEditing(testWS, "t1")

	// Act.
	r.SetEditing(testWS, "")

	// Assert.
	if latest(t, r).GetItems()[0].GetPrompt().GetEditing() != nil {
		t.Fatal("the editing marker survived the edit's end")
	}
}

func TestTrayKeepsTheEditingMarkerAcrossAHoldsPush(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	r.SetEditing(testWS, "t1")

	// Act.
	r.SetHeldPrompts(testWS, []wsm.HeldPrompt{hold("t1", "one")})

	// Assert.
	if latest(t, r).GetItems()[0].GetPrompt().GetEditing() == nil {
		t.Fatal("a holds push dropped the standing editing marker")
	}
}

func TestTrayBadgesThePromptBeingEdited(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	r.SetHeldPrompts(testWS, []wsm.HeldPrompt{hold("t1", "one")})

	// Act.
	r.SetEditing(testWS, "t1")

	// Assert.
	p := latest(t, r).GetItems()[0].GetPrompt()
	if p.GetBadge().GetLabel() != "editing" || len(p.GetNotes()) != 1 || p.GetNotes()[0].GetSentence() != "queued — classifying" {
		t.Fatalf("badge = %v, notes = %v, want the editing badge over the classifying note", p.GetBadge(), p.GetNotes())
	}
}

// TestAHoldForAnUnboundWorkspaceStatesTheViolationAtError pins that the
// violation is an ERROR, stated once, rather than context on quieter records.
func TestAHoldForAnUnboundWorkspaceStatesTheViolationAtError(t *testing.T) {
	// Arrange
	surfaces := dlog.NewTestSurfaces()
	r, err := holds.New(surfaces)
	if err != nil {
		t.Fatalf("holds.New: %v", err)
	}

	// Act
	r.SetHeldPrompts(testWS, []wsm.HeldPrompt{hold("t1", "one")})
	r.SetHeldPrompts(testWS, nil)

	// Assert
	n := 0
	for _, rec := range surfaces.Records() {
		if rec.Level == "error" && rec.Operation == "daemon.holds.unbound_workspace" {
			n++
		}
	}
	if n != 1 {
		t.Fatalf("unbound-workspace ERROR records = %d, want 1", n)
	}
}

// TestConcurrentChangesLeaveTheNewestTrayPublished pins the tray's
// publication order: each change's tray is published under the lock that
// rendered it, so once concurrent changes are all in, the topic holds the
// render of the state they left, never a stale tray published late.
func TestConcurrentChangesLeaveTheNewestTrayPublished(t *testing.T) {
	// Arrange
	r, _ := newResolver(t)
	const writers = 64
	start := make(chan struct{})
	var wg sync.WaitGroup
	for i := 0; i < writers; i++ {
		wg.Add(1)
		go func(i int) {
			defer wg.Done()
			<-start
			r.SetHeldPrompts(testWS, []wsm.HeldPrompt{hold(fmt.Sprintf("t%d", i), "held")})
		}(i)
	}

	// Act
	close(start)
	wg.Wait()

	// Assert
	if got, want := latest(t, r), holds.RenderNow(r, testWS); !proto.Equal(got, want) {
		t.Fatalf("published tray = %v, want the render of the final state %v", got, want)
	}
}
