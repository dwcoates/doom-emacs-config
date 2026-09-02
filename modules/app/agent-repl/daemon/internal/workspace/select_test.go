package workspace

import (
	"context"
	"testing"
	"time"

	"claude-repld/internal/wsm"
)

func TestSelectRecordsTheSelectionInstant(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Select(context.Background(), "w1"); err != nil {
		t.Fatalf("Select: %v", err)
	}

	// Assert.
	if f.db.current == nil || *f.db.current != "w1" || !f.db.currentAt.Equal(fixedNow) {
		t.Fatalf("current = (%v, %v), want w1 at %v", f.db.current, f.db.currentAt, fixedNow)
	}
}

func TestSelectClearsTheAttentionMarker(t *testing.T) {
	// Arrange: the user has now looked at whatever raised it.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.attention["w1"] = true

	// Act.
	if err := f.verbs.Select(context.Background(), "w1"); err != nil {
		t.Fatalf("Select: %v", err)
	}

	// Assert.
	if f.db.attention["w1"] {
		t.Fatal("Select() left the attention marker set")
	}
}

func TestSelectTellsTheRoster(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Select(context.Background(), "w1"); err != nil {
		t.Fatalf("Select: %v", err)
	}

	// Assert.
	if len(f.sidebar.selected) != 1 || f.sidebar.selected[0] != "w1" {
		t.Fatalf("roster selections = %v, want w1 once", f.sidebar.selected)
	}
}

func TestSelectIsIdempotent(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Select(context.Background(), "w1"); err != nil {
		t.Fatalf("first Select: %v", err)
	}
	if err := f.verbs.Select(context.Background(), "w1"); err != nil {
		t.Fatalf("second Select: %v", err)
	}

	// Assert.
	if *f.db.current != "w1" {
		t.Fatalf("current = %v, want w1", *f.db.current)
	}
}

func TestSelectRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange.
	f := newFixture(t)

	// Act.
	err := f.verbs.Select(context.Background(), "nope")

	// Assert.
	asRefusal(t, err, ArmUnknownWorkspace)
}

func TestSetPriorityRecordsThePriority(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	priority := wsm.PriorityP05

	// Act.
	if err := f.verbs.SetPriority(context.Background(), "w1", &priority); err != nil {
		t.Fatalf("SetPriority: %v", err)
	}

	// Assert.
	got := f.db.priorities["w1"]
	if got == nil || *got != wsm.PriorityP05 {
		t.Fatalf("recorded priority = %v, want P05", got)
	}
}

func TestSetPriorityClearsThePriority(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.SetPriority(context.Background(), "w1", nil); err != nil {
		t.Fatalf("SetPriority: %v", err)
	}

	// Assert.
	got, recorded := f.db.priorities["w1"]
	if !recorded || got != nil {
		t.Fatalf("recorded priority = (%v, %v), want a recorded clear", got, recorded)
	}
}

func TestSetPriorityRepublishesTheRoster(t *testing.T) {
	// Arrange: clients follow roster order strictly, so the order changing is
	// a roster push.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	priority := wsm.PriorityP2

	// Act.
	if err := f.verbs.SetPriority(context.Background(), "w1", &priority); err != nil {
		t.Fatalf("SetPriority: %v", err)
	}

	// Assert.
	if len(f.sidebar.registries) != 1 {
		t.Fatalf("roster republications = %d, want exactly one", len(f.sidebar.registries))
	}
}

// TestSelectRefreshesTheRegistryBeforeStampingTheSelection pins the ORDER. The
// current id and the selection instant are both WSM's, so a roster rendered
// from the pre-select registry would carry the selection with an unstamped
// when-column and the switch would appear to land twice.
func TestSelectRefreshesTheRegistryBeforeStampingTheSelection(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Select(context.Background(), "w1"); err != nil {
		t.Fatalf("Select: %v", err)
	}

	// Assert.
	want := []string{"registry", "selected"}
	if len(f.sidebar.calls) != len(want) || f.sidebar.calls[0] != want[0] || f.sidebar.calls[1] != want[1] {
		t.Fatalf("roster calls = %v, want %v", f.sidebar.calls, want)
	}
}

// TestSelectPublishesARegistryCarryingTheSelection pins that the refreshed
// registry is the one WSM holds AFTER the write, not before it.
func TestSelectPublishesARegistryCarryingTheSelection(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Select(context.Background(), "w1"); err != nil {
		t.Fatalf("Select: %v", err)
	}

	// Assert.
	if len(f.sidebar.registries) != 1 {
		t.Fatalf("roster republications = %d, want exactly one", len(f.sidebar.registries))
	}
	got := f.sidebar.registries[0].Current
	if got == nil || *got != "w1" {
		t.Fatalf("the published registry's current = %v, want w1", got)
	}
}

// TestReselectingDoesNotRestampTheSelectionInstant pins the idempotence the
// contract states: re-selecting the workspace already being looked at is a
// success that CHANGES NO VIEW. The instant answers "when did the user last
// switch to this", and the user did not switch.
func TestReselectingDoesNotRestampTheSelectionInstant(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	if err := f.verbs.Select(context.Background(), "w1"); err != nil {
		t.Fatalf("first Select: %v", err)
	}
	f.db.currentAt = fixedNow.Add(-time.Hour)

	// Act.
	if err := f.verbs.Select(context.Background(), "w1"); err != nil {
		t.Fatalf("second Select: %v", err)
	}

	// Assert.
	if !f.db.currentAt.Equal(fixedNow.Add(-time.Hour)) {
		t.Fatalf("the selection instant = %v, want the re-selection to leave it alone", f.db.currentAt)
	}
}

// TestReselectingStillClearsTheAttentionMarker pins that idempotence does not
// swallow the clear: a notification can raise the marker while the workspace
// IS current, and looking at it again is what clears it.
func TestReselectingStillClearsTheAttentionMarker(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	if err := f.verbs.Select(context.Background(), "w1"); err != nil {
		t.Fatalf("first Select: %v", err)
	}
	f.db.attention["w1"] = true

	// Act.
	if err := f.verbs.Select(context.Background(), "w1"); err != nil {
		t.Fatalf("second Select: %v", err)
	}

	// Assert.
	if f.db.attention["w1"] {
		t.Fatal("re-selecting left the attention marker set")
	}
}
