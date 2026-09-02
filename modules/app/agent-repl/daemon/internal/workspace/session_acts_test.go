package workspace

import (
	"context"
	"errors"
	"testing"

	"claude-repld/internal/wsm"
)

func TestSetModelGoesThroughTheQueue(t *testing.T) {
	// Arrange: a model change that jumped the queue would run the queued prompt
	// on a model the user did not choose for it.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.cards.models, f.cards.hasModels = []string{"opus", "sonnet"}, true

	// Act.
	if err := f.verbs.SetModel(context.Background(), "w1", "opus"); err != nil {
		t.Fatalf("SetModel: %v", err)
	}

	// Assert.
	acts := f.queue.acts["w1"]
	if len(acts) != 1 || acts[0].Kind != actSetModel || acts[0].Value != "opus" {
		t.Fatalf("session acts = %+v, want one set_model act naming opus", acts)
	}
}

// TestSetModelRefusesAModelOutsideTheServedCatalog covers the selector's
// contract: the daemon accepts only the tokens it offered, so a model nobody
// was shown never reaches the vendor.
func TestSetModelRefusesAModelOutsideTheServedCatalog(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.cards.models, f.cards.hasModels = []string{"opus", "sonnet"}, true

	// Act.
	err := f.verbs.SetModel(context.Background(), "w1", "not-a-real-model")

	// Assert.
	asRefusal(t, err, ArmNotInCatalog)
	if acts := f.queue.acts["w1"]; len(acts) != 0 {
		t.Fatalf("session acts = %+v, want none for a model outside the catalog", acts)
	}
}

// TestSetModelRefusesWhenNoCatalogWasServed covers the other half: with no
// catalog served there is nothing the daemon offered, so there is nothing it
// can accept.
func TestSetModelRefusesWhenNoCatalogWasServed(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	err := f.verbs.SetModel(context.Background(), "w1", "opus")

	// Assert.
	asRefusal(t, err, ArmNotInCatalog)
}

func TestSetModelRefusesAnEmptyModel(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	err := f.verbs.SetModel(context.Background(), "w1", "")

	// Assert.
	asRefusal(t, err, ArmUnservedAnswer)
}

func TestSetModelSurfacesAQueueFailure(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.queue.actErr = errors.New("the merge lease refuses new work")

	// Act.
	err := f.verbs.SetModel(context.Background(), "w1", "opus")

	// Assert.
	if err == nil {
		t.Fatal("SetModel() = nil error, want the queue refusal surfaced")
	}
}

func TestSetPermissionModeAcceptsAServedMode(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.cards.modes, f.cards.hasModes = []string{"default", "plan"}, true

	// Act.
	if err := f.verbs.SetPermissionMode(context.Background(), "w1", "plan"); err != nil {
		t.Fatalf("SetPermissionMode: %v", err)
	}

	// Assert.
	acts := f.queue.acts["w1"]
	if len(acts) != 1 || acts[0].Kind != actSetPermissionMode || acts[0].Value != "plan" {
		t.Fatalf("session acts = %+v, want one set_permission_mode act naming plan", acts)
	}
}

func TestSetPermissionModeRefusesAModeThePickerNeverServed(t *testing.T) {
	// Arrange: the daemon accepts only what it offered.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.cards.modes, f.cards.hasModes = []string{"default", "plan"}, true

	// Act.
	err := f.verbs.SetPermissionMode(context.Background(), "w1", "acceptEdits")

	// Assert.
	asRefusal(t, err, ArmModeNotServed)
}

func TestSetPermissionModeRefusesWhenNoPickerWasServed(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	err := f.verbs.SetPermissionMode(context.Background(), "w1", "plan")

	// Assert.
	asRefusal(t, err, ArmModeNotServed)
}

func TestSetPermissionModeRefusesAnUngatedModeWithoutRecordedConsent(t *testing.T) {
	// Arrange: consenting once, at creation, is what buys an ungated session.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.cards.modes, f.cards.hasModes = []string{"default", "bypassPermissions"}, true
	f.db.jobs["w1"] = wsm.CreationJob{Workspace: "w1"}

	// Act.
	err := f.verbs.SetPermissionMode(context.Background(), "w1", "bypassPermissions")

	// Assert.
	asRefusal(t, err, ArmUngatedWithoutConsent)
}

func TestSetPermissionModeAcceptsAnUngatedModeWithRecordedConsent(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.cards.modes, f.cards.hasModes = []string{"default", "bypassPermissions"}, true
	f.db.jobs["w1"] = wsm.CreationJob{Workspace: "w1", ConsentedUngatedMode: "bypassPermissions"}

	// Act.
	if err := f.verbs.SetPermissionMode(context.Background(), "w1", "bypassPermissions"); err != nil {
		t.Fatalf("SetPermissionMode: %v", err)
	}

	// Assert.
	if len(f.queue.acts["w1"]) != 1 {
		t.Fatalf("session acts = %+v, want exactly one", f.queue.acts["w1"])
	}
}

func TestSetPermissionModeRefusesAnEmptyMode(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	err := f.verbs.SetPermissionMode(context.Background(), "w1", "")

	// Assert.
	asRefusal(t, err, ArmModeNotServed)
}
