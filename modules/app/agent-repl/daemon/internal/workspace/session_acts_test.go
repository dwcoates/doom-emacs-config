package workspace

import (
	"context"
	"errors"
	"testing"

	"agent-repld/internal/wsm"
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

func TestRequireServedModel(t *testing.T) {
	cases := []struct {
		name      string
		catalog   []string
		hasModels bool
		model     string
		wantArm   string
	}{
		{name: "a served model passes", catalog: []string{"opus", "sonnet"}, hasModels: true, model: "sonnet"},
		{name: "an unserved model is not in the catalog", catalog: []string{"opus"}, hasModels: true, model: "gpt", wantArm: ArmNotInCatalog},
		{name: "no catalog is the caller's own arm", model: "opus", wantArm: ArmNoSession},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			f.cards.models, f.cards.hasModels = tc.catalog, tc.hasModels

			// Act
			err := f.verbs.(*verbs).requireServedModel(f.log.Global(), "Verb", "w1", tc.model, ArmNoSession)

			// Assert
			if tc.wantArm == "" {
				if err != nil {
					t.Fatalf("requireServedModel = %v, want nil", err)
				}
				return
			}
			asRefusal(t, err, tc.wantArm)
		})
	}
}

// TestEveryModelVerbChecksTheCatalogThroughRequireServedModel pins that
// SetModel and SetDefaultModel refuse an unserved model with the shared
// helper's own reason, so a hand-rolled check that drifted fails here.
func TestEveryModelVerbChecksTheCatalogThroughRequireServedModel(t *testing.T) {
	calls := map[string]func(*fixture) error{
		"SetModel":        func(f *fixture) error { return f.verbs.SetModel(context.Background(), "w1", "gpt") },
		"SetDefaultModel": func(f *fixture) error { return f.verbs.SetDefaultModel(context.Background(), "w1", "gpt") },
	}
	for name, call := range calls {
		t.Run(name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			f.cards.models, f.cards.hasModels = []string{"opus"}, true
			want := asRefusal(t, f.verbs.(*verbs).requireServedModel(f.log.Global(), name, "w1", "gpt", ArmNoSession), ArmNotInCatalog)

			// Act
			got := asRefusal(t, call(f), ArmNotInCatalog)

			// Assert
			if got.Reason != want.Reason {
				t.Fatalf("%s reason = %q, want the shared helper's %q", name, got.Reason, want.Reason)
			}
		})
	}
}
