package topbar

import (
	"context"
	"errors"
	"sync"
	"testing"

	"google.golang.org/protobuf/proto"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// fakeSelections is a SelectionStore in memory.
type fakeSelections struct {
	mu         sync.Mutex
	selections map[ids.WorkspaceID]wsm.Selections
	catalogs   map[string]*conversationv1.ModelCatalog
	readErr    error
	writeErr   error
}

func newFakeSelections() *fakeSelections {
	return &fakeSelections{selections: map[ids.WorkspaceID]wsm.Selections{}, catalogs: map[string]*conversationv1.ModelCatalog{}}
}

func (f *fakeSelections) WorkspaceSelections(_ context.Context, id ids.WorkspaceID) (wsm.Selections, bool, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	sel, ok := f.selections[id]
	return sel, ok, f.readErr
}

func (f *fakeSelections) put(id ids.WorkspaceID, edit func(*wsm.Selections)) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.writeErr != nil {
		return f.writeErr
	}
	sel := f.selections[id]
	edit(&sel)
	f.selections[id] = sel
	return nil
}

func (f *fakeSelections) PutWorkspaceModel(_ context.Context, id ids.WorkspaceID, model string) error {
	return f.put(id, func(s *wsm.Selections) { s.Model = model })
}

func (f *fakeSelections) PutWorkspacePermissionMode(_ context.Context, id ids.WorkspaceID, mode string) error {
	return f.put(id, func(s *wsm.Selections) { s.PermissionMode = mode })
}

func (f *fakeSelections) PutWorkspaceEffort(_ context.Context, id ids.WorkspaceID, level conversationv1.AgentEffortLevel) error {
	return f.put(id, func(s *wsm.Selections) { s.Effort = level })
}

func (f *fakeSelections) AccountModelCatalog(_ context.Context, configDir string) (*conversationv1.ModelCatalog, bool, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	c, ok := f.catalogs[configDir]
	return c, ok, f.readErr
}

func (f *fakeSelections) PutAccountModelCatalog(_ context.Context, configDir string, catalog *conversationv1.ModelCatalog) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.writeErr != nil {
		return f.writeErr
	}
	f.catalogs[configDir] = catalog
	return nil
}

func keptOption(name string) *conversationv1.ModelOption {
	return &conversationv1.ModelOption{Model: &conversationv1.AgentModel{Name: name}, DisplayName: name}
}

// sessionless brings the workspace to everything readiness needs, with no
// session started: naming, then the account (which seeds).
func sessionless(t *testing.T, h *harness) {
	t.Helper()
	h.r.SetNaming(testWS, Naming{Name: "fix-flaky-reconnect", ConfigDir: "/Users/dev/.claude"})
	h.r.SetAccount(testWS, testAccount("dev@example.com"))
}

func errorRecorded(h *harness, op string) bool {
	for _, record := range h.log.Records() {
		if record.Level == dlog.LevelError && record.Operation == op {
			return true
		}
	}
	return false
}

func TestTheRememberedControlsAreDrawnWithNoSession(t *testing.T) {
	// Arrange
	store := newFakeSelections()
	store.selections[testWS] = wsm.Selections{Model: "opus", PermissionMode: "plan"}
	store.catalogs["/Users/dev/.claude"] = &conversationv1.ModelCatalog{Options: []*conversationv1.ModelOption{keptOption("opus"), keptOption("sonnet")}}
	h := newHarness(t, WithSelectionStore(store))

	// Act
	sessionless(t, h)

	// Assert
	view := h.view(t)
	if view.GetModelSelector().GetSelected().GetModel().GetName() != "opus" || len(view.GetModelSelector().GetOptions()) != 2 ||
		view.GetPermissionModePicker().GetCurrent().GetMode() != "plan" {
		t.Fatalf("selector %v, picker %v: want the remembered opus of two options, and plan", view.GetModelSelector(), view.GetPermissionModePicker())
	}
}

func TestTheRememberedPickIsTheEffortInForce(t *testing.T) {
	// Arrange
	store := newFakeSelections()
	store.selections[testWS] = wsm.Selections{Model: "opus", Effort: effortLow}
	h := newHarness(t, WithSelectionStore(store))

	// Act
	sessionless(t, h)

	// Assert
	if level, picked := h.r.PickedEffort(testWS); !picked || level != effortLow {
		t.Fatalf("PickedEffort = %v, %v; want the remembered pick", level, picked)
	}
}

func TestNothingRememberedDrawsNoControls(t *testing.T) {
	// Arrange
	h := newHarness(t, WithSelectionStore(newFakeSelections()))

	// Act
	sessionless(t, h)

	// Assert
	if view := h.view(t); view.GetModelSelector() != nil || view.GetPermissionModePicker() != nil {
		t.Fatalf("selector %v, picker %v: want none with nothing remembered", view.GetModelSelector(), view.GetPermissionModePicker())
	}
}

func TestALiveSessionsFactsBeatTheRememberedOnes(t *testing.T) {
	// Arrange
	store := newFakeSelections()
	store.selections[testWS] = wsm.Selections{Model: "sonnet"}
	h := newHarness(t, WithSelectionStore(store))
	h.r.SetNaming(testWS, Naming{Name: "w", ConfigDir: "/Users/dev/.claude"})
	h.r.OnSessionStarted(testWS, sessionStarted("vend-1", "claude-opus-5"))

	// Act
	h.r.SetAccount(testWS, testAccount("dev@example.com"))

	// Assert
	if got := h.view(t).GetModelSelector().GetSelected().GetModel().GetName(); got != "claude-opus-5" {
		t.Fatalf("selected = %q, want the live session's model", got)
	}
}

func TestASessionStartKeepsItsModelAndMode(t *testing.T) {
	// Arrange
	store := newFakeSelections()
	h := newHarness(t, WithSelectionStore(store))

	// Act
	h.ready(t)

	// Assert
	if got := store.selections[testWS]; got.Model != "claude-opus-5" || got.PermissionMode == "" {
		t.Fatalf("kept = %+v, want the started model and its mode", got)
	}
}

func TestASessionsCatalogIsKeptUnderItsLogin(t *testing.T) {
	// Arrange
	store := newFakeSelections()
	h := newHarness(t, WithSelectionStore(store))
	started := sessionStarted("vend-1", "opus")
	started.ModelCatalog = []*conversationv1.ModelOption{keptOption("opus")}
	h.r.SetNaming(testWS, Naming{Name: "w", ConfigDir: "/Users/dev/.claude"})
	h.r.OnSessionStarted(testWS, started)

	// Act
	h.r.SetAccount(testWS, testAccount("dev@example.com"))

	// Assert
	if got := store.catalogs["/Users/dev/.claude"]; !proto.Equal(got, &conversationv1.ModelCatalog{Options: started.ModelCatalog}) {
		t.Fatalf("kept catalog = %v, want the session's", got)
	}
}

func TestAModelChangeIsKept(t *testing.T) {
	// Arrange
	store := newFakeSelections()
	h := newHarness(t, WithSelectionStore(store))
	h.ready(t)

	// Act
	h.r.OnSessionUpdate(testWS, &conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_ModelChanged{
		ModelChanged: &conversationv1.SessionModelChanged{EffectiveModel: &conversationv1.AgentModel{Name: "sonnet"}},
	}})

	// Assert
	if got := store.selections[testWS].Model; got != "sonnet" {
		t.Fatalf("kept model = %q, want sonnet", got)
	}
}

func TestAPickedEffortIsKept(t *testing.T) {
	// Arrange
	store := newFakeSelections()
	h := newHarness(t, WithSelectionStore(store))
	h.ready(t)

	// Act
	h.r.SetPickedEffort(testWS, effortLow)

	// Assert
	if got := store.selections[testWS].Effort; got != effortLow {
		t.Fatalf("kept effort = %v, want the pick", got)
	}
}

func TestAFailedKeepIsAnError(t *testing.T) {
	// Arrange
	store := newFakeSelections()
	store.writeErr = errors.New("disk full")
	h := newHarness(t, WithSelectionStore(store))

	// Act
	h.ready(t)

	// Assert
	if !errorRecorded(h, opSelections) {
		t.Fatalf("no ERROR %s record for a failed keep", opSelections)
	}
}

func TestAFailedReadIsAnErrorAndSeedsNothing(t *testing.T) {
	// Arrange
	store := newFakeSelections()
	store.selections[testWS] = wsm.Selections{Model: "opus"}
	store.readErr = errors.New("locked")
	h := newHarness(t, WithSelectionStore(store))

	// Act
	sessionless(t, h)

	// Assert
	if !errorRecorded(h, opSelections) || h.view(t).GetModelSelector() != nil {
		t.Fatalf("want an ERROR %s record and no seeded selector", opSelections)
	}
}
