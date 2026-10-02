package rollout

import (
	"context"
	"slices"
	"testing"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// sessionlessRecord reads a registered workspace's record for a direct call.
func sessionlessRecord(t *testing.T, h *harness, ws ids.WorkspaceID) wsm.Workspace {
	t.Helper()
	record, err := h.db.Workspace(context.Background(), ws)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	return record
}

func TestBringUpSessionlessStartsEachPendingWorkspace(t *testing.T) {
	// Arrange
	h := newHarness(t)
	first, _ := h.workspace(t)
	second, _ := h.workspace(t)
	pending := []wsm.Workspace{sessionlessRecord(t, h, first), sessionlessRecord(t, h, second)}

	// Act
	h.c.bringUpSessionless(pending, nil)

	// Assert
	got := h.Started()
	slices.Sort(got)
	want := []ids.WorkspaceID{first, second}
	slices.Sort(want)
	if !slices.Equal(got, want) {
		t.Fatalf("started = %v, want both pending workspaces", got)
	}
}

func TestBringUpSessionlessRaisesTheMarkerBeforeItReturns(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.mu.Lock()
	h.startErr[ws] = errFake
	h.mu.Unlock()

	// Act
	h.c.bringUpSessionless([]wsm.Workspace{sessionlessRecord(t, h, ws)}, nil)
	h.mu.Lock()
	first := append([]markerEdge(nil), h.marker...)
	h.mu.Unlock()

	// Assert
	if len(first) == 0 || first[0] != (markerEdge{ws: ws, up: true}) {
		t.Fatalf("marker edges on return = %v, want the raise made before the start ran", first)
	}
}

func TestBringUpSessionlessLowersTheMarkerOnceTheStartEnds(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	h.c.bringUpSessionless([]wsm.Workspace{sessionlessRecord(t, h, ws)}, nil)

	// Assert
	want := []markerEdge{{ws: ws, up: true}, {ws: ws, up: false}}
	if got := h.Marker(); !slices.Equal(got, want) {
		t.Fatalf("marker edges = %v, want %v", got, want)
	}
}

func TestBringUpSessionlessDoesNothingForNoWorkspace(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.c.bringUpSessionless(nil, nil)

	// Assert
	if got := h.Started(); len(got) != 0 {
		t.Fatalf("started = %v, want nothing", got)
	}
	if got := records(h.log, opBringUp); len(got) != 0 {
		t.Fatalf("bring-up records = %v, want none for an empty bring-up", got)
	}
}

func TestBringUpSessionlessRecordsAFailedStartAtError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.mu.Lock()
	h.startErr[ws] = errFake
	h.mu.Unlock()

	// Act
	h.c.bringUpSessionless([]wsm.Workspace{sessionlessRecord(t, h, ws)}, nil)
	h.Started()

	// Assert
	errs := levelRecords(records(h.log, opBringUp), dlog.LevelError)
	if len(errs) != 1 || errs[0].Context[dlog.KeyWorkspaceID] != string(ws) || errs[0].Context["error"] != errFake.Error() {
		t.Fatalf("bring-up error records = %+v, want one naming the workspace and the start's cause", errs)
	}
}

// TestBringUpSessionlessGoesThroughTheSharedBringUp pins that the takeover's
// starts are the boot's: the per-workspace record is bringup.Run's own, under
// this package's operation, so a hand-rolled start loop here fails.
func TestBringUpSessionlessGoesThroughTheSharedBringUp(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	h.c.bringUpSessionless([]wsm.Workspace{sessionlessRecord(t, h, ws)}, nil)
	h.Started()

	// Assert
	var shared bool
	for _, rec := range records(h.log, opBringUp) {
		if rec.Message == "an open workspace's session was started" {
			shared = true
		}
	}
	if !shared {
		t.Fatalf("bring-up records = %v, want bringup.Run's per-workspace record", records(h.log, opBringUp))
	}
}
