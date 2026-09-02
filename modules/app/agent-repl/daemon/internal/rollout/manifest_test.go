package rollout

import (
	"context"
	"sync"
	"testing"

	"claude-repld/internal/ids"
	"claude-repld/internal/sessionlock"
	"claude-repld/internal/wsm"
)

func TestTheReconciliationMatrixNamesEachDisposition(t *testing.T) {
	// Arrange
	cases := []struct {
		name   string
		intent Intent
		lock   sessionlock.State
		want   DispositionKind
	}{
		{"a session meant to survive whose lock is held", IntentPreserve, sessionlock.StateHeld, DispositionPreserved},
		{"a session meant to survive whose lock is free", IntentPreserve, sessionlock.StateFree, DispositionDied},
		{"a session meant to end whose lock is free", IntentStandDown, sessionlock.StateFree, DispositionRolled},
		{"a session meant to end whose lock is held", IntentStandDown, sessionlock.StateHeld, DispositionUnknown},
		{"a probe that could not tell about a preserved session", IntentPreserve, sessionlock.StateUnknown, DispositionUnknown},
		{"a probe that could not tell about a stood-down session", IntentStandDown, sessionlock.StateUnknown, DispositionUnknown},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := disposition(tc.intent, tc.lock)

			// Assert
			if got != tc.want {
				t.Fatalf("disposition(%s, %s) = %s, want %s", tc.intent, tc.lock, got, tc.want)
			}
		})
	}
}

// reconcileOne writes a one-session manifest with the given intent and lock
// state, then reconciles it.
func reconcileOne(t *testing.T, h *harness, intent Intent, lock sessionlock.State) (ids.WorkspaceID, []Disposition) {
	t.Helper()
	ws, dir := h.workspace(t)
	h.mu.Lock()
	h.lockStates[dir] = lock
	h.mu.Unlock()
	if err := h.c.writeManifest(context.Background(), Manifest{
		Daemon: ids.InstanceID("daemon-outgoing-previous"), WrittenAt: instant,
		Sessions: []ManifestSession{{
			Workspace: ws, Dir: dir, ShimPID: 4242, VendorSessionID: "vendor-1", Intent: intent,
		}},
	}); err != nil {
		t.Fatalf("writeManifest: %v", err)
	}
	got, err := h.c.Reconcile(context.Background())
	if err != nil {
		t.Fatalf("Reconcile: %v", err)
	}
	return ws, got
}

func TestReconcileAnswersOneDispositionPerSession(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	ws, got := reconcileOne(t, h, IntentPreserve, sessionlock.StateHeld)

	// Assert
	if len(got) != 1 || got[0].Workspace != ws || got[0].Kind != DispositionPreserved {
		t.Fatalf("dispositions = %+v, want one PRESERVED record for %s", got, ws)
	}
}

func TestAPreservedSessionsRecordIsAlreadyResolved(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	ws, _ := reconcileOne(t, h, IntentPreserve, sessionlock.StateHeld)
	open, err := h.db.OpenFaults(context.Background(), wsm.FaultScope{Workspace: &ws, Kind: FaultBounceDisposition})

	// Assert
	if err != nil {
		t.Fatalf("OpenFaults: %v", err)
	}
	if len(open) != 0 {
		t.Fatalf("open faults = %d, want none: a preserved session needs nothing doing", len(open))
	}
}

func TestARolledSessionsRecordIsAlreadyResolved(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	ws, _ := reconcileOne(t, h, IntentStandDown, sessionlock.StateFree)
	open, err := h.db.OpenFaults(context.Background(), wsm.FaultScope{Workspace: &ws, Kind: FaultBounceDisposition})

	// Assert
	if err != nil {
		t.Fatalf("OpenFaults: %v", err)
	}
	if len(open) != 0 {
		t.Fatalf("open faults = %d, want none: a rolled session ended as intended", len(open))
	}
}

func TestASessionThatSilentlyDiedLeavesAnOpenFault(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	ws, _ := reconcileOne(t, h, IntentPreserve, sessionlock.StateFree)
	open, err := h.db.OpenFaults(context.Background(), wsm.FaultScope{Workspace: &ws, Kind: FaultBounceDisposition})

	// Assert
	if err != nil {
		t.Fatalf("OpenFaults: %v", err)
	}
	if len(open) != 1 {
		t.Fatalf("open faults = %d, want one: which session died is the whole point of the record", len(open))
	}
	if open[0].Evidence["disposition"] != string(DispositionDied) {
		t.Fatalf("evidence disposition = %q, want DIED", open[0].Evidence["disposition"])
	}
}

func TestAnUndeterminableSessionLeavesAnOpenFault(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	ws, _ := reconcileOne(t, h, IntentStandDown, sessionlock.StateHeld)
	open, err := h.db.OpenFaults(context.Background(), wsm.FaultScope{Workspace: &ws, Kind: FaultBounceDisposition})

	// Assert
	if err != nil {
		t.Fatalf("OpenFaults: %v", err)
	}
	if len(open) != 1 || open[0].Evidence["disposition"] != string(DispositionUnknown) {
		t.Fatalf("open faults = %+v, want one UNKNOWN record", open)
	}
}

func TestTheDispositionRecordCarriesTheManifestsPidRatherThanACount(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	ws, _ := reconcileOne(t, h, IntentPreserve, sessionlock.StateFree)
	open, err := h.db.OpenFaults(context.Background(), wsm.FaultScope{Workspace: &ws, Kind: FaultBounceDisposition})

	// Assert
	if err != nil {
		t.Fatalf("OpenFaults: %v", err)
	}
	if open[0].Evidence["shim_pid"] != "4242" || open[0].Evidence["vendor_session_id"] != "vendor-1" {
		t.Fatalf("evidence = %v, want the manifest's own pid and vendor session", open[0].Evidence)
	}
}

func TestReconcileAnswersNothingWithNoManifest(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	got, err := h.c.Reconcile(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("Reconcile: %v", err)
	}
	if got != nil {
		t.Fatalf("dispositions = %+v, want none on an ordinary boot", got)
	}
}

func TestReadManifestRefusesAManifestThatWillNotDecode(t *testing.T) {
	// Arrange
	h := newHarness(t)
	path := h.c.deps.IntentManifest
	if err := h.c.writeManifest(context.Background(), Manifest{WrittenAt: instant}); err != nil {
		t.Fatalf("writeManifest: %v", err)
	}
	if err := writeFile(path, "not json"); err != nil {
		t.Fatalf("corrupt the manifest: %v", err)
	}

	// Act
	_, _, err := ReadManifest(path)

	// Assert
	if err == nil {
		t.Fatalf("ReadManifest accepted a manifest that will not decode")
	}
}

func TestWriteManifestRefusesWithNoConfiguredPath(t *testing.T) {
	// Arrange
	h := newHarness(t, func(d *Deps) { d.IntentManifest = "" })

	// Act
	err := h.c.writeManifest(context.Background(), Manifest{WrittenAt: instant})

	// Assert
	if err == nil {
		t.Fatalf("writeManifest accepted an empty path")
	}
}

// toggleReadOnlyDB is a state handle that answers read-only until it is
// promoted, which is exactly the joining successor's handle.
type toggleReadOnlyDB struct {
	wsm.DB
	mu       sync.Mutex
	readOnly bool
}

func (d *toggleReadOnlyDB) ReadOnly() bool {
	d.mu.Lock()
	defer d.mu.Unlock()
	return d.readOnly
}

func (d *toggleReadOnlyDB) Promote(ctx context.Context) error {
	d.mu.Lock()
	d.readOnly = false
	d.mu.Unlock()
	return d.DB.Promote(ctx)
}

func (d *toggleReadOnlyDB) OpenFault(ctx context.Context, f wsm.Fault) (ids.FaultID, error) {
	if d.ReadOnly() {
		return "", wsm.ErrReadOnly
	}
	return d.DB.OpenFault(ctx, f)
}

// TestABounceDispositionIsDeferredWhileTheHandleIsReadOnly covers the joining
// successor's reconciliation: it reads the outgoing daemon's manifest before it
// owns anything, while the incumbent is still the sole writer. The accounting
// is a write, so it is HELD until the promotion rather than failed loudly on
// every ordinary handover.
func TestABounceDispositionIsDeferredWhileTheHandleIsReadOnly(t *testing.T) {
	// Arrange
	h := newHarness(t)
	handle := &toggleReadOnlyDB{DB: h.c.deps.DB, readOnly: true}
	h.c.deps.DB = handle

	// Act
	ws, _ := reconcileOne(t, h, IntentPreserve, sessionlock.StateFree)

	// Assert: nothing was written, and the accounting is standing.
	open, err := h.db.OpenFaults(context.Background(), wsm.FaultScope{Workspace: &ws, Kind: FaultBounceDisposition})
	if err != nil {
		t.Fatalf("OpenFaults: %v", err)
	}
	if len(open) != 0 {
		t.Fatalf("open faults = %d, want none while the handle is read-only", len(open))
	}
	if got := len(h.c.pendingDispositions); got != 1 {
		t.Fatalf("pending dispositions = %d, want the one the read-only handle deferred", got)
	}
}

// TestTheDeferredBounceDispositionsAreWrittenAtThePromotion is the other half:
// deferred is not dropped.
func TestTheDeferredBounceDispositionsAreWrittenAtThePromotion(t *testing.T) {
	// Arrange
	h := newHarness(t)
	handle := &toggleReadOnlyDB{DB: h.c.deps.DB, readOnly: true}
	h.c.deps.DB = handle
	ws, _ := reconcileOne(t, h, IntentPreserve, sessionlock.StateFree)

	// Act
	if err := handle.Promote(context.Background()); err != nil {
		t.Fatalf("Promote: %v", err)
	}
	h.c.flushDispositions(context.Background())

	// Assert
	open, err := h.db.OpenFaults(context.Background(), wsm.FaultScope{Workspace: &ws, Kind: FaultBounceDisposition})
	if err != nil {
		t.Fatalf("OpenFaults: %v", err)
	}
	if len(open) != 1 {
		t.Fatalf("open faults = %d, want the deferred disposition written at the promotion", len(open))
	}
}
