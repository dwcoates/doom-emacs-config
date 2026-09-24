package rollout

import (
	"context"
	"errors"
	"os"
	"sync"
	"testing"

	"claude-repld/internal/health"
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
		{"a workspace with no shim whose lock is free", IntentNoSession, sessionlock.StateFree, DispositionPreserved},
		{"a workspace with no shim whose probe could not tell", IntentNoSession, sessionlock.StateUnknown, DispositionPreserved},
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
	return reconcileOnePID(t, h, intent, lock, 4242)
}

// reconcileOnePID is reconcileOne with the manifest entry's shim pid chosen,
// because a pid of zero is what "no process" means on the wire.
func reconcileOnePID(t *testing.T, h *harness, intent Intent, lock sessionlock.State, pid int) (ids.WorkspaceID, []Disposition) {
	t.Helper()
	ws, dir := h.workspace(t)
	h.mu.Lock()
	h.lockStates[dir] = lock
	h.mu.Unlock()
	if err := h.c.writeManifest(context.Background(), Manifest{
		Daemon: ids.InstanceID("daemon-outgoing-previous"), WrittenAt: instant,
		Sessions: []ManifestSession{{
			Workspace: ws, Dir: dir, ShimPID: pid, VendorSessionID: "vendor-1", Intent: intent,
		}},
	}); err != nil {
		t.Fatalf("writeManifest: %v", err)
	}
	got, err := h.c.Reconcile(context.Background(), nil)
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

// recordingDB remembers every fault id the reconciliation opened, so a test
// can read a record back after it was resolved.
type recordingDB struct {
	wsm.DB
	mu     sync.Mutex
	opened []ids.FaultID
}

func (d *recordingDB) OpenFault(ctx context.Context, f wsm.Fault) (ids.FaultID, error) {
	id, err := d.DB.OpenFault(ctx, f)
	if err == nil {
		d.mu.Lock()
		d.opened = append(d.opened, id)
		d.mu.Unlock()
	}
	return id, err
}

// diedRecord reconciles one preserve-intent session whose lock reads free and
// answers the one record it left.
func diedRecord(t *testing.T) wsm.Fault {
	t.Helper()
	h := newHarness(t)
	recorder := &recordingDB{DB: h.c.deps.DB}
	h.c.deps.DB = recorder
	reconcileOne(t, h, IntentPreserve, sessionlock.StateFree)
	if len(recorder.opened) != 1 {
		t.Fatalf("records opened = %d, want exactly one for the dead session", len(recorder.opened))
	}
	f, err := h.db.Fault(context.Background(), recorder.opened[0])
	if err != nil {
		t.Fatalf("Fault: %v", err)
	}
	return f
}

// TestASessionThatSilentlyDiedLeavesNoOpenFault pins the ordinary dead-shim
// path: a free lock is the kernel's proof the shim is gone, nothing could
// close an open fault here, and one stood on the strip for good.
func TestASessionThatSilentlyDiedLeavesNoOpenFault(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	ws, _ := reconcileOne(t, h, IntentPreserve, sessionlock.StateFree)
	open, err := h.db.OpenFaults(context.Background(), wsm.FaultScope{Workspace: &ws, Kind: health.KindBounceDied})

	// Assert
	if err != nil {
		t.Fatalf("OpenFaults: %v", err)
	}
	if len(open) != 0 {
		t.Fatalf("open faults = %d, want none: a dead shim with a free lock takes the ordinary dead-shim path", len(open))
	}
}

func TestASessionThatSilentlyDiedIsStillRecorded(t *testing.T) {
	// Arrange, Act
	f := diedRecord(t)

	// Assert: which session died is still the record's whole point.
	if f.Evidence["disposition"] != string(DispositionDied) || f.ResolvedAt == nil {
		t.Fatalf("record = %+v, want a resolved DIED record", f)
	}
}

func TestAnUndeterminableSessionLeavesAnOpenFault(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	ws, _ := reconcileOne(t, h, IntentStandDown, sessionlock.StateHeld)
	open, err := h.db.OpenFaults(context.Background(), wsm.FaultScope{Workspace: &ws, Kind: health.KindBounceUnknown})

	// Assert
	if err != nil {
		t.Fatalf("OpenFaults: %v", err)
	}
	if len(open) != 1 || open[0].Evidence["disposition"] != string(DispositionUnknown) {
		t.Fatalf("open faults = %+v, want one UNKNOWN record", open)
	}
}

func TestTheDispositionRecordCarriesTheManifestsPidRatherThanACount(t *testing.T) {
	// Arrange, Act
	f := diedRecord(t)

	// Assert
	if f.Evidence["shim_pid"] != "4242" || f.Evidence["vendor_session_id"] != "vendor-1" {
		t.Fatalf("evidence = %v, want the manifest's own pid and vendor session", f.Evidence)
	}
}

func TestReconcileAnswersNothingWithNoManifest(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	got, err := h.c.Reconcile(context.Background(), nil)

	// Assert
	if err != nil {
		t.Fatalf("Reconcile: %v", err)
	}
	if got != nil {
		t.Fatalf("dispositions = %+v, want none on an ordinary boot", got)
	}
}

func TestNoManifestWithASurvivingSessionAnswersBounceUnknown(t *testing.T) {
	// Arrange: a crash — the outgoing daemon wrote no manifest — whose shim
	// this boot adopted.
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	got, err := h.c.Reconcile(context.Background(), []AdoptedSession{{Workspace: ws, ShimPID: 4242}})

	// Assert
	if err != nil {
		t.Fatalf("Reconcile: %v", err)
	}
	if len(got) != 1 || got[0].Kind != DispositionUnknown || got[0].Workspace != ws {
		t.Fatalf("dispositions = %+v, want one UNKNOWN for the adopted workspace", got)
	}
}

func TestNoManifestWithASurvivingSessionLeavesAnOpenFault(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	if _, err := h.c.Reconcile(context.Background(), []AdoptedSession{{Workspace: ws, ShimPID: 4242}}); err != nil {
		t.Fatalf("Reconcile: %v", err)
	}
	open, err := h.db.OpenFaults(context.Background(), wsm.FaultScope{Workspace: &ws, Kind: health.KindBounceUnknown})

	// Assert
	if err != nil {
		t.Fatalf("OpenFaults: %v", err)
	}
	if len(open) != 1 {
		t.Fatalf("open faults = %+v, want one bounce_unknown: an unaccounted session is surfaced per workspace", open)
	}
}

// TestNoManifestFaultNamesTheSurvivingShimPID pins that the unaccounted
// session's record says WHICH process survived. It recorded `shim_pid: 0` —
// the zero value of an empty manifest entry, not a reading of anything — for
// the one case where a process demonstrably answered the boot's dial.
func TestNoManifestFaultNamesTheSurvivingShimPID(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	if _, err := h.c.Reconcile(context.Background(), []AdoptedSession{{Workspace: ws, ShimPID: 4242}}); err != nil {
		t.Fatalf("Reconcile: %v", err)
	}
	open, err := h.db.OpenFaults(context.Background(), wsm.FaultScope{Workspace: &ws, Kind: health.KindBounceUnknown})

	// Assert
	if err != nil {
		t.Fatalf("OpenFaults: %v", err)
	}
	if len(open) != 1 || open[0].Evidence["shim_pid"] != "4242" {
		t.Fatalf("open faults = %+v, want one naming shim_pid 4242", open)
	}
}

func TestNoManifestWithNoSurvivingSessionIsAnOrdinaryBoot(t *testing.T) {
	// Arrange: nothing survived, so nothing went unaccounted for.
	h := newHarness(t)

	// Act
	got, err := h.c.Reconcile(context.Background(), nil)

	// Assert
	if err != nil || got != nil {
		t.Fatalf("Reconcile() = (%+v, %v), want no dispositions on an ordinary boot", got, err)
	}
}

func TestADiedDispositionIsRecordedUnderTheBounceDiedKind(t *testing.T) {
	// Arrange, Act
	f := diedRecord(t)

	// Assert: the record's kind still says what became of the session.
	if f.Kind != health.KindBounceDied {
		t.Fatalf("record kind = %q, want %q", f.Kind, health.KindBounceDied)
	}
}

func TestAPreservedDispositionKeepsTheGenericKind(t *testing.T) {
	// Arrange, Act: an ordinary disposition is accounting, not a fault the
	// host view renders, so it stays under the generic kind.
	h := newHarness(t)
	_, got := reconcileOne(t, h, IntentPreserve, sessionlock.StateHeld)

	// Assert
	if len(got) != 1 || got[0].Kind != DispositionPreserved {
		t.Fatalf("dispositions = %+v, want one PRESERVED", got)
	}
	if kind := faultKind(DispositionPreserved); kind != FaultBounceDisposition {
		t.Fatalf("faultKind(PRESERVED) = %q, want %q", kind, FaultBounceDisposition)
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
	recorder := &recordingDB{DB: h.c.deps.DB}
	handle := &toggleReadOnlyDB{DB: recorder, readOnly: true}
	h.c.deps.DB = handle
	reconcileOne(t, h, IntentPreserve, sessionlock.StateFree)

	// Act
	if err := handle.Promote(context.Background()); err != nil {
		t.Fatalf("Promote: %v", err)
	}
	h.c.flushDispositions(context.Background())

	// Assert
	if len(recorder.opened) != 1 {
		t.Fatalf("records written = %d, want the deferred disposition written at the promotion", len(recorder.opened))
	}
}

// TestAHeadlessWorkspaceReconcilesSilently covers the ordinary handover of a
// workspace that was registered and never opened: it has no shim, so its free
// lock is not a session that silently died and it raises no fault.
func TestAHeadlessWorkspaceReconcilesSilently(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	ws, got := reconcileOnePID(t, h, IntentNoSession, sessionlock.StateFree, 0)
	open, err := h.db.OpenFaults(context.Background(), wsm.FaultScope{Workspace: &ws, Kind: FaultBounceDisposition})

	// Assert
	if err != nil {
		t.Fatalf("OpenFaults: %v", err)
	}
	if len(got) != 1 || got[0].Kind != DispositionPreserved {
		t.Fatalf("dispositions = %+v, want one PRESERVED record for a workspace with no shim", got)
	}
	if len(open) != 0 {
		t.Fatalf("open faults = %d, want none: a workspace that never had a shim needs no human", len(open))
	}
}

// TestAManifestEntryWithNoPidIsNeverAnUnknownDisposition covers the manifests
// an OLDER daemon wrote, which spelled every entry `preserve`. The pid is what
// names a process, so an entry without one reconciles as no-session whatever
// the intent field says.
func TestAManifestEntryWithNoPidIsNeverAnUnknownDisposition(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	_, got := reconcileOnePID(t, h, IntentPreserve, sessionlock.StateUnknown, 0)

	// Assert
	if len(got) != 1 || got[0].Kind != DispositionPreserved {
		t.Fatalf("dispositions = %+v, want PRESERVED for a pidless entry, never UNKNOWN", got)
	}
	if got[0].Intent != IntentNoSession {
		t.Fatalf("intent = %s, want the pidless entry normalized to no_session", got[0].Intent)
	}
}

// manifestExists reports whether the harness's intent manifest is on disk.
func manifestExists(t *testing.T, h *harness) bool {
	t.Helper()
	_, err := os.Stat(h.c.deps.IntentManifest)
	switch {
	case err == nil:
		return true
	case errors.Is(err, os.ErrNotExist):
		return false
	default:
		t.Fatalf("stat the intent manifest: %v", err)
		return false
	}
}

// TestReconcileRetiresTheManifestOnceEveryDispositionIsRecorded pins that a
// manifest is consumed: a manifest nothing removed was reconciled again on
// every boot for a day, re-opening the same bounce fault each time.
func TestReconcileRetiresTheManifestOnceEveryDispositionIsRecorded(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	reconcileOne(t, h, IntentPreserve, sessionlock.StateHeld)

	// Assert
	if manifestExists(t, h) {
		t.Fatalf("the intent manifest is still on disk after every disposition it names was recorded")
	}
}

func TestASecondBootFindsNoManifestToAccountFor(t *testing.T) {
	// Arrange
	h := newHarness(t)
	reconcileOne(t, h, IntentPreserve, sessionlock.StateFree)

	// Act
	got, err := h.c.Reconcile(context.Background(), nil)

	// Assert
	if err != nil || got != nil {
		t.Fatalf("second Reconcile = (%+v, %v), want no dispositions: the manifest was consumed", got, err)
	}
}

func TestAManifestWhoseAccountingWasDeferredIsKept(t *testing.T) {
	// Arrange: the joining successor's read-only handle defers every record.
	h := newHarness(t)
	h.c.deps.DB = &toggleReadOnlyDB{DB: h.c.deps.DB, readOnly: true}

	// Act
	reconcileOne(t, h, IntentPreserve, sessionlock.StateHeld)

	// Assert
	if !manifestExists(t, h) {
		t.Fatalf("the intent manifest was retired while its dispositions were only deferred")
	}
}

func TestWritingTheDeferredDispositionsRetiresTheManifest(t *testing.T) {
	// Arrange
	h := newHarness(t)
	handle := &toggleReadOnlyDB{DB: h.c.deps.DB, readOnly: true}
	h.c.deps.DB = handle
	reconcileOne(t, h, IntentPreserve, sessionlock.StateHeld)
	if err := handle.Promote(context.Background()); err != nil {
		t.Fatalf("Promote: %v", err)
	}

	// Act
	h.c.flushDispositions(context.Background())

	// Assert
	if manifestExists(t, h) {
		t.Fatalf("the intent manifest is still on disk after its deferred dispositions were written")
	}
}

func TestAManifestWhoseRecordFailedIsKept(t *testing.T) {
	// Arrange: a workspace the registry does not hold refuses its fault.
	h := newHarness(t)
	if err := h.c.writeManifest(context.Background(), Manifest{
		Daemon: ids.InstanceID("daemon-outgoing-previous"), WrittenAt: instant,
		Sessions: []ManifestSession{{
			Workspace: ids.WorkspaceID("0000000000000000"), Dir: t.TempDir(), ShimPID: 4242, Intent: IntentPreserve,
		}},
	}); err != nil {
		t.Fatalf("writeManifest: %v", err)
	}

	// Act
	if _, err := h.c.Reconcile(context.Background(), nil); err != nil {
		t.Fatalf("Reconcile: %v", err)
	}

	// Assert
	if !manifestExists(t, h) {
		t.Fatalf("the intent manifest was retired although a disposition it names was never recorded")
	}
}
