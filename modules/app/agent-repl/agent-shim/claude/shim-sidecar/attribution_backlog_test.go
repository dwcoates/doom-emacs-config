package main

import (
	"os"
	"path/filepath"
	"strconv"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/shim-claude-sidecar/internal/discover"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

// These fixtures describe the attribution boundary independently of discovery
// and the store.  In particular, the runtime-looking directory component is
// deliberately never supplied as an owner candidate.
func attributionFixture(t *testing.T, taskID, runtimeID string, modTime time.Time) HeldTarget {
	t.Helper()
	root := t.TempDir()
	path := filepath.Join(root, "claude-501", "project", runtimeID, "tasks", taskID+".output")
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatalf("mkdir spool fixture: %v", err)
	}
	if err := os.WriteFile(path, []byte("fixture output\n"), 0o600); err != nil {
		t.Fatalf("write spool fixture: %v", err)
	}
	if err := os.Chtimes(path, modTime, modTime); err != nil {
		t.Fatalf("set spool fixture modification time: %v", err)
	}
	return HeldTarget{Path: path, Root: root, TaskID: taskID, ModTime: modTime}
}

func unresolvedOwner(taskID string) OwnerResolution {
	return OwnerResolution{TaskID: taskID, Outcome: OwnerUnresolvedAwaitingOwner}
}

func resolvedTaskOwner(taskID, sessionID string) OwnerResolution {
	return OwnerResolution{TaskID: taskID, SessionID: sessionID, Source: OwnerSourceDurableOpenTask, Outcome: OwnerResolvedTask}
}

func discardHeldReport(HeldLogRecord) {}

func ownerTarget(target HeldTarget) discover.Target {
	return discover.Target{Path: target.Path, Kind: tail.KindShellSpool, TaskID: target.TaskID}
}

func observeHeld(t *testing.T, lifecycle *HeldLifecycle, target HeldTarget, owner OwnerResolution, evidence HeldEvidence, now time.Time) HeldDecision {
	t.Helper()
	decision, err := lifecycle.Observe(target, owner, evidence, now)
	if err != nil {
		t.Fatalf("Observe(%q): %v", target.Path, err)
	}
	return decision
}

func TestAttributionBacklogRestartPastLaunchUsesDurableOpenTaskOwner(t *testing.T) {
	// A restored cursor beyond the transcript's launch line must not cause the
	// spool to be held: the durable open-task identity is authoritative.
	now := time.Date(2026, time.August, 5, 14, 0, 0, 0, time.UTC)
	target := attributionFixture(t, "task-restart", "runtime-id-not-an-owner", now.Add(-time.Minute))
	lifecycle := NewHeldLifecycle(4, discardHeldReport)
	owners, _ := ownerSidecar(t)
	seeded := owners.seedOwners([]*corev1.OpenTaskState{{
		Started: &corev1.Event{
			SessionId: "session-durable",
			Payload: &corev1.Event_TaskStarted{TaskStarted: &corev1.TaskStarted{
				TaskId: target.TaskID, OutputPath: target.Path,
			}},
		},
	}})
	if seeded != 1 {
		t.Fatalf("seeded durable owner mappings = %d, want 1", seeded)
	}
	owner := owners.resolveOwnerResult(ownerTarget(target))
	if owner.Outcome != OwnerResolvedPath || owner.SessionID != "session-durable" {
		t.Fatalf("durable restarted owner resolution = %#v, want exact-path durable owner", owner)
	}

	decision := observeHeld(t, lifecycle, target, owner, HeldEvidence{
		ActiveTaskKnown:       true,
		ActiveTask:            true,
		OpenSessionKnown:      true,
		OpenSession:           true,
		CursorPastLaunchKnown: true,
		CursorPastLaunch:      true,
		ModTime:               target.ModTime,
	}, now)

	if decision.State != HeldResolved || decision.SessionID != "session-durable" {
		t.Fatalf("restart decision = %#v, want resolved durable session", decision)
	}
	if snapshot := lifecycle.Snapshot(now); snapshot.ActiveCount != 0 || snapshot.TerminalTotal != 0 {
		t.Fatalf("restart snapshot = %#v, want no backlog", snapshot)
	}
}

func TestAttributionBacklogLateOwnerArrivalReleasesActiveHold(t *testing.T) {
	now := time.Date(2026, time.August, 5, 14, 0, 0, 0, time.UTC)
	target := attributionFixture(t, "task-late-owner", "runtime-id-not-an-owner", now)
	lifecycle := NewHeldLifecycle(4, discardHeldReport)

	first := observeHeld(t, lifecycle, target, unresolvedOwner(target.TaskID), HeldEvidence{ActiveTaskKnown: true, ActiveTask: true, ModTime: target.ModTime}, now)
	if first.State != HeldActive || first.Reason != HeldMissingLiveLaunchObservation {
		t.Fatalf("initial decision = %#v, want active wait for live launch", first)
	}

	second := observeHeld(t, lifecycle, target, OwnerResolution{
		TaskID: target.TaskID, SessionID: "session-late", Source: OwnerSourceLiveLaunch, Outcome: OwnerResolvedTask,
	}, HeldEvidence{ActiveTaskKnown: true, ActiveTask: true, OpenSessionKnown: true, OpenSession: true, ModTime: target.ModTime}, now.Add(time.Second))
	if second.State != HeldResolved || second.SessionID != "session-late" {
		t.Fatalf("late-owner decision = %#v, want resolved live owner", second)
	}
	if snapshot := lifecycle.Snapshot(now.Add(time.Second)); snapshot.ActiveCount != 0 {
		t.Fatalf("late-owner snapshot = %#v, want released active hold", snapshot)
	}
}

func TestAttributionBacklogClosedHistoricalSpoolIsTerminal(t *testing.T) {
	now := time.Date(2026, time.August, 5, 14, 0, 0, 0, time.UTC)
	target := attributionFixture(t, "task-closed", "runtime-id-not-an-owner", now.Add(-48*time.Hour))
	lifecycle := NewHeldLifecycle(4, discardHeldReport)

	decision := observeHeld(t, lifecycle, target, unresolvedOwner(target.TaskID), HeldEvidence{
		ActiveTaskKnown:       true,
		ActiveTask:            false,
		OpenSessionKnown:      true,
		OpenSession:           false,
		CursorPastLaunchKnown: true,
		CursorPastLaunch:      true,
		ModTime:               target.ModTime,
	}, now)
	if decision.State != HeldTerminal || decision.Reason != HeldReasonHistorical {
		t.Fatalf("closed historical decision = %#v, want terminal historical", decision)
	}
	snapshot := lifecycle.Snapshot(now)
	if snapshot.ActiveCount != 0 || snapshot.TerminalTotal != 1 || snapshot.ByReason[HeldReasonHistorical] != 1 {
		t.Fatalf("closed historical snapshot = %#v, want one terminal historical spool", snapshot)
	}
}

func TestAttributionBacklogActiveMissingOwnerFailsReadinessWithReason(t *testing.T) {
	now := time.Date(2026, time.August, 5, 14, 0, 0, 0, time.UTC)
	target := attributionFixture(t, "task-active", "runtime-id-not-an-owner", now)
	lifecycle := NewHeldLifecycle(4, discardHeldReport)
	decision := observeHeld(t, lifecycle, target, unresolvedOwner(target.TaskID), HeldEvidence{ActiveTaskKnown: true, ActiveTask: true, ModTime: target.ModTime}, now)
	if decision.State != HeldActive || decision.Reason != HeldMissingLiveLaunchObservation {
		t.Fatalf("active missing-owner decision = %#v", decision)
	}

	readiness := lifecycle.Readiness(0, now)
	if readiness.Ready || readiness.ActiveUnresolved != 1 || readiness.Reason == "" {
		t.Fatalf("readiness = %#v, want explicit active-held failure", readiness)
	}
}

func TestAttributionBacklogDiagnosticsAreBoundedAndExplainable(t *testing.T) {
	now := time.Date(2026, time.August, 5, 14, 0, 0, 0, time.UTC)
	var reports int
	lifecycle := NewHeldLifecycle(4, func(HeldLogRecord) { reports++ })
	for i := 0; i < 20; i++ {
		target := attributionFixture(t, "task-diagnostic-"+strconv.Itoa(i), "runtime-id-not-an-owner", now.Add(-time.Duration(i)*time.Minute))
		observeHeld(t, lifecycle, target, unresolvedOwner(target.TaskID), HeldEvidence{ActiveTaskKnown: true, ActiveTask: true, ModTime: target.ModTime}, now)
	}

	snapshot := lifecycle.Snapshot(now)
	if snapshot.ActiveCount != 20 || len(snapshot.Samples) == 0 || len(snapshot.Samples) >= snapshot.ActiveCount {
		t.Fatalf("bounded diagnostics = %#v, want a nonempty bounded sample", snapshot)
	}
	if snapshot.ByReason[HeldMissingLiveLaunchObservation] != 20 || len(snapshot.ByRoot) != 20 || len(snapshot.ByAgeBucket) == 0 {
		t.Fatalf("diagnostic dimensions = %#v, want reason root and age buckets", snapshot)
	}
	for _, sample := range snapshot.Samples {
		if sample.PathHash == "" || sample.TaskID == "" || sample.Reason == "" {
			t.Fatalf("diagnostic sample = %#v, want hashed path task and reason", sample)
		}
	}
	if reports == 0 {
		t.Fatal("active attribution backlog produced no canonical diagnostic report")
	}
}

func TestAttributionBacklogNeverGuessesAcrossSimilarSpoolFilenames(t *testing.T) {
	now := time.Date(2026, time.August, 5, 14, 0, 0, 0, time.UTC)
	lifecycle := NewHeldLifecycle(4, discardHeldReport)
	first := attributionFixture(t, "task-shared-name", "runtime-session-a", now)
	second := attributionFixture(t, "task-shared-name", "runtime-session-b", now)
	owners, _ := ownerSidecar(t)
	if !owners.noteTaskOwner(first.TaskID, "session-a", first.Path, OwnerSourceLiveLaunch) {
		t.Fatal("recording first exact-path owner failed")
	}
	if !owners.noteTaskOwner(second.TaskID, "session-b", second.Path, OwnerSourceLiveLaunch) {
		t.Fatal("recording second exact-path owner failed")
	}
	firstOwner := owners.resolveOwnerResult(ownerTarget(first))
	secondOwner := owners.resolveOwnerResult(ownerTarget(second))
	if firstOwner.Outcome != OwnerResolvedPath || firstOwner.SessionID != "session-a" {
		t.Fatalf("first exact-path owner = %#v, want session-a", firstOwner)
	}
	if secondOwner.Outcome != OwnerResolvedPath || secondOwner.SessionID != "session-b" {
		t.Fatalf("second exact-path owner = %#v, want session-b", secondOwner)
	}
	unmatched := ownerTarget(second)
	unmatched.Path = filepath.Join(filepath.Dir(second.Path), "unmatched.output")
	conflict := owners.resolveOwnerResult(unmatched)
	if conflict.Outcome != OwnerUnresolvedConflict {
		t.Fatalf("task-only conflict resolution = %#v, want explicit conflict", conflict)
	}

	firstDecision := observeHeld(t, lifecycle, first, firstOwner, HeldEvidence{ActiveTaskKnown: true, ActiveTask: true, OpenSessionKnown: true, OpenSession: true, ModTime: first.ModTime}, now)
	secondDecision := observeHeld(t, lifecycle, second, conflict, HeldEvidence{ActiveTaskKnown: true, ActiveTask: true, ModTime: second.ModTime}, now)

	if firstDecision.State != HeldResolved || firstDecision.SessionID != "session-a" {
		t.Fatalf("first similar-name decision = %#v, want path-confirmed session-a", firstDecision)
	}
	if secondDecision.State != HeldTerminal || secondDecision.Reason != HeldNoAuthoritativeOwner {
		t.Fatalf("second similar-name decision = %#v, want terminal no-authoritative-owner", secondDecision)
	}
	if snapshot := lifecycle.Snapshot(now); snapshot.ActiveCount != 0 || snapshot.TerminalTotal != 1 {
		t.Fatalf("similar-name snapshot = %#v, want only terminal unresolved second spool", snapshot)
	}
}
