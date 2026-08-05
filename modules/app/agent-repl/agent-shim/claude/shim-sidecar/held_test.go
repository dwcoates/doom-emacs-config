package main

import (
	"strings"
	"testing"
	"time"
)

func heldTarget(id string, mod time.Time) HeldTarget {
	return HeldTarget{Path: "/tmp/claude-501/project/runtime/tasks/" + id + ".output", Root: "/tmp/claude-501", TaskID: id, ModTime: mod}
}

func heldEvidence(mod time.Time, active, open, cursorPast bool) HeldEvidence {
	return HeldEvidence{
		ModTime:         mod,
		ActiveTaskKnown: true, ActiveTask: active,
		OpenSessionKnown: true, OpenSession: open,
		CursorPastLaunchKnown: true, CursorPastLaunch: cursorPast,
	}
}

func unresolvedOwner(t HeldTarget) OwnerResolution {
	return OwnerResolution{TaskID: t.TaskID, OutputPath: t.Path, Outcome: OwnerUnresolvedAwaitingOwner}
}

func heldLifecycle(t *testing.T) (*HeldLifecycle, *[]HeldLogRecord) {
	t.Helper()
	var records []HeldLogRecord
	return NewHeldLifecycle(2, func(record HeldLogRecord) { records = append(records, record) }), &records
}

func TestHeldLifecycleTerminatesHistoricalSpoolDespiteAwaitingOwner(t *testing.T) {
	now := time.UnixMilli(1_000_000)
	target := heldTarget("b-old", now.Add(-UnownedSpoolWindow-time.Millisecond))
	l, _ := heldLifecycle(t)

	decision, err := l.Observe(target, unresolvedOwner(target), heldEvidence(target.ModTime, false, false, true), now)
	if err != nil {
		t.Fatal(err)
	}
	if decision.State != HeldStateTerminal || decision.Reason != HeldReasonHistorical || decision.SessionID != "" {
		t.Fatalf("decision = %+v, want historical terminal without session", decision)
	}
	if got := l.Snapshot(now); got.ActiveCount != 0 || got.TerminalTotal != 1 || got.ByReason[HeldReasonHistorical] != 1 {
		t.Fatalf("snapshot = %+v, want one historical terminal and no active holds", got)
	}
}

func TestHeldLifecycleRetriesUntilLateAuthoritativeOwnerArrives(t *testing.T) {
	now := time.UnixMilli(1_000_000)
	target := heldTarget("b-late", now)
	l, _ := heldLifecycle(t)

	first, err := l.Observe(target, unresolvedOwner(target), heldEvidence(target.ModTime, true, true, false), now)
	if err != nil || first.State != HeldStateActive || first.Reason != HeldReasonAwaitingOwner {
		t.Fatalf("first = %+v, %v; want active awaiting owner", first, err)
	}
	owner := OwnerResolution{TaskID: target.TaskID, OutputPath: target.Path, SessionID: "transcript-session", Outcome: OwnerResolvedTask}
	second, err := l.Observe(target, owner, heldEvidence(target.ModTime, true, true, false), now.Add(time.Second))
	if err != nil || second.State != HeldStateResolved || second.SessionID != "transcript-session" || second.Reason != "" {
		t.Fatalf("second = %+v, %v; want resolved transcript owner", second, err)
	}
	if got := l.Snapshot(now); got.ActiveCount != 0 || got.TerminalTotal != 0 {
		t.Fatalf("snapshot = %+v, want no remaining lifecycle entries", got)
	}
}

func TestHeldLifecycleTerminalObservationIsIdempotent(t *testing.T) {
	now := time.UnixMilli(1_000_000)
	target := heldTarget("b-terminal", now.Add(-UnownedSpoolWindow-time.Millisecond))
	l, _ := heldLifecycle(t)
	evidence := heldEvidence(target.ModTime, false, false, true)
	first, err := l.Observe(target, unresolvedOwner(target), evidence, now)
	if err != nil || first.State != HeldStateTerminal {
		t.Fatalf("first = %+v, %v", first, err)
	}
	second, err := l.Observe(target, unresolvedOwner(target), evidence, now.Add(time.Second))
	if err != nil || second != first {
		t.Fatalf("second = %+v, %v; want idempotent %+v", second, err, first)
	}
}

func TestHeldLifecycleReadinessIgnoresTerminalHistoryAndBoundsSamples(t *testing.T) {
	now := time.UnixMilli(1_000_000)
	l, _ := heldLifecycle(t)
	for _, id := range []string{"b-1", "b-2", "b-3"} {
		target := heldTarget(id, now.Add(-UnownedSpoolWindow-time.Millisecond))
		if _, err := l.Observe(target, unresolvedOwner(target), heldEvidence(target.ModTime, false, false, true), now); err != nil {
			t.Fatal(err)
		}
	}
	readiness := l.Readiness(0, now)
	if !readiness.Ready || readiness.ActiveUnresolved != 0 || readiness.HistoricalTerminal != 3 {
		t.Fatalf("readiness = %+v, want terminal history excluded from readiness", readiness)
	}
	snapshot := l.Snapshot(now)
	if len(snapshot.Samples) != 2 {
		t.Fatalf("samples = %d, want bounded 2", len(snapshot.Samples))
	}
	for _, sample := range snapshot.Samples {
		if strings.Contains(sample.PathHash, "/") || sample.PathHash == "" {
			t.Fatalf("sample path hash = %q, must not expose path", sample.PathHash)
		}
	}
}

func TestHeldLifecycleKeepsRecentConflictActiveForExactPathResolution(t *testing.T) {
	now := time.UnixMilli(1_000_000)
	target := heldTarget("b-conflict", now)
	l, _ := heldLifecycle(t)
	owner := OwnerResolution{TaskID: target.TaskID, OutputPath: target.Path, Outcome: OwnerUnresolvedConflict}
	decision, err := l.Observe(target, owner, heldEvidence(target.ModTime, false, false, false), now)
	if err != nil {
		t.Fatal(err)
	}
	if decision.State != HeldStateActive || decision.Reason != HeldReasonConflict {
		t.Fatalf("decision = %+v, want active conflicting-owner anomaly", decision)
	}
	if readiness := l.Readiness(0, now); readiness.Ready || readiness.ActiveUnresolved != 1 {
		t.Fatalf("readiness = %+v, want conflict counted as active unresolved", readiness)
	}
}

func TestHeldLifecycleRejectsMissingAuthoritativeTaskEvidenceLoudly(t *testing.T) {
	now := time.UnixMilli(1_000_000)
	target := heldTarget("b-invalid", now)
	l, records := heldLifecycle(t)
	evidence := HeldEvidence{ModTime: target.ModTime}
	if _, err := l.Observe(target, unresolvedOwner(target), evidence, now); err == nil {
		t.Fatal("Observe accepted missing authoritative task evidence")
	}
	if len(*records) == 0 || !strings.Contains((*records)[len(*records)-1].Message, "error=") {
		t.Fatalf("records = %+v, want canonical error report", *records)
	}
}
