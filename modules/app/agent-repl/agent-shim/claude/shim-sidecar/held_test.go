package main

import (
	"strings"
	"testing"
	"time"
)

func heldTarget(id string, mod time.Time) HeldTarget {
	return HeldTarget{Path: "/tmp/claude-501/project/runtime/tasks/" + id + ".output", Root: "/tmp/claude-501", TaskID: id, ModTime: mod}
}

func heldEvidence(mod time.Time, active bool) HeldEvidence {
	return HeldEvidence{
		ModTime:         mod,
		ActiveTaskKnown: true, ActiveTask: active,
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

	decision, err := l.Observe(target, unresolvedOwner(target), heldEvidence(target.ModTime, false), now)
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

func TestHeldLifecycleTerminalSpoolWarnsOutsideTheVerboseGate(t *testing.T) {
	// Arrange — TERMINAL is final: the spool is never tailed again, so its
	// bytes never reach the store and the record must persist unconditionally.
	now := time.UnixMilli(1_000_000)
	target := heldTarget("b-old", now.Add(-UnownedSpoolWindow-time.Millisecond))
	l, records := heldLifecycle(t)
	// Act
	if _, err := l.Observe(target, unresolvedOwner(target), heldEvidence(target.ModTime, false), now); err != nil {
		t.Fatal(err)
	}
	// Assert
	var terminal []HeldLogRecord
	for _, record := range *records {
		if record.State == HeldStateTerminal {
			terminal = append(terminal, record)
		}
	}
	if len(terminal) != 1 {
		t.Fatalf("terminal records = %d, want 1", len(terminal))
	}
	if terminal[0].Level != "warn" || terminal[0].Verbose {
		t.Fatalf("terminal record level = %q verbose = %v, want warn and non-verbose", terminal[0].Level, terminal[0].Verbose)
	}
}

func TestHeldLifecycleRetriesUntilLateAuthoritativeOwnerArrives(t *testing.T) {
	now := time.UnixMilli(1_000_000)
	target := heldTarget("b-late", now)
	l, _ := heldLifecycle(t)

	first, err := l.Observe(target, unresolvedOwner(target), heldEvidence(target.ModTime, true), now)
	if err != nil || first.State != HeldStateActive || first.Reason != HeldReasonAwaitingOwner {
		t.Fatalf("first = %+v, %v; want active awaiting owner", first, err)
	}
	owner := OwnerResolution{TaskID: target.TaskID, OutputPath: target.Path, SessionID: "transcript-session", Outcome: OwnerResolvedTask}
	second, err := l.Observe(target, owner, heldEvidence(target.ModTime, true), now.Add(time.Second))
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
	evidence := heldEvidence(target.ModTime, false)
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
		if _, err := l.Observe(target, unresolvedOwner(target), heldEvidence(target.ModTime, false), now); err != nil {
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
	decision, err := l.Observe(target, owner, heldEvidence(target.ModTime, false), now)
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
	if len(*records) == 0 || (*records)[len(*records)-1].Level != "error" || !strings.Contains((*records)[len(*records)-1].Message, "error=") {
		t.Fatalf("records = %+v, want canonical error report", *records)
	}
}

func TestHeldLifecycleRejectsInvalidInputsWithoutMutation(t *testing.T) {
	now := time.UnixMilli(1_000_000)
	baseTarget := heldTarget("b-invalid-input", now)
	baseOwner := unresolvedOwner(baseTarget)
	baseEvidence := HeldEvidence{ModTime: baseTarget.ModTime, ActiveTaskKnown: true}
	tests := []struct {
		name     string
		target   HeldTarget
		owner    OwnerResolution
		evidence HeldEvidence
		now      time.Time
	}{
		{name: "missing root", target: func() HeldTarget { v := baseTarget; v.Root = ""; return v }(), owner: baseOwner, evidence: baseEvidence, now: now},
		{name: "evidence modtime mismatch", target: baseTarget, owner: baseOwner, evidence: func() HeldEvidence { v := baseEvidence; v.ModTime = now.Add(-time.Second); return v }(), now: now},
		{name: "future stat time", target: func() HeldTarget { v := baseTarget; v.ModTime = now.Add(time.Second); return v }(), owner: baseOwner, evidence: HeldEvidence{ActiveTaskKnown: true}, now: now},
		{name: "owner task mismatch", target: baseTarget, owner: func() OwnerResolution { v := baseOwner; v.TaskID = "other"; return v }(), evidence: baseEvidence, now: now},
		{name: "invalid owner metadata", target: baseTarget, owner: OwnerResolution{TaskID: baseTarget.TaskID, OutputPath: baseTarget.Path, Outcome: OwnerUnresolvedInvalid}, evidence: baseEvidence, now: now},
		{name: "resolved owner missing session", target: baseTarget, owner: OwnerResolution{TaskID: baseTarget.TaskID, OutputPath: baseTarget.Path, Outcome: OwnerResolvedTask}, evidence: baseEvidence, now: now},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			l, records := heldLifecycle(t)
			if _, err := l.Observe(tc.target, tc.owner, tc.evidence, tc.now); err == nil {
				t.Fatal("Observe accepted invalid input")
			}
			if got := (*records)[len(*records)-1]; got.Level != "error" || !strings.Contains(got.Message, "error=") {
				t.Fatalf("last record = %+v, want canonical error", got)
			}
			if len(l.active) != 0 || len(l.terminal) != 0 {
				t.Fatalf("invalid input mutated lifecycle: active=%v terminal=%v", l.active, l.terminal)
			}
		})
	}
}

func TestHeldReadinessRejectsNegativeThresholdLoudly(t *testing.T) {
	now := time.UnixMilli(1_000_000)
	l, records := heldLifecycle(t)
	defer func() {
		if recover() == nil {
			t.Fatal("Readiness accepted a negative threshold")
		}
		if got := (*records)[len(*records)-1]; got.Operation != "held-readiness" || got.Level != "error" {
			t.Fatalf("last record = %+v, want readiness error", got)
		}
	}()
	l.Readiness(-1, now)
}
