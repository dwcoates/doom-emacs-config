package server

import (
	"errors"
	"fmt"
	"strings"
	"testing"

	"claude-repld/internal/inflight"
)

// fakeInFlight answers the manifest's "what is in flight" question.
type fakeInFlight struct {
	set inflight.Set
}

func (f fakeInFlight) InFlight(string) inflight.Set { return f.set }

// fakeTerminals answers the manifest's "did it end legitimately" question.
type fakeTerminals struct {
	terminated map[string]bool
	err        error
}

func (f fakeTerminals) WorkItemTerminated(_, kind, id string) (bool, string, error) {
	if f.err != nil {
		return false, "", f.err
	}
	if f.terminated[kind+":"+id] {
		return true, "a terminal record exists", nil
	}
	return false, "no terminal record exists", nil
}

func capture() (func(string, ...any), *[]string) {
	var records []string
	return func(format string, args ...any) {
		records = append(records, fmt.Sprintf(format, args...))
	}, &records
}

func held(records *[]string, want string) bool {
	for _, rec := range *records {
		if strings.Contains(rec, want) {
			return true
		}
	}
	return false
}

// TestBounceManifestJudgesAnInterruptedItem is the whole mechanism end to end:
// an item live before the bounce, gone after it, with no terminal record.
func TestBounceManifestJudgesAnInterruptedItem(t *testing.T) {
	// Arrange — a workspace holding one task when the bounce starts.
	ws := t.TempDir()
	item := inflight.Item{Kind: inflight.KindTask, ID: "task-1"}
	logf, records := capture()
	RecordBounceStart(logf, map[string]string{"s1": ws},
		fakeInFlight{set: inflight.MustAnswered(ws, item)}, "b1", "deploy")

	// Act — the incoming daemon sees nothing live and no terminal was recorded.
	tally, err := ReportBounceEnd(logf, ws,
		fakeInFlight{set: inflight.MustAnswered(ws)},
		fakeTerminals{terminated: map[string]bool{}})

	// Assert
	if err != nil {
		t.Fatalf("ReportBounceEnd: %v", err)
	}
	if tally[inflight.DispositionInterrupted] != 1 {
		t.Fatalf("tally = %v, want one INTERRUPTED", tally)
	}
	if !held(records, "WORK LOST") {
		t.Fatalf("records = %v, want the lost-work line", *records)
	}
}

// TestBounceManifestJudgesACompletedItem covers the arm that requires a
// recorded terminal, which is what separates a completion from a death.
func TestBounceManifestJudgesACompletedItem(t *testing.T) {
	// Arrange
	ws := t.TempDir()
	item := inflight.Item{Kind: inflight.KindTask, ID: "task-1"}
	logf, _ := capture()
	RecordBounceStart(logf, map[string]string{"s1": ws},
		fakeInFlight{set: inflight.MustAnswered(ws, item)}, "b1", "deploy")

	// Act
	tally, err := ReportBounceEnd(logf, ws,
		fakeInFlight{set: inflight.MustAnswered(ws)},
		fakeTerminals{terminated: map[string]bool{"task:task-1": true}})

	// Assert
	if err != nil {
		t.Fatalf("ReportBounceEnd: %v", err)
	}
	if tally[inflight.DispositionCompleted] != 1 {
		t.Fatalf("tally = %v, want one COMPLETED", tally)
	}
}

// TestBounceManifestJudgesAPreservedItem covers the promise being kept.
func TestBounceManifestJudgesAPreservedItem(t *testing.T) {
	// Arrange
	ws := t.TempDir()
	item := inflight.Item{Kind: inflight.KindTask, ID: "task-1"}
	logf, _ := capture()
	RecordBounceStart(logf, map[string]string{"s1": ws},
		fakeInFlight{set: inflight.MustAnswered(ws, item)}, "b1", "deploy")

	// Act
	tally, err := ReportBounceEnd(logf, ws,
		fakeInFlight{set: inflight.MustAnswered(ws, item)},
		fakeTerminals{terminated: map[string]bool{}})

	// Assert
	if err != nil {
		t.Fatalf("ReportBounceEnd: %v", err)
	}
	if tally[inflight.DispositionPreserved] != 1 {
		t.Fatalf("tally = %v, want one PRESERVED", tally)
	}
}

// TestBounceManifestNeverFoldsAnUnreadableTerminalIntoCompleted is the rule the
// whole taxonomy turns on: a fate nobody could observe is UNKNOWN.
func TestBounceManifestNeverFoldsAnUnreadableTerminalIntoCompleted(t *testing.T) {
	// Arrange
	ws := t.TempDir()
	item := inflight.Item{Kind: inflight.KindQuery, ID: "q-1"}
	logf, _ := capture()
	RecordBounceStart(logf, map[string]string{"s1": ws},
		fakeInFlight{set: inflight.MustAnswered(ws, item)}, "b1", "deploy")

	// Act — the query plane records no terminal in this ledger.
	tally, err := ReportBounceEnd(logf, ws,
		fakeInFlight{set: inflight.MustAnswered(ws)},
		fakeTerminals{err: errors.New("this ledger records no terminal for a query")})

	// Assert
	if err != nil {
		t.Fatalf("ReportBounceEnd: %v", err)
	}
	if tally[inflight.DispositionUnknown] != 1 || tally[inflight.DispositionCompleted] != 0 {
		t.Fatalf("tally = %v, want one UNKNOWN and no COMPLETED", tally)
	}
}

// TestBounceManifestSurvivesTheBounceItDescribes pins the observability
// requirement: the START and END records are written by two separate calls with
// no descriptor held between them, and both are readable afterwards.
func TestBounceManifestSurvivesTheBounceItDescribes(t *testing.T) {
	// Arrange
	ws := t.TempDir()
	logf, _ := capture()
	RecordBounceStart(logf, map[string]string{"s1": ws},
		fakeInFlight{set: inflight.MustAnswered(ws, inflight.Item{Kind: inflight.KindTask, ID: "task-1"})}, "b1", "deploy")

	// Act
	if _, err := ReportBounceEnd(logf, ws,
		fakeInFlight{set: inflight.MustAnswered(ws)},
		fakeTerminals{terminated: map[string]bool{}}); err != nil {
		t.Fatalf("ReportBounceEnd: %v", err)
	}
	got, err := inflight.Read(ws)

	// Assert
	if err != nil {
		t.Fatalf("Read: %v", err)
	}
	if len(got) != 2 || got[0].Phase != inflight.PhaseStart || got[1].Phase != inflight.PhaseEnd {
		t.Fatalf("manifest = %+v, want a START followed by its END", got)
	}
}

// TestBounceManifestMakesNoClaimWithoutAPredecessorStart pins that a missing
// START is reported as a blind spot rather than as a clean bounce.
func TestBounceManifestMakesNoClaimWithoutAPredecessorStart(t *testing.T) {
	// Arrange
	ws := t.TempDir()
	logf, records := capture()

	// Act
	tally, err := ReportBounceEnd(logf, ws,
		fakeInFlight{set: inflight.MustAnswered(ws)},
		fakeTerminals{terminated: map[string]bool{}})

	// Assert
	if err != nil || tally != nil {
		t.Fatalf("ReportBounceEnd = (%v, %v), want no claim and no error", tally, err)
	}
	if !held(records, "no predecessor START record") {
		t.Fatalf("records = %v, want the missing-start blind spot recorded", *records)
	}
}

// TestBounceManifestCannotLaunderAnUnknownPreStateIntoAJudgeableOne pins the
// round trip: a START that could not observe its workspace rebuilds as UNKNOWN.
func TestBounceManifestKeepsAnUnknownPreStateUnknown(t *testing.T) {
	// Arrange
	ws := t.TempDir()
	logf, records := capture()
	RecordBounceStart(logf, map[string]string{"s1": ws},
		fakeInFlight{set: inflight.Unanswered(ws, "the shim never answered")}, "b1", "deploy")

	// Act
	_, err := ReportBounceEnd(logf, ws,
		fakeInFlight{set: inflight.MustAnswered(ws)},
		fakeTerminals{terminated: map[string]bool{}})

	// Assert
	if !errors.Is(err, inflight.ErrBeforeUnknown) {
		t.Fatalf("ReportBounceEnd err = %v, want ErrBeforeUnknown", err)
	}
	if !held(records, "UNJUDGEABLE") {
		t.Fatalf("records = %v, want the unjudgeable bounce recorded", *records)
	}
}

// TestRecordBounceStartWithNoSourceSaysSo pins that an unwired manifest is a
// stated blind spot, never a silent one.
func TestRecordBounceStartWithNoSourceSaysSo(t *testing.T) {
	// Arrange
	logf, records := capture()

	// Act
	RecordBounceStart(logf, map[string]string{"s1": t.TempDir()}, nil, "b1", "deploy")

	// Assert
	if !held(records, "makes NO claim") {
		t.Fatalf("records = %v, want the unwired manifest stated", *records)
	}
}

// TestReportBounceEndRequiresBothSources pins that judging a vanished item
// without a terminal source could only guess between COMPLETED and INTERRUPTED.
func TestReportBounceEndRequiresBothSources(t *testing.T) {
	// Arrange
	logf, _ := capture()

	// Act
	_, err := ReportBounceEnd(logf, t.TempDir(), fakeInFlight{set: inflight.MustAnswered("/w")}, nil)

	// Assert
	if err == nil {
		t.Fatal("ReportBounceEnd accepted a missing terminal source")
	}
}
