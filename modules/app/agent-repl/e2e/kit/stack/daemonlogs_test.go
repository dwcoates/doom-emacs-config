package stack

import (
	"agent-repld/integration/harness"
	"fmt"
	"os"
	"path/filepath"
	"testing"
)

// TestSweepRecordSinceIgnoresARecordBeforeTheMark is the marking rule itself,
// against the exact shape that made TestKeepAlive/RevivalAfterHibernate's pass depend on
// how busy the box was: a "parked a session idle past the cutoff" record the daemon wrote
// BEFORE the test acted (the mount-spawned session's own stand-down) must not
// satisfy a wait for the hibernation of the session the test then ran a turn
// on. Against an unmarked scan of the same sink this is the record that was
// returned.
func TestSweepRecordSinceIgnoresARecordBeforeTheMark(t *testing.T) {
	t.Parallel()
	// Arrange
	records := []harness.LogRecord{
		{Operation: HibernateSweepOp, Message: HibernateSucceededMsg},
		{Operation: HibernateSweepOp, Message: "the workspace's session is already terminal"},
	}

	// Act
	_, found := SweepRecordSince(records, SweepMark(len(records)), HibernateSucceededMsg)

	// Assert
	if found {
		t.Fatalf("sweepRecordSince matched a record written before the mark; that record is about an act the test had not yet performed")
	}
}

// TestSweepRecordSinceFindsARecordAfterTheMark is the same rule's other half:
// the record the test's own act produces is the one returned.
func TestSweepRecordSinceFindsARecordAfterTheMark(t *testing.T) {
	t.Parallel()
	// Arrange
	before := []harness.LogRecord{{Operation: HibernateSweepOp, Message: HibernateSucceededMsg}}
	after := append(append([]harness.LogRecord{}, before...),
		harness.LogRecord{Operation: HibernateSweepOp, Message: HibernateSucceededMsg, Timestamp: "the one this test caused"})

	// Act
	got, found := SweepRecordSince(after, SweepMark(len(before)), HibernateSucceededMsg)

	// Assert
	if !found || got.Timestamp != "the one this test caused" {
		t.Fatalf("sweepRecordSince(mark=%d) = %v, %t; want the record written after the mark", len(before), got, found)
	}
}

// TestSweepRecordSinceIgnoresAnotherOperationAtTheSameMessage keeps the match
// bound to the idle sweep's OWN operation: a record from a different operation
// that happens to carry the same message is not the sweep's decision.
func TestSweepRecordSinceIgnoresAnotherOperationAtTheSameMessage(t *testing.T) {
	t.Parallel()
	// Arrange
	records := []harness.LogRecord{{Operation: "daemon.workspace.bring_up", Message: HibernateSucceededMsg}}

	// Act
	_, found := SweepRecordSince(records, 0, HibernateSucceededMsg)

	// Assert
	if found {
		t.Fatalf("sweepRecordSince matched a %q record; only %q is the idle sweep's own decision",
			"daemon.workspace.bring_up", HibernateSweepOp)
	}
}

func TestConclusionBudgetRecords(t *testing.T) {
	cases := []struct {
		name    string
		message string
		want    int
	}{
		{name: "a teardown that ended its tails", message: "the session's watches were concluded", want: 0},
		{name: "a teardown that waited out a budget", message: "the WatchAgent tail on a-1 " + conclusionBudgetSpent, want: 1},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			dir := t.TempDir()
			record := fmt.Sprintf(`{"timestamp":"2026-10-13T00:00:00Z","runtime":"shim","level":"error","operation":"shim.engine.session","message":%q}`+"\n", tc.message)
			if err := os.WriteFile(filepath.Join(dir, "shim.log"), []byte(record), 0o600); err != nil {
				t.Fatal(err)
			}

			// Act
			found := ConclusionBudgetRecords(t, dir)

			// Assert
			if len(found) != tc.want {
				t.Fatalf("found %v, want %d record(s)", found, tc.want)
			}
		})
	}
}
