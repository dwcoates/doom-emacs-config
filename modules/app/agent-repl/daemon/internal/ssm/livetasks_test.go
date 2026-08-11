package ssm

import (
	"reflect"
	"testing"
)

// seedTaskRow appends one task lifecycle row directly, mirroring what the task
// sinks write. taskID "" seeds the ANONYMOUS leg — a start with no identity.
func seedTaskRow(t *testing.T, m *Manager, ws, state, taskID string, at int64) {
	t.Helper()
	seedTaskSignal(t, m.db, ws, "s1", state, state, at, at, taskID)
}

// TestLiveTaskIDsNamesStartsWithoutEnds is the ordinary read: identities, not a
// count.
func TestLiveTaskIDsNamesStartsWithoutEnds(t *testing.T) {
	// Arrange
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	seedTaskRow(t, m, "ws1", sigTaskStarted, "live-b", 1)
	seedTaskRow(t, m, "ws1", sigTaskStarted, "done-a", 2)
	seedTaskRow(t, m, "ws1", sigTaskStarted, "live-a", 3)
	seedTaskRow(t, m, "ws1", sigTaskEnded, "done-a", 4)

	// Act
	ids, anonymous, err := m.LiveTaskIDs("ws1")

	// Assert
	if err != nil {
		t.Fatalf("LiveTaskIDs: %v", err)
	}
	if anonymous != 0 {
		t.Fatalf("anonymous = %d, want 0", anonymous)
	}
	if !reflect.DeepEqual(ids, []string{"live-a", "live-b"}) {
		t.Fatalf("LiveTaskIDs = %v, want the two unmatched starts in a stable order", ids)
	}
}

// TestLiveTaskIDsIgnoresAnUnmatchedEnd mirrors resolve.go's rule: an end with no
// observed start is an anomaly for the ingestion edge, never a negative
// contribution to the live set.
func TestLiveTaskIDsIgnoresAnUnmatchedEnd(t *testing.T) {
	// Arrange
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	seedTaskRow(t, m, "ws1", sigTaskEnded, "ghost", 1)

	// Act
	ids, anonymous, err := m.LiveTaskIDs("ws1")

	// Assert
	if err != nil {
		t.Fatalf("LiveTaskIDs: %v", err)
	}
	if len(ids) != 0 || anonymous != 0 {
		t.Fatalf("LiveTaskIDs = %v anonymous=%d, want nothing live", ids, anonymous)
	}
}

// TestLiveTaskIDsReportsAnonymousStartsSeparately is the rule that keeps the
// set honest: a live task with no identity cannot be a member, and dropping it
// silently would UNDERSTATE what is running.
func TestLiveTaskIDsReportsAnonymousStartsSeparately(t *testing.T) {
	// Arrange
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	seedTaskRow(t, m, "ws1", sigTaskStarted, "", 1)
	seedTaskRow(t, m, "ws1", sigTaskStarted, "named", 2)

	// Act
	ids, anonymous, err := m.LiveTaskIDs("ws1")

	// Assert
	if err != nil {
		t.Fatalf("LiveTaskIDs: %v", err)
	}
	if anonymous != 1 {
		t.Fatalf("anonymous = %d, want 1", anonymous)
	}
	if !reflect.DeepEqual(ids, []string{"named"}) {
		t.Fatalf("LiveTaskIDs = %v, want only the identified member", ids)
	}
}

// TestLiveTaskIDsAnonymousLegIsFlooredAtZero covers more anonymous ends than
// starts, which must not read as a negative live population.
func TestLiveTaskIDsAnonymousLegIsFlooredAtZero(t *testing.T) {
	// Arrange
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	seedTaskRow(t, m, "ws1", sigTaskEnded, "", 1)
	seedTaskRow(t, m, "ws1", sigTaskEnded, "", 2)

	// Act
	_, anonymous, err := m.LiveTaskIDs("ws1")

	// Assert
	if err != nil {
		t.Fatalf("LiveTaskIDs: %v", err)
	}
	if anonymous != 0 {
		t.Fatalf("anonymous = %d, want it floored at 0", anonymous)
	}
}

// TestLiveTaskIDsOfAnUnknownWorkspaceIsEmptyNotAnError pins that a workspace
// with no rows answers "nothing", the same way ActiveTurnIDs does.
func TestLiveTaskIDsOfAWorkspaceWithNoRowsIsEmpty(t *testing.T) {
	// Arrange
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})

	// Act
	ids, anonymous, err := m.LiveTaskIDs("never-seen")

	// Assert
	if err != nil {
		t.Fatalf("LiveTaskIDs: %v", err)
	}
	if len(ids) != 0 || anonymous != 0 {
		t.Fatalf("LiveTaskIDs = %v anonymous=%d, want nothing", ids, anonymous)
	}
}

// TestLiveTaskIDsRefusesAnEmptyWorkspace covers the validation refusal.
func TestLiveTaskIDsRefusesAnEmptyWorkspace(t *testing.T) {
	// Arrange
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})

	// Act
	_, _, err := m.LiveTaskIDs("")

	// Assert
	if err == nil {
		t.Fatal("LiveTaskIDs accepted an empty workspace")
	}
}

// TestLiveTaskIDsAgreesWithTheCountItIsFoldedBeside pins the two reads of the
// same rows against each other: the identified members plus the anonymous leg
// must equal resolve.go's live_task_count, or the badge and the bounce decision
// are describing different populations.
func TestLiveTaskIDsAgreesWithLiveTaskCount(t *testing.T) {
	// Arrange
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	seedSignal(t, m.db, "ws1", "s1", sigIdle, causeSessionStarted, 0, 1)
	seedTaskRow(t, m, "ws1", sigTaskStarted, "a", 2)
	seedTaskRow(t, m, "ws1", sigTaskStarted, "b", 3)
	seedTaskRow(t, m, "ws1", sigTaskStarted, "", 4)
	seedTaskRow(t, m, "ws1", sigTaskEnded, "a", 5)

	// Act
	ids, anonymous, err := m.LiveTaskIDs("ws1")
	if err != nil {
		t.Fatalf("LiveTaskIDs: %v", err)
	}
	got, err := resolve(m.db, "ws1", nil)
	if err != nil {
		t.Fatalf("resolve: %v", err)
	}

	// Assert
	if int64(len(ids))+anonymous != got.liveTaskCount {
		t.Fatalf("identified=%d + anonymous=%d != live_task_count=%d; the badge and the bounce decision would describe different populations",
			len(ids), anonymous, got.liveTaskCount)
	}
}
