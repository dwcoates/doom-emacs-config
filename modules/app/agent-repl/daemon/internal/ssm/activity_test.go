package ssm

import (
	"testing"
)

// LastActivityMs dates a workspace off its own state log, which is what lets a
// caller gate a teardown on real elapsed quiet rather than on a single flag.

func TestLastActivityDatesTheWorkspaceOffItsNewestRow(t *testing.T) {
	// Arrange — two edges, so the newest one is the answer and the older is not.
	m, _, _ := openUnwiredTest(t, fakeResolver{})
	clock := int64(1000)
	m.clock = func() int64 { return clock }
	if err := m.ApplyWired("ws", WiringStarting, "bring_up"); err != nil {
		t.Fatalf("ApplyWired starting: %v", err)
	}
	clock = 5000
	if err := m.ApplyWired("ws", WiringWired, "shim_ready"); err != nil {
		t.Fatalf("ApplyWired wired: %v", err)
	}

	// Act.
	at, dated, err := m.LastActivityMs("ws")
	if err != nil {
		t.Fatalf("LastActivityMs: %v", err)
	}

	// Assert.
	if !dated {
		t.Fatal("dated = false, want the workspace dated by its log")
	}
	if at != 5000 {
		t.Errorf("last activity = %d, want the newest row at 5000", at)
	}
}

func TestLastActivityReportsUndatedForAWorkspaceWithNoRows(t *testing.T) {
	// Arrange — a workspace nothing has ever been recorded about. Answering with
	// a zero timestamp would read as "idle since the epoch" to any caller.
	m, _, _ := openUnwiredTest(t, fakeResolver{})

	// Act.
	at, dated, err := m.LastActivityMs("never-seen")
	if err != nil {
		t.Fatalf("LastActivityMs: %v", err)
	}

	// Assert.
	if dated {
		t.Fatalf("dated = true (at %d), want the unknown workspace reported as undated", at)
	}
}

func TestLastActivityRejectsAnEmptyWorkspace(t *testing.T) {
	// Arrange.
	m, _, _ := openUnwiredTest(t, fakeResolver{})

	// Act.
	_, _, err := m.LastActivityMs("")

	// Assert.
	if err == nil {
		t.Fatal("LastActivityMs(\"\") = nil error, want a loud rejection")
	}
}
