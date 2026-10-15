package harness

import (
	"strings"
	"testing"
)

func TestARecordingTBRecordsAnErrorWithoutFailingTheRealTest(t *testing.T) {
	// Arrange
	r := NewRecordingTB(t, "TestInner")

	// Act
	r.Errorf("bad %d", 1)

	// Assert
	if !r.Failed() || r.Failures() != "bad 1" || t.Failed() {
		t.Fatalf("Failed=%v Failures=%q real failed=%v, want only the recording failed", r.Failed(), r.Failures(), t.Failed())
	}
}

func TestARecordingTBFatalEndsTheActAndIsRecorded(t *testing.T) {
	// Arrange
	r := NewRecordingTB(t, "TestInner")
	after := false

	// Act
	completed := Within(func() {
		r.Fatalf("stop")
		after = true
	})

	// Assert
	if completed || after || r.Failures() != "stop" {
		t.Fatalf("completed=%v after=%v Failures=%q, want the act ended at the Fatalf", completed, after, r.Failures())
	}
}

func TestARecordingTBSkipEndsTheActAndIsRecordedAsASkip(t *testing.T) {
	// Arrange
	r := NewRecordingTB(t, "TestInner")

	// Act
	completed := Within(func() { r.Skipf("no %s", "node") })

	// Assert
	if completed || r.Failures() != "SKIP: no node" {
		t.Fatalf("completed=%v Failures=%q, want the act ended and the skip recorded", completed, r.Failures())
	}
}

func TestARecordingTBRunsItsCleanupsLastRegisteredFirst(t *testing.T) {
	// Arrange
	r := NewRecordingTB(t, "TestInner")
	var order []string
	r.Cleanup(func() { order = append(order, "first") })
	r.Cleanup(func() { order = append(order, "second") })

	// Act
	r.RunCleanups()

	// Assert
	if strings.Join(order, ",") != "second,first" {
		t.Fatalf("order = %v, want second,first", order)
	}
}

func TestWithinAnswersCompletedForAnActThatFinishes(t *testing.T) {
	// Act
	completed := Within(func() {})

	// Assert
	if !completed {
		t.Fatal("Within = false, want true for an act that ran to its end")
	}
}
