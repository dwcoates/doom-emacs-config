package topbar

import "testing"

func TestAnEmptyStateIsNotReady(t *testing.T) {
	// Arrange
	s := newWSState()

	// Act
	got := s.ready()

	// Assert
	if got {
		t.Fatalf("ready() = true on an empty accumulation")
	}
}

func TestEveryMissingFactIsNamed(t *testing.T) {
	// Arrange
	s := newWSState()

	// Act
	got := s.missing()

	// Assert
	want := []string{"naming", "session_started", "account", "permission_mode_picker", "context_usage"}
	if len(got) != len(want) {
		t.Fatalf("missing = %v, want %v", got, want)
	}
	for i := range want {
		if got[i] != want[i] {
			t.Fatalf("missing = %v, want %v", got, want)
		}
	}
}

func TestTheExpensiveSumIsBothCacheMissBuckets(t *testing.T) {
	// Arrange
	figures := usageFingerprint{written: 3_000, unwritten: 500, read: 90_000}

	// Act
	got := figures.misses()

	// Assert
	if got != 3_500 {
		t.Fatalf("misses() = %d, want written + unwritten and NOT the cache read", got)
	}
}

func TestSubtractingMoreThanWasCountedSaturatesAtZero(t *testing.T) {
	// Arrange
	total := usageFingerprint{written: 100}

	// Act
	got := subtractUsage(total, usageFingerprint{written: 900})

	// Assert
	if got.written != 0 {
		t.Fatalf("written = %d, want a saturating floor rather than an unsigned wrap", got.written)
	}
}

func TestSummingTwoFingerprintsAddsEveryBucket(t *testing.T) {
	// Arrange
	a := usageFingerprint{read: 1, written: 2, unwritten: 3, output: 4, thinking: 5}
	b := usageFingerprint{read: 10, written: 20, unwritten: 30, output: 40, thinking: 50}

	// Act
	got := addUsage(a, b)

	// Assert
	want := usageFingerprint{read: 11, written: 22, unwritten: 33, output: 44, thinking: 55}
	if got != want {
		t.Fatalf("addUsage = %+v, want %+v", got, want)
	}
}
