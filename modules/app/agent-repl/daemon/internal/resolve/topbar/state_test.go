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
	// THE WORKSPACE FACTS AND NOTHING ELSE. No session fact is a gate under
	// the fixed-schema ruling: each session-scoped cell states its own
	// not-yet-known in its own slot instead.
	want := []string{"naming", "account"}
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

func TestReadinessGatesAParkedWorkspaceOnTheWorkspaceFactsAlone(t *testing.T) {
	for _, tc := range []struct {
		name  string
		state func(*wsState)
		want  bool
	}{
		{
			name:  "the naming and the account are in hand",
			state: func(s *wsState) { s.namingSet, s.accountSet = true, true },
			want:  true,
		},
		{
			name:  "the naming has not arrived",
			state: func(s *wsState) { s.accountSet = true },
			want:  false,
		},
		{
			name:  "the account has not been read",
			state: func(s *wsState) { s.namingSet = true },
			want:  false,
		},
	} {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s := newWSState()
			s.parked = true
			tc.state(s)

			// Act
			got := s.ready()

			// Assert
			if got != tc.want {
				t.Fatalf("ready() = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestAParkedWorkspaceIsNeverReportedAsAwaitingASessionFact(t *testing.T) {
	// Arrange
	s := newWSState()
	s.parked = true

	// Act
	got := s.missing()

	// Assert
	want := []string{"naming", "account"}
	if len(got) != len(want) {
		t.Fatalf("missing() = %v, want %v", got, want)
	}
	for i := range want {
		if got[i] != want[i] {
			t.Fatalf("missing() = %v, want %v", got, want)
		}
	}
}

func TestReadinessGatesAColdGatedWorkspaceOnTheWorkspaceFactsAlone(t *testing.T) {
	for _, tc := range []struct {
		name  string
		state func(*wsState)
		want  bool
	}{
		{
			name:  "the naming and the account are in hand",
			state: func(s *wsState) { s.namingSet, s.accountSet = true, true },
			want:  true,
		},
		{
			name:  "the naming has not arrived",
			state: func(s *wsState) { s.accountSet = true },
			want:  false,
		},
		{
			name:  "the account has not been read",
			state: func(s *wsState) { s.namingSet = true },
			want:  false,
		},
	} {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s := newWSState()
			s.coldGate = true
			tc.state(s)

			// Act
			got := s.ready()

			// Assert
			if got != tc.want {
				t.Fatalf("ready() = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestAColdGatedWorkspaceIsNeverReportedAsAwaitingASessionFact(t *testing.T) {
	// Arrange
	s := newWSState()
	s.coldGate = true

	// Act
	got := s.missing()

	// Assert
	want := []string{"naming", "account"}
	if len(got) != len(want) {
		t.Fatalf("missing() = %v, want %v", got, want)
	}
	for i := range want {
		if got[i] != want[i] {
			t.Fatalf("missing() = %v, want %v", got, want)
		}
	}
}
