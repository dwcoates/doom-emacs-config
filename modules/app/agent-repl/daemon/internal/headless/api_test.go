package headless

import (
	"errors"
	"testing"
)

func TestResolveBinPrefersTheConfiguredBinary(t *testing.T) {
	// Arrange.
	t.Setenv(EnvClaudeBin, "/from/env")

	// Act.
	got := ResolveBin("/configured")

	// Assert.
	if got != "/configured" {
		t.Fatalf("ResolveBin() = %q, want the configured binary", got)
	}
}

func TestResolveBinFallsBackToTheEnvironment(t *testing.T) {
	// Arrange.
	t.Setenv(EnvClaudeBin, "/from/env")

	// Act.
	got := ResolveBin("")

	// Assert.
	if got != "/from/env" {
		t.Fatalf("ResolveBin() = %q, want the environment's binary", got)
	}
}

// TestResolveBinNeverAnswersEmpty pins the live defect this package closed:
// buildJudge handed the classifier an empty binary, and every classification
// refused before it reached the model.
func TestResolveBinNeverAnswersEmpty(t *testing.T) {
	// Arrange.
	t.Setenv(EnvClaudeBin, "")

	// Act.
	got := ResolveBin("")

	// Assert.
	if got != DefaultBin {
		t.Fatalf("ResolveBin() = %q, want %q", got, DefaultBin)
	}
}

func TestBinSourceNamesWhereTheBinaryCameFrom(t *testing.T) {
	// Arrange.
	tests := []struct {
		name       string
		configured string
		env        string
		want       string
	}{
		{name: "configured", configured: "/x", env: "/y", want: "configured"},
		{name: "environment", configured: "", env: "/y", want: EnvClaudeBin},
		{name: "default", configured: "", env: "", want: "default"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Setenv(EnvClaudeBin, tt.env)

			// Act.
			got := BinSource(tt.configured)

			// Assert.
			if got != tt.want {
				t.Fatalf("BinSource(%q) = %q, want %q", tt.configured, got, tt.want)
			}
		})
	}
}

func TestCauseOfReadsAHeadlessError(t *testing.T) {
	// Arrange.
	err := &Error{Cause: CauseGuardRefused, Detail: "forbidden"}

	// Act, Assert.
	if got := CauseOf(err); got != CauseGuardRefused {
		t.Fatalf("CauseOf(*Error) = %q, want %q", got, CauseGuardRefused)
	}
}

func TestCauseOfReportsExitStatusForANonHeadlessError(t *testing.T) {
	// Arrange.
	err := errors.New("some other failure")

	// Act, Assert — never an empty cause, so the arm always names something.
	if got := CauseOf(err); got != CauseExitStatus {
		t.Fatalf("CauseOf(plain) = %q, want %q", got, CauseExitStatus)
	}
}

func TestNewResolvesTheBinary(t *testing.T) {
	// Arrange.
	t.Setenv(EnvClaudeBin, "/from/env")

	// Act.
	c := New(permissiveGuard(t), "")

	// Assert.
	if c.Bin() != "/from/env" {
		t.Fatalf("Bin() = %q, want the resolved binary", c.Bin())
	}
}
