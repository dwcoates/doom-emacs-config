package e2e

import "testing"

// TestEverySandboxValueOfOneTestSharesItsScratch pins that the scratch is the
// TEST's: a scenario helper and its caller each hold their own sandbox value,
// and the daemon exempts only the one scratch the world states.
func TestEverySandboxValueOfOneTestSharesItsScratch(t *testing.T) {
	// Arrange
	first := &localSandbox{t: t}
	second := &localSandbox{t: t}

	// Act
	a, b := first.Scratch(), second.Scratch()

	// Assert
	if a != b {
		t.Fatalf("two sandbox values of one test answered %q and %q, want one scratch", a, b)
	}
}

func TestTwoTestsHaveTwoScratches(t *testing.T) {
	// Arrange
	var dirs [2]string
	for i, name := range []string{"one", "two"} {
		t.Run(name, func(t *testing.T) {
			// Act
			dirs[i] = (&localSandbox{t: t}).Scratch()
		})
	}

	// Assert
	if dirs[0] == dirs[1] {
		t.Fatalf("two tests shared the scratch %q", dirs[0])
	}
}
