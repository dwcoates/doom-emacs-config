package harness

import "testing"

// SPARING'S OWN TESTS.
//
// The exemption registry is read by TWO reapers now — this package's
// `Daemon.strayPIDs`, and the Emacs layer's own `findStrays`, which keys on a
// scenario root rather than a state directory. It is exported for exactly that
// reason, so its answers are pinned here rather than only through a caller.

func TestSparedFromStrayReapingAnswersTrueForADeclaredPid(t *testing.T) {
	// Arrange: a pid nothing has declared. 0x7fffffff is never a live process
	// on any host this suite runs on, so nothing else can be declaring it.
	const pid = 0x7ffffffe
	if SparedFromStrayReaping(pid) {
		t.Fatalf("arrange: pid %d is already spared before anything declared it", pid)
	}

	// Act.
	SpareFromStrayReaping(t, pid)

	// Assert.
	if !SparedFromStrayReaping(pid) {
		t.Fatalf("SparedFromStrayReaping(%d) = false after SpareFromStrayReaping declared it", pid)
	}
}

func TestSparedFromStrayReapingAnswersFalseForAnUndeclaredPid(t *testing.T) {
	// Arrange: a pid this test never declares, distinct from the one above so
	// the two cannot pass on each other's declaration.
	const pid = 0x7ffffffd

	// Act.
	spared := SparedFromStrayReaping(pid)

	// Assert: an undeclared pid belongs to the daemon's tree as far as every
	// reaper is concerned, and answering otherwise would leave a real stray
	// running.
	if spared {
		t.Fatalf("SparedFromStrayReaping(%d) = true for a pid nothing declared", pid)
	}
}
