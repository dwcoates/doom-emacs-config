// Package tempdirstest gives a test binary ONE exempt temporary root for the
// registry's temporary-directory guard (tempdirs).
//
// A package whose tests register directories in a real registry runs its tests
// through Main: TMPDIR is pointed at a short directory made under /tmp, so
// every t.TempDir and os.MkdirTemp("") of the run lands inside it, and Guard answers the production roots exempting that directory
// and nothing else. Every other temporary folder is still refused.
//
// It is a test package: nothing the daemon links imports it, so no running
// daemon can reach the exemption through it.
package tempdirstest

import (
	"errors"
	"fmt"
	"os"
	"testing"

	"claude-repld/internal/tempdirs"
)

// guard is the run's guard, built by Main before any test runs and only read
// thereafter.
var (
	guard tempdirs.Guard
	built bool
)

// Main runs m with TMPDIR pointed at the run's exempt root, and answers the
// exit code. A root it cannot set up is a harness failure and panics. Call
// it from TestMain: `os.Exit(tempdirstest.Main(m))`.
func Main(m *testing.M) int {
	real := os.TempDir()
	// THE ROOT IS UNDER /tmp AND SHORT, as the integration harness's run root
	// is: tests bind unix sockets beneath their temporary directories, and a
	// root nested inside macOS's long per-user $TMPDIR pushed those paths past
	// the 104-byte sun_path limit.
	root, err := os.MkdirTemp("/tmp", "arunit")
	if err != nil {
		panic(fmt.Errorf("tempdirstest: make the exempt temporary root: %w", err))
	}
	defer os.RemoveAll(root)
	g, err := tempdirs.New(real, root)
	if err != nil {
		panic(fmt.Errorf("tempdirstest: build the guard: %w", err))
	}
	if err := os.Setenv("TMPDIR", root); err != nil {
		panic(fmt.Errorf("tempdirstest: point TMPDIR at %s: %w", root, err))
	}
	guard, built = g, true
	return m.Run()
}

// RunGuard answers the run's guard, or an error when the test binary did not
// run through Main.
func RunGuard() (tempdirs.Guard, error) {
	if !built {
		return tempdirs.Guard{}, errors.New("tempdirstest: no guard; this package's TestMain must run tempdirstest.Main")
	}
	return guard, nil
}

// Guard answers the run's guard, failing t when there is none: a test binary
// that did not run through Main is a harness bug.
func Guard(t testing.TB) tempdirs.Guard {
	t.Helper()
	g, err := RunGuard()
	if err != nil {
		t.Fatal(err)
	}
	return g
}
