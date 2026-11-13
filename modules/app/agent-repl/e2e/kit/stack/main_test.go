package stack

import (
	"fmt"
	"os"
	"path/filepath"
	"testing"
)

// TestMain runs this package's tests as an e2e suite of their own: the layout
// is the e2e package's, two directories above this one, and a re-exec of this
// binary as a stand-in store (storedump_test.go) runs no test.
func TestMain(m *testing.M) {
	if mode := os.Getenv(envStandInStore); mode != "" {
		os.Exit(runStandInStore(mode))
	}
	here, err := os.Getwd()
	if err != nil {
		fmt.Fprintln(os.Stderr, "stack: the working directory, which is this package's:", err)
		os.Exit(1)
	}
	os.Exit(RunSuite(m, filepath.Clean(filepath.Join(here, "..", "..")), nil, nil))
}
