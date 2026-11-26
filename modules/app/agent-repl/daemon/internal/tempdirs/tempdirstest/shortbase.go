package tempdirstest

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// EnvShortBase is the scheduled test run's RAM disk mount (testrun's ramdisk
// package exports it to every unit). When it is set, the short roots this
// package and the integration harness make go there instead of /tmp, so their
// files never reach the SSD.
const EnvShortBase = "AGENT_REPL_TEST_SHORT_BASE"

// DefaultShortBase is where the short roots go outside a run on a RAM disk.
const DefaultShortBase = "/tmp"

// ShortBase answers the directory short test roots are made in.
//
// A set value must be an existing directory beneath /tmp: beneath /tmp so
// every root made there is still a temporary directory to the registration
// guard (tempdirs.FixedRoots) and still short enough for a unix socket path.
// Anything else is an error, never a silent fall back to /tmp.
func ShortBase(getenv func(string) string) (string, error) {
	base := getenv(EnvShortBase)
	if base == "" {
		return DefaultShortBase, nil
	}
	if filepath.Clean(base) != base || !strings.HasPrefix(base, "/tmp/") {
		return "", fmt.Errorf("tempdirstest: %s=%q is not a clean directory beneath /tmp", EnvShortBase, base)
	}
	info, err := os.Stat(base)
	if err != nil {
		return "", fmt.Errorf("tempdirstest: %s=%q: %w", EnvShortBase, base, err)
	}
	if !info.IsDir() {
		return "", fmt.Errorf("tempdirstest: %s=%q is not a directory", EnvShortBase, base)
	}
	return base, nil
}

// ShortDir makes a temporary directory whose path is short enough for a unix
// socket (macOS caps sun_path at 104 bytes and a test's own t.TempDir is
// nowhere near short enough), in the run's short base (ShortBase), and removes
// it when the test ends. PREFIX names it for a reader of a leftover.
func ShortDir(t testing.TB, prefix string) string {
	t.Helper()
	base, err := ShortBase(os.Getenv)
	if err != nil {
		t.Fatalf("tempdirstest: the short base: %v", err)
	}
	dir, err := os.MkdirTemp(base, prefix)
	if err != nil {
		t.Fatalf("tempdirstest: make a short directory: %v", err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(dir) })
	return dir
}
