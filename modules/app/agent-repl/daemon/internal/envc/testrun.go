package envc

import (
	"fmt"
	"path/filepath"
	"strings"
)

// RequireTestRun answers nil on a daemon running under EnvForbidVendorCalls,
// and the one boot refusal of a test-run seam otherwise.
//
// EVERY TEST-RUN SEAM IS HONORED ONLY UNDER THE VENDOR GUARD: it is what
// every test run sets and what no daemon serving a person runs under, so a
// seam a live daemon inherited is refused at boot rather than honored.
// SETTING is the seam as the environment spells it (a name, or name=value),
// and CONSEQUENCE, when not empty, says what a live daemon does instead.
func (c Contracts) RequireTestRun(setting, consequence string) error {
	if c.forbidVendorCalls {
		return nil
	}
	if consequence != "" {
		consequence = "; " + consequence
	}
	return fmt.Errorf("%s is a test-run seam and is honored only with %s set%s", setting, EnvForbidVendorCalls, consequence)
}

// RequireTestMetricsRoot refuses a test run's metrics root outside the test
// run's own temporary directory, tempDir (os.TempDir under the run). A
// daemon serving a person, or one with no metrics root, is never refused.
//
// A TEST RUN NEVER WRITES THE LIVE METRICS STORE. A test daemon handed the
// live root advertises itself in the live targets directory, so the store
// scrapes it in place of the daemon a person runs, and its last values are
// pushed into the live history under runs nobody can tell from real ones
// (2026-10-20: dozens of e2e daemons inside one half hour).
func (c Contracts) RequireTestMetricsRoot(root, tempDir string) error {
	if !c.forbidVendorCalls || root == "" {
		return nil
	}
	rel, err := filepath.Rel(filepath.Clean(tempDir), filepath.Clean(root))
	if err == nil && rel != ".." && !strings.HasPrefix(rel, ".."+string(filepath.Separator)) {
		return nil
	}
	return fmt.Errorf("a test run's metrics root %q lies outside its temporary directory %q (%s is set); a test run never writes the live metrics store", root, tempDir, EnvForbidVendorCalls)
}
