package suites

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
)

// PrepareWebappDist builds the module's webapp dist when it is stale, through
// the one implementation of the staleness rule (e2e/sandbox/bin/webapp-dist.sh
// ensure). A run calls it once, before any unit starts, and hands every suite
// that reads the dist NoWebappBuildEnv, so the run is the dist's one writer.
func PrepareWebappDist(module string) error {
	ensure := exec.Command(filepath.Join(module, "e2e", "sandbox", "bin", "webapp-dist.sh"), "ensure")
	ensure.Env = os.Environ()
	if out, err := ensure.CombinedOutput(); err != nil {
		return fmt.Errorf("suites: build the webapp dist of %s: %w\n%s", module, err, out)
	}
	return nil
}
