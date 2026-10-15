package suites

import (
	"fmt"
	"os"
	"os/exec"
)

// runEnsure runs a module script that makes something ready before units are
// planned or run (npm deps, the webapp dist), with the caller's environment.
// A failure is WHAT failing, carrying the script's whole combined output.
func runEnsure(what, path string, args ...string) error {
	cmd := exec.Command(path, args...)
	cmd.Env = os.Environ()
	if out, err := cmd.CombinedOutput(); err != nil {
		return fmt.Errorf("suites: %s: %w\n%s", what, err, out)
	}
	return nil
}
