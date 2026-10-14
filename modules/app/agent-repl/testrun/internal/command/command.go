// Package command runs the helper commands testrun itself needs, never a
// unit (units run through run.OSExec): those of planning and reporting
// (`go list`, `vitest list`, a harness's --list, `go tool cover`) through
// Output, and every command held to a bound (the RAM disk's disk-manager
// probes, the reap's `ps`) through Bounded and Outcome.
package command

import (
	"bytes"
	"fmt"
	"os/exec"
	"strings"
)

// Output runs cmd and returns its stdout. A failure names the command line
// and its directory and carries everything the command wrote to stderr, so
// the run log alone says why it failed. cmd.Stderr must be unset: Output
// owns it.
func Output(cmd *exec.Cmd) ([]byte, error) {
	if cmd.Stderr != nil {
		panic(fmt.Sprintf("command: Output owns the stderr of %v", cmd.Args))
	}
	var stderr bytes.Buffer
	cmd.Stderr = &stderr
	out, err := cmd.Output()
	if err != nil {
		return nil, fmt.Errorf("%s (in %s): %w\n%s", strings.Join(cmd.Args, " "), cmd.Dir, err, stderr.String())
	}
	return out, nil
}
