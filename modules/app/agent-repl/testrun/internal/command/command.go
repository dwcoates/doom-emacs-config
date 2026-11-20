// Package command runs the helper commands testrun itself needs, never a
// unit (units run through run.OSExec): those of planning and reporting
// (`go list`, `vitest list`, a harness's --list, `go tool cover`) through
// Output, a command whose exit status is an answer through Verdict, and every command held to a bound (the RAM disk's disk-manager
// probes, the reap's `ps`) through Bounded and Outcome.
package command

import (
	"bytes"
	"errors"
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

// Verdict runs cmd and returns its stdout, its stderr and whether it exited
// zero, for a command whose non-zero exit is an answer to read from its
// output rather than a failure (a `go test` or `go vet` over packages, one of
// which did not compile or pass). Only a command that could not run at all
// is an error. cmd.Stdout and cmd.Stderr must be unset: Verdict owns them.
func Verdict(cmd *exec.Cmd) (stdout, stderr []byte, exitedZero bool, err error) {
	if cmd.Stdout != nil || cmd.Stderr != nil {
		panic(fmt.Sprintf("command: Verdict owns the output of %v", cmd.Args))
	}
	var out, errOut bytes.Buffer
	cmd.Stdout, cmd.Stderr = &out, &errOut
	err = cmd.Run()
	var exit *exec.ExitError
	if errors.As(err, &exit) {
		return out.Bytes(), errOut.Bytes(), false, nil
	}
	if err != nil {
		return nil, nil, false, fmt.Errorf("%s (in %s): %w", strings.Join(cmd.Args, " "), cmd.Dir, err)
	}
	return out.Bytes(), errOut.Bytes(), true, nil
}
