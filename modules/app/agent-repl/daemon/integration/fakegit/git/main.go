// Command git is the integration suite's scripted `git`. It is built once per
// test run and placed FIRST on the daemon's PATH, so every git invocation the
// daemon makes is answered from the fixture file named by FAKEGIT_STATE. No
// real git binary is ever reached, and no repository is ever initialized.
//
// A command with no fixture exits 128 loudly rather than succeeding silently,
// so a test can never pass on a git conversation nobody intended.
package main

import (
	"fmt"
	"os"

	"claude-repld/integration/fakegit"
)

func main() {
	statePath := os.Getenv(fakegit.EnvStateFile)
	if statePath == "" {
		fmt.Fprintf(os.Stderr, "fatal: fakegit: %s is unset; the harness must point every git at its fixture file\n", fakegit.EnvStateFile)
		os.Exit(128)
	}
	cwd, err := os.Getwd()
	if err != nil {
		fmt.Fprintf(os.Stderr, "fatal: fakegit: working directory: %v\n", err)
		os.Exit(128)
	}

	var result fakegit.Result
	if err := fakegit.WithLock(statePath, func(s *fakegit.State) error {
		result = fakegit.Run(s, cwd, os.Args[1:])
		return nil
	}); err != nil {
		fmt.Fprintf(os.Stderr, "fatal: %v\n", err)
		os.Exit(128)
	}

	os.Stdout.WriteString(result.Stdout)
	os.Stderr.WriteString(result.Stderr)
	os.Exit(result.Exit)
}
