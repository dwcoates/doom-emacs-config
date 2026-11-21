package profiling

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"runtime/pprof"
	"sync"
)

// ExitProfileEnvironment names a directory a runtime profiles its CPU into
// for its WHOLE LIFE: set at its start, it starts a CPU profile at once and
// writes it as <runtime>-<pid>.cpu.pprof when the runtime stops. A profiling
// test run (testrun.v1.TestRunRequest.profile) sets it on every production
// process its harnesses start, so a test's profile includes the daemon,
// store, sidecar and shims it ran. Unset, nothing happens.
//
// A runtime profiling for its whole life is not armed for a session: Go
// holds one CPU profile per process, and the whole-life one already covers
// any window a session would.
const ExitProfileEnvironment = "AGENT_REPL_EXIT_PROFILE_DIR"

// StartExitProfile starts RUNTIME's whole-life CPU profile into DIR, which it
// creates, recording ExitStarted through EVENTS. The returned stop ends the
// profile and writes it, recording ExitStopped, or Failed when the file could
// not be finished; it is idempotent. A profile that cannot start is recorded
// as Failed and answered as an error.
func StartExitProfile(runtime, dir string, events func(Event)) (func(), error) {
	path := filepath.Join(dir, fmt.Sprintf("%s-%d.cpu.pprof", runtime, os.Getpid()))
	f, err := createExitProfile(dir, path)
	if err == nil {
		if startErr := pprof.StartCPUProfile(f); startErr != nil {
			err = errors.Join(fmt.Errorf("profiling: start the whole-life cpu profile: %w", startErr), f.Close())
		}
	}
	if err != nil {
		events(Event{Kind: Failed, Reason: "exit_profile", Files: []string{path}, Err: err})
		return nil, err
	}
	events(Event{Kind: ExitStarted, Files: []string{path}})
	var once sync.Once
	return func() {
		once.Do(func() {
			pprof.StopCPUProfile()
			if err := f.Close(); err != nil {
				events(Event{Kind: Failed, Reason: "exit_profile", Files: []string{path}, Err: fmt.Errorf("profiling: close the whole-life cpu profile: %w", err)})
				return
			}
			events(Event{Kind: ExitStopped, Reason: "exit", Files: []string{path}})
		})
	}, nil
}

// createExitProfile makes DIR and opens PATH in it.
func createExitProfile(dir, path string) (*os.File, error) {
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return nil, fmt.Errorf("profiling: create the whole-life profile's directory: %w", err)
	}
	f, err := os.Create(path)
	if err != nil {
		return nil, fmt.Errorf("profiling: create the whole-life cpu profile: %w", err)
	}
	return f, nil
}
