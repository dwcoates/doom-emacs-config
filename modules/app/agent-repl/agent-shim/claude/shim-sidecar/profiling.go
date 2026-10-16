package main

import (
	"fmt"

	"agentrepl/logging/profiling"
	"agentrepl/shim-claude-sidecar/internal/logging"
)

// startRuntimeProfiling arms the sidecar's RUNTIME profiling: bin/profile-agent-repl.sh
// opens a session and signals SIGWINCH, and the sidecar starts (or stops) a CPU
// profile into it and writes its heap profile beside it on the stop
// (agentrepl/logging/profiling, Arm). The returned stop ends any running
// profile and waits for the watcher to leave.
func startRuntimeProfiling(logf *logging.Bound, getenv func(string) string, home func() (string, error)) (func(), error) {
	stop, err := profiling.Arm("sidecar", logf.LevelWindow(), func(e profiling.Event) { recordProfilingEvent(logf, e) }, getenv, home)
	if err != nil {
		return nil, fmt.Errorf("shim-claude-sidecar: %w", err)
	}
	return stop, nil
}

// recordProfilingEvent records one profiling event as profiling.Describe words
// it.
func recordProfilingEvent(logf *logging.Bound, e profiling.Event) {
	d := profiling.Describe("sidecar", e)
	logf.With(logging.Context{Operation: d.Operation, Level: d.Level}).Log("%s", d.Text())
}
