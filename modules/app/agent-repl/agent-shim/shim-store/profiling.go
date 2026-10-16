package main

import (
	"fmt"

	"agentrepl/logging/profiling"
	"agentrepl/shim-store/internal/logging"
)

// startRuntimeProfiling arms the store's RUNTIME profiling: bin/profile-agent-repl.sh
// opens a session and signals SIGWINCH, and the store starts (or stops) a CPU
// profile into it and writes its heap profile beside it on the stop
// (agentrepl/logging/profiling, Arm). The returned stop ends any running
// profile and waits for the watcher to leave.
func startRuntimeProfiling(log *logging.Logger, getenv func(string) string, home func() (string, error)) (func(), error) {
	stop, err := profiling.Arm("store", log.LevelWindow(), func(e profiling.Event) { recordProfilingEvent(log, e) }, getenv, home)
	if err != nil {
		return nil, fmt.Errorf("shim-store: %w", err)
	}
	return stop, nil
}

// recordProfilingEvent records one profiling event as profiling.Describe words
// it, under the store's namespace.
func recordProfilingEvent(log *logging.Logger, e profiling.Event) {
	d := profiling.Describe("store", e)
	log.Log(logging.Fields{Operation: "store." + d.Operation, Level: d.Level}, "%s", d.Text())
}
