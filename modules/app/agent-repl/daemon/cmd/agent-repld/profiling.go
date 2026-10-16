package main

import (
	"fmt"

	"agent-repld/internal/dlog"
	"agentrepl/logging/profiling"
)

// startRuntimeProfiling arms the daemon's RUNTIME profiling: bin/profile-agent-repl.sh
// opens a session and signals SIGWINCH, and the daemon starts (or stops) a CPU
// profile into it and writes its heap profile beside it on the stop
// (agentrepl/logging/profiling, Arm). The returned stop ends any running
// profile and waits for the watcher to leave.
func startRuntimeProfiling(log dlog.Logger, level profiling.LevelWindow, getenv func(string) string, home func() (string, error)) (func(), error) {
	stop, err := profiling.Arm("daemon", level, func(e profiling.Event) { recordProfilingEvent(log, e) }, getenv, home)
	if err != nil {
		return nil, fmt.Errorf("agent-repld: %w", err)
	}
	return stop, nil
}

// recordProfilingEvent records one profiling event as profiling.Describe words
// it, under the daemon's namespace.
func recordProfilingEvent(log dlog.Logger, e profiling.Event) {
	d := profiling.Describe("daemon", e)
	if d.Level == dlog.LevelError {
		log.Error("daemon."+d.Operation, d.Message, dlog.Context(d.Context()))
		return
	}
	log.Info("daemon."+d.Operation, d.Message, dlog.Context(d.Context()))
}
