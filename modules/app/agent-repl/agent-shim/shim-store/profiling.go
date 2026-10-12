package main

import (
	"fmt"
	"strings"

	"agentrepl/logging/profiling"
	"agentrepl/shim-store/internal/logging"
)

// startRuntimeProfiling arms the store's RUNTIME profiling: bin/profile-agent-repl.sh
// opens a session and signals SIGUSR2, and the store starts (or stops) a CPU
// profile into it and writes its heap profile beside it on the stop
// (agentrepl/logging/profiling). Off, it costs one parked goroutine. The
// returned stop ends any running profile and waits for the watcher to leave.
func startRuntimeProfiling(log *logging.Logger, getenv func(string) string, home func() (string, error)) (func(), error) {
	dir, err := home()
	if err != nil && getenv(profiling.RootEnvironment) == "" {
		return nil, fmt.Errorf("shim-store: the profiles root needs a home directory: %w", err)
	}
	root, err := profiling.Root(getenv, dir)
	if err != nil {
		return nil, fmt.Errorf("shim-store: resolve the profiles root: %w", err)
	}
	p, err := profiling.New(profiling.Options{
		Runtime: "store",
		Root:    root,
		Events:  func(e profiling.Event) { recordProfilingEvent(log, e) },
	})
	if err != nil {
		return nil, fmt.Errorf("shim-store: build the runtime profiler: %w", err)
	}
	done := make(chan struct{})
	left := make(chan struct{})
	go func() {
		defer close(left)
		profiling.Watch(p, done)
	}()
	return func() { close(done); <-left }, nil
}

// recordProfilingEvent records one profiling event: a start or a stop at
// info (a lifecycle transition), a failure at error.
func recordProfilingEvent(log *logging.Logger, e profiling.Event) {
	files := strings.Join(e.Files, ",")
	switch e.Kind {
	case profiling.Started:
		log.Log(logging.Fields{Operation: "store.profiling.started", Level: "info"},
			"runtime profiling started into the open session session=%s files=%s", e.Session, files)
	case profiling.Stopped:
		log.Log(logging.Fields{Operation: "store.profiling.stopped", Level: "info"},
			"runtime profiling stopped and wrote its profiles session=%s reason=%s files=%s", e.Session, e.Reason, files)
	default:
		log.Log(logging.Fields{Operation: "store.profiling.failed", Level: "error"},
			"runtime profiling could not do what the session asked session=%s reason=%s cause=%v", e.Session, e.Reason, e.Err)
	}
}
