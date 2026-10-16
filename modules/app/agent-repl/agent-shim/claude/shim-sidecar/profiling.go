package main

import (
	"fmt"
	"strings"

	"agentrepl/logging/profiling"
	"agentrepl/shim-claude-sidecar/internal/logging"
)

// startRuntimeProfiling arms the sidecar's RUNTIME profiling: bin/profile-agent-repl.sh
// opens a session and signals SIGWINCH, and the sidecar starts (or stops) a CPU
// profile into it and writes its heap profile beside it on the stop
// (agentrepl/logging/profiling). Off, it costs one parked goroutine. The
// returned stop ends any running profile and waits for the watcher to leave.
func startRuntimeProfiling(logf *logging.Bound, getenv func(string) string, home func() (string, error)) (func(), error) {
	dir, err := home()
	if err != nil && getenv(profiling.RootEnvironment) == "" {
		return nil, fmt.Errorf("shim-claude-sidecar: the profiles root needs a home directory: %w", err)
	}
	root, err := profiling.Root(getenv, dir)
	if err != nil {
		return nil, fmt.Errorf("shim-claude-sidecar: resolve the profiles root: %w", err)
	}
	p, err := profiling.New(profiling.Options{
		Runtime: "sidecar",
		Root:    root,
		Events:  func(e profiling.Event) { recordProfilingEvent(logf, e) },
		Level:   logf.LevelWindow(),
	})
	if err != nil {
		return nil, fmt.Errorf("shim-claude-sidecar: build the runtime profiler: %w", err)
	}
	done := make(chan struct{})
	left := make(chan struct{})
	go func() {
		defer close(left)
		profiling.Watch(p, done)
	}()
	return func() { close(done); <-left }, nil
}

// recordProfilingEvent records one profiling event: a start, a stop, or a
// session's log level window opening or closing at info (lifecycle
// transitions), a failure at error.
func recordProfilingEvent(logf *logging.Bound, e profiling.Event) {
	files := strings.Join(e.Files, ",")
	switch e.Kind {
	case profiling.Started:
		logf.With(logging.Context{Operation: "profiling.started", Level: "info"}).Log(
			"runtime profiling started into the open session session=%s files=%s", e.Session, files)
	case profiling.Stopped:
		logf.With(logging.Context{Operation: "profiling.stopped", Level: "info"}).Log(
			"runtime profiling stopped and wrote its profiles session=%s reason=%s files=%s", e.Session, e.Reason, files)
	case profiling.LevelOpened:
		logf.With(logging.Context{Operation: "profiling.level_opened", Level: "info"}).Log(
			"the profiling session raised the sidecar's log level for its window session=%s level=%s until=%d", e.Session, e.Level, e.Until.Unix())
	case profiling.LevelClosed:
		logf.With(logging.Context{Operation: "profiling.level_closed", Level: "info"}).Log(
			"the profiling session's log level window closed; the sidecar is back at info session=%s level=%s reason=%s", e.Session, e.Level, e.Reason)
	default:
		logf.With(logging.Context{Operation: "profiling.failed", Level: "error"}).Log(
			"runtime profiling could not do what the session asked session=%s reason=%s cause=%v", e.Session, e.Reason, e.Err)
	}
}
