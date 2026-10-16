package main

import (
	"fmt"

	"agent-repld/internal/dlog"
	"agentrepl/logging/profiling"
)

// startRuntimeProfiling arms the daemon's RUNTIME profiling: bin/profile-agent-repl.sh
// opens a session and signals SIGWINCH, and the daemon starts (or stops) a CPU
// profile into it and writes its heap profile beside it on the stop
// (agentrepl/logging/profiling). Off, it costs one parked goroutine. The
// returned stop ends any running profile and waits for the watcher to leave.
func startRuntimeProfiling(log dlog.Logger, level profiling.LevelWindow, getenv func(string) string, home func() (string, error)) (func(), error) {
	dir, err := home()
	if err != nil && getenv(profiling.RootEnvironment) == "" {
		return nil, fmt.Errorf("agent-repld: the profiles root needs a home directory: %w", err)
	}
	root, err := profiling.Root(getenv, dir)
	if err != nil {
		return nil, fmt.Errorf("agent-repld: resolve the profiles root: %w", err)
	}
	p, err := profiling.New(profiling.Options{
		Runtime: "daemon",
		Root:    root,
		Events:  func(e profiling.Event) { recordProfilingEvent(log, e) },
		Level:   level,
	})
	if err != nil {
		return nil, fmt.Errorf("agent-repld: build the runtime profiler: %w", err)
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
func recordProfilingEvent(log dlog.Logger, e profiling.Event) {
	ctx := dlog.Context{"session": e.Session, "files": e.Files}
	if e.Reason != "" {
		ctx["reason"] = e.Reason
	}
	switch e.Kind {
	case profiling.Started:
		log.Info("daemon.profiling.started", "runtime profiling started into the open session", ctx)
	case profiling.Stopped:
		log.Info("daemon.profiling.stopped", "runtime profiling stopped and wrote its profiles", ctx)
	case profiling.LevelOpened:
		ctx["level"], ctx["until"] = e.Level, e.Until.Unix()
		log.Info("daemon.profiling.level_opened", "the profiling session raised the daemon's log level for its window", ctx)
	case profiling.LevelClosed:
		ctx["level"], ctx["until"] = e.Level, e.Until.Unix()
		log.Info("daemon.profiling.level_closed", "the profiling session's log level window closed; the daemon is back at info", ctx)
	default:
		ctx["cause"] = fmt.Sprint(e.Err)
		log.Error("daemon.profiling.failed", "runtime profiling could not do what the session asked", ctx)
	}
}
