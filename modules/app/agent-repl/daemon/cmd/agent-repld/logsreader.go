package main

import (
	"context"
	"fmt"
	"path/filepath"
	"time"

	"agent-repld/internal/merge"
	"agent-repld/internal/scriptrunner"
)

// logsReadBound bounds one read of every system's warnings and errors over a
// deploy's window: bin/logs.sh --all over the current files and their five
// rotations, measured at 4s on the owner's machine with its compiled reader
// cached (2026-11-14, a 40-minute window); 10s is that with headroom for a
// cold reader build.
const logsReadBound = 10 * time.Second

// lineRunner runs a script and answers its output (scriptrunner.Runner).
type lineRunner interface {
	RunLinesWith(ctx context.Context, dir string, argv []string, opts scriptrunner.Options, onLine func(string)) (scriptrunner.Run, error)
}

// logsReader is the deploy intake's log reader (merge.Deps.ReadLogs): agent-
// repl's own bin/logs.sh, under MODULEROOT, read as JSONL over the span. A
// read that fails, times out or prints no records is an error: the intake's
// facts are never a guess.
func logsReader(runner lineRunner, moduleRoot string) merge.LogReader {
	script := filepath.Join(moduleRoot, "bin", "logs.sh")
	return func(ctx context.Context, repoDir string, since, until time.Time) ([]merge.LogRecord, error) {
		argv := []string{script, "--all", "--json", "--level", "warn",
			"--since", since.UTC().Format(time.RFC3339Nano), "--until", until.UTC().Format(time.RFC3339Nano)}
		run, err := runner.RunLinesWith(ctx, repoDir, argv, scriptrunner.Options{Timeout: logsReadBound}, func(string) {})
		if err != nil {
			return nil, fmt.Errorf("agent-repld: run %s: %w", script, err)
		}
		if run.TimedOut {
			return nil, fmt.Errorf("agent-repld: %s outlived its bound of %s (measured at 4s)", script, logsReadBound)
		}
		if run.ExitCode != 0 {
			return nil, fmt.Errorf("agent-repld: %s exited %d: %s", script, run.ExitCode, run.Output)
		}
		return merge.ParseLogLines(run.Output)
	}
}
