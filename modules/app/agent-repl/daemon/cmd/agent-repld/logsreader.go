package main

import (
	"context"
	"fmt"
	"path/filepath"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/merge"
	"agent-repld/internal/scriptrunner"
)

// logsReadBound bounds one read of every system's warnings and errors over a
// deploy's window: bin/logs.sh --all --level warn, which opens only each
// sink's warnings file (proto/vocab/log-warnings-sink.json). Measured
// 2026-11-17 over 1,404 log files (10 GB) for a 20-minute window: 0.86s cold,
// 0.52s warm, before any warnings file existed, so the read was the reader's
// discovery of every sink. 3s is three and a half times the worse, headroom
// for the warnings files the window adds. (Before the warnings files the same
// read filtered every record ever written: 7s measured on 2026-11-15, and it
// outlived an 18s bound on 2026-11-17, holding the merge queue.) Measured
// again 2026-11-24 over 11,830 log files (6.2 GB) for a 20-minute window:
// 0.96-0.99s warm idle. The read runs during the handover, whose load is
// heavier than an idle host's: every read records its duration (operation
// daemon.merge.logs_read), and the bound is to be resized from those records.
const logsReadBound = 3 * time.Second

// logsReadMeasured is the measurement logsReadBound was sized from, named in
// the failure a read past the bound records.
const logsReadMeasured = 990 * time.Millisecond

// opLogsRead is the operation of the record every window read leaves.
const opLogsRead = "daemon.merge.logs_read"

// lineRunner runs a script and answers its output (scriptrunner.Runner).
type lineRunner interface {
	RunLinesWith(ctx context.Context, dir string, argv []string, opts scriptrunner.Options, onLine func(string)) (scriptrunner.Run, error)
}

// logsReader is the deploy intake's log reader (merge.Deps.ReadLogs): agent-
// repl's own bin/logs.sh, under MODULEROOT, read as JSONL over the span. A
// read that fails, times out or prints no records is an error: the intake's
// facts are never a guess.
func logsReader(runner lineRunner, moduleRoot string, log dlog.Logger) merge.LogReader {
	script := filepath.Join(moduleRoot, "bin", "logs.sh")
	return func(ctx context.Context, repoDir string, since, until time.Time) ([]merge.LogRecord, error) {
		argv := []string{script, "--all", "--json", "--level", "warn",
			"--since", since.UTC().Format(time.RFC3339Nano), "--until", until.UTC().Format(time.RFC3339Nano)}
		run, err := runner.RunLinesWith(ctx, repoDir, argv, scriptrunner.Options{Timeout: logsReadBound}, func(string) {})
		if err != nil {
			return nil, fmt.Errorf("agent-repld: run %s: %w", script, err)
		}
		fields := dlog.Context{
			"duration_ms": run.Wall.Milliseconds(),
			"bound_ms":    logsReadBound.Milliseconds(),
			"measured_ms": logsReadMeasured.Milliseconds(),
			"since":       since.UTC().Format(time.RFC3339Nano),
			"until":       until.UTC().Format(time.RFC3339Nano),
		}
		if run.TimedOut {
			fields["outcome"] = "timed_out"
			log.Error(opLogsRead, "the deploy's window read outlived its bound", fields)
			return nil, fmt.Errorf("agent-repld: %s outlived its bound of %s (measured at %s)", script, logsReadBound, logsReadMeasured)
		}
		if run.ExitCode != 0 {
			return nil, fmt.Errorf("agent-repld: %s exited %d: %s", script, run.ExitCode, run.Output)
		}
		records, err := merge.ParseLogLines(run.Output)
		if err != nil {
			return nil, err
		}
		fields["records"] = len(records)
		log.Info(opLogsRead, "read the deploy's window", fields)
		return records, nil
	}
}
