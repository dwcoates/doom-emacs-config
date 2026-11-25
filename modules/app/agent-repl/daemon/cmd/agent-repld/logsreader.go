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
// discovery of every sink. (Before the warnings files the same read filtered
// every record ever written: 7s measured on 2026-11-15, and it outlived an 18s
// bound on 2026-11-17, holding the merge queue.) Measured again 2026-11-24
// over 11,830 log files: 0.96-0.99s warm idle, most of it logs.sh building
// five reader arguments per daemon workspace in bash; the then 3s bound timed
// out at 3003ms on 2026-11-25 during a handover.
//
// Since the reader enumerates the workspaces' logs itself, measured
// 2026-11-25 over 11,972 log files and 1,627 daemon workspaces, the window of
// that timeout (51s) read in 0.40-0.50s over ten runs on a host at load
// average 8. The read runs during the handover: the records of
// daemon.merge.logs_read put handover reads (1291-1437ms) at about 1.5 times
// the idle ones (980-1005ms), so the healthy worst case under handover load is
// 0.75s, and the bound is three times that.
const logsReadBound = 2250 * time.Millisecond

// logsReadMeasured is the measurement logsReadBound was sized from, named in
// the failure a read past the bound records: the idle worst case.
const logsReadMeasured = 500 * time.Millisecond

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
