package main

import (
	"context"
	"errors"
	"slices"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/scriptrunner"
)

// fakeLines answers one scripted run and records what it was asked.
type fakeLines struct {
	run  scriptrunner.Run
	err  error
	dir  string
	argv []string
	opts scriptrunner.Options
}

func (f *fakeLines) RunLinesWith(_ context.Context, dir string, argv []string, opts scriptrunner.Options, _ func(string)) (scriptrunner.Run, error) {
	f.dir, f.argv, f.opts = dir, argv, opts
	return f.run, f.err
}

func TestTheLogsReaderReadsEveryWarningAndErrorOfTheSpan(t *testing.T) {
	// Arrange
	since := time.Date(2026, 11, 14, 12, 0, 0, 0, time.UTC)
	runner := &fakeLines{run: scriptrunner.Run{Output: "{\"level\":\"warn\",\"operation\":\"a\",\"message\":\"m\"}\nlogs: one sink unreadable\n"}}

	// Act
	got, err := logsReader(runner, "/module", dlog.NewTestLogger())(context.Background(), "/repo", since, since.Add(time.Minute))

	// Assert
	if err != nil || len(got) != 1 || got[0].Operation != "a" {
		t.Fatalf("read = (%+v, %v), want the one record", got, err)
	}
	wantArgv := []string{"/module/bin/logs.sh", "--all", "--json", "--level", "warn", "--since", "2026-11-14T12:00:00Z", "--until", "2026-11-14T12:01:00Z"}
	if runner.dir != "/repo" || !slices.Equal(runner.argv, wantArgv) || runner.opts.Timeout != logsReadBound {
		t.Fatalf("ran %v in %s bounded %v, want %v in /repo bounded %v", runner.argv, runner.dir, runner.opts.Timeout, wantArgv, logsReadBound)
	}
}

func TestTheLogsReaderFailsLoudly(t *testing.T) {
	tests := []struct {
		name   string
		runner *fakeLines
		want   string
	}{
		{name: "a reader that cannot run", runner: &fakeLines{err: errors.New("no such file")}, want: "no such file"},
		{name: "a reader that outlives its bound", runner: &fakeLines{run: scriptrunner.Run{TimedOut: true}}, want: "outlived its bound of 3s (measured at 990ms)"},
		{name: "a reader that exits non-zero", runner: &fakeLines{run: scriptrunner.Run{ExitCode: 2, Output: "logs: no sink"}}, want: "exited 2"},
		{name: "a record that does not decode", runner: &fakeLines{run: scriptrunner.Run{Output: "{\"level\":"}}, want: "no record"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			_, err := logsReader(tt.runner, "/module", dlog.NewTestLogger())(context.Background(), "/repo", time.Now(), time.Now())

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("read = %v, want an error saying %q", err, tt.want)
			}
		})
	}
}

func TestTheLogsReaderRecordsEveryReadsOutcome(t *testing.T) {
	since := time.Date(2026, 11, 25, 1, 30, 0, 0, time.UTC)
	until := since.Add(20 * time.Minute)
	tests := []struct {
		name      string
		run       scriptrunner.Run
		wantErr   bool
		wantLevel string
		want      dlog.Context
	}{
		{
			name:      "a read that succeeds records its duration and record count",
			run:       scriptrunner.Run{Wall: 1234 * time.Millisecond, Output: "{\"level\":\"warn\",\"operation\":\"a\",\"message\":\"m\"}\n"},
			wantLevel: "INFO",
			want:      dlog.Context{"duration_ms": int64(1234), "records": 1},
		},
		{
			name:      "a read that outlives its bound records the bound and measurement",
			run:       scriptrunner.Run{Wall: 3 * time.Second, TimedOut: true},
			wantErr:   true,
			wantLevel: "ERROR",
			want:      dlog.Context{"duration_ms": int64(3000), "outcome": "timed_out"},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			log := dlog.NewTestLogger()

			// Act
			_, err := logsReader(&fakeLines{run: tt.run}, "/module", log)(context.Background(), "/repo", since, until)

			// Assert
			if (err != nil) != tt.wantErr {
				t.Fatalf("read error = %v, want error %t", err, tt.wantErr)
			}
			rec, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Operation: "daemon.merge.logs_read"})
			if !ok || len(log.Records()) != 1 {
				t.Fatalf("records = %+v, want exactly one daemon.merge.logs_read", log.Records())
			}
			if !strings.EqualFold(rec.Level, tt.wantLevel) {
				t.Fatalf("level = %q, want %q", rec.Level, tt.wantLevel)
			}
			want := dlog.Context{
				"bound_ms": int64(3000), "measured_ms": int64(990),
				"since": "2026-11-25T01:30:00Z", "until": "2026-11-25T01:50:00Z",
			}
			for k, v := range tt.want {
				want[k] = v
			}
			for k, v := range want {
				if rec.Context[k] != v {
					t.Errorf("context[%q] = %v, want %v", k, rec.Context[k], v)
				}
			}
		})
	}
}

func TestTheLogsReaderRecordsNothingForANonZeroExit(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()

	// Act
	_, err := logsReader(&fakeLines{run: scriptrunner.Run{ExitCode: 2, Output: "x"}}, "/module", log)(context.Background(), "/repo", time.Now(), time.Now())

	// Assert
	if err == nil || len(log.Records()) != 0 {
		t.Fatalf("read = %v with records %+v, want the exit error and no record", err, log.Records())
	}
}
