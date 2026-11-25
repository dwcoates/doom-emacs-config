package ctxend

import (
	"context"
	"errors"
	"fmt"
	"testing"

	"agent-repld/internal/dlog"
	"agentrepl/logging/logsite"
)

func TestIsNamesOnlyAContextEnding(t *testing.T) {
	tests := []struct {
		name string
		err  error
		want bool
	}{
		{name: "cancelled", err: context.Canceled, want: true},
		{name: "deadline exceeded", err: context.DeadlineExceeded, want: true},
		{name: "wrapped cancellation", err: fmt.Errorf("wsm: commit transaction: %w", context.Canceled), want: true},
		{name: "wrapped deadline", err: fmt.Errorf("outer: %w", fmt.Errorf("inner: %w", context.DeadlineExceeded)), want: true},
		{name: "a cancel cause's context error", err: func() error {
			ctx, cancel := context.WithCancelCause(context.Background())
			cancel(errors.New("the daemon is exiting"))
			return ctx.Err()
		}(), want: true},
		{name: "a failure", err: errors.New("database is locked"), want: false},
		{name: "no error", err: nil, want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange in the table. Act.
			got := Is(tt.err)
			// Assert.
			if got != tt.want {
				t.Fatalf("Is(%v) = %v, want %v", tt.err, got, tt.want)
			}
		})
	}
}

func TestLogSplitsByWhetherTheContextEnded(t *testing.T) {
	tests := []struct {
		name         string
		err          error
		cutLevel     string
		wantLevel    string
		wantTemplate string
	}{
		{name: "cancelled", err: context.Canceled, cutLevel: dlog.LevelInfo, wantLevel: dlog.LevelInfo, wantTemplate: "the call was cut"},
		{name: "a debug cut", err: context.Canceled, cutLevel: dlog.LevelDebug, wantLevel: dlog.LevelDebug, wantTemplate: "the call was cut"},
		{name: "wrapped deadline", err: fmt.Errorf("read: %w", context.DeadlineExceeded), cutLevel: dlog.LevelInfo, wantLevel: dlog.LevelInfo, wantTemplate: "the call was cut"},
		{name: "a failure", err: errors.New("database is locked"), cutLevel: dlog.LevelInfo, wantLevel: dlog.LevelError, wantTemplate: "the call failed"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			log := dlog.NewTestLogger()

			// Act.
			Log(tt.err, log, tt.cutLevel, "daemon.test", logsite.Msg("the call was cut"), logsite.Msg("the call failed"), dlog.Context{"k": "v"})

			// Assert.
			records := log.Records()
			if len(records) != 1 {
				t.Fatalf("records = %v, want one", records)
			}
			got := records[0]
			wantSite := logsite.Key(dlog.RuntimeDaemon, "agent-repld/internal/ctxend.TestLogSplitsByWhetherTheContextEnded", tt.wantTemplate)
			if got.Level != tt.wantLevel || got.Template != tt.wantTemplate || got.Operation != "daemon.test" || got.Context["k"] != "v" || got.Site != wantSite {
				t.Fatalf("recorded %+v, want %s %q with the fields, site %s", got, tt.wantLevel, tt.wantTemplate, wantSite)
			}
		})
	}
}
