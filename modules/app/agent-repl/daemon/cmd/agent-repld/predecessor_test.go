package main

import (
	"os"
	"path/filepath"
	"testing"

	"agent-repld/internal/daemonaddr"
	"agent-repld/internal/dlog"
)

func TestADeathReapedIsRecordedWithItsPanic(t *testing.T) {
	const (
		panicked = "the daemon before this one died of a panic, leaving its life lock standing"
		killed   = "the daemon before this one died without its orderly exit, leaving its life lock standing, and its stdio log holds no panic of its (killed, or its output lost)"
	)
	tests := []struct {
		name        string
		death       daemonaddr.Death
		stdio       string
		wantPID     int
		wantPanic   bool
		wantMessage string
		wantFields  dlog.Context
		// wantAbsent are the fields the record must not carry.
		wantAbsent []string
	}{
		{name: "a daemon that panicked", death: daemonaddr.Death{Instance: "641fd388dc8e496c", PID: 61885, PIDKnown: true},
			stdio:   `{"message":"x","pid":61885}` + "\npanic: boom\n\ngoroutine 1 [running]:\nagent-repld/internal/x.f(0x1)\n\tagent-repld/internal/x/x.go:9 +0x10\n",
			wantPID: 61885, wantPanic: true, wantMessage: panicked,
			wantFields: dlog.Context{"instance": "641fd388dc8e496c", "pid": 61885, "panic": "panic: boom", "site": "agent-repld/internal/x.f at agent-repld/internal/x/x.go:9"}},
		{name: "a daemon killed without a panic", death: daemonaddr.Death{Instance: "641fd388dc8e496c", PID: 61885, PIDKnown: true},
			stdio:   `{"message":"x","pid":61885}` + "\n",
			wantPID: 61885, wantMessage: killed,
			wantFields: dlog.Context{"instance": "641fd388dc8e496c", "pid": 61885}, wantAbsent: []string{"panic", "site"}},
		{name: "an older build's life lock naming no pid", death: daemonaddr.Death{Instance: "641fd388dc8e496c"},
			stdio:       `{"message":"x","pid":61885}` + "\npanic: boom\n",
			wantMessage: killed,
			wantFields:  dlog.Context{"instance": "641fd388dc8e496c"}, wantAbsent: []string{"pid", "panic", "site"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			dir := t.TempDir()
			if err := os.WriteFile(filepath.Join(dir, dlog.StdioLogName), []byte(tt.stdio), 0o644); err != nil {
				t.Fatal(err)
			}
			log := dlog.NewTestLogger()

			// Act
			d := readPredecessorDeath(dir, tt.death, log)

			// Assert
			if d.PID != tt.wantPID || d.Panicked != tt.wantPanic || d.Instance != tt.death.Instance {
				t.Fatalf("death = %+v; want pid %d, panicked %v, instance %s", d, tt.wantPID, tt.wantPanic, tt.death.Instance)
			}
			rec, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: opPredecessorDied, Message: tt.wantMessage})
			if !ok {
				t.Fatalf("records = %+v; want the death at ERROR saying %q", log.Records(), tt.wantMessage)
			}
			for k, v := range tt.wantFields {
				if rec.Context[k] != v {
					t.Fatalf("record %v: %s = %v, want %v", rec.Context, k, rec.Context[k], v)
				}
			}
			for _, k := range tt.wantAbsent {
				if _, has := rec.Context[k]; has {
					t.Fatalf("record %v carries %s, want none", rec.Context, k)
				}
			}
		})
	}
}

func TestADeathWhoseStdioLogCannotBeReadIsStillRecorded(t *testing.T) {
	// Arrange: a directory where the stdio log belongs.
	dir := t.TempDir()
	if err := os.Mkdir(filepath.Join(dir, dlog.StdioLogName), 0o755); err != nil {
		t.Fatal(err)
	}
	log := dlog.NewTestLogger()

	// Act
	readPredecessorDeath(dir, daemonaddr.Death{Instance: "a", PID: 7, PIDKnown: true}, log)

	// Assert
	rec, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: opPredecessorDied})
	if !ok || rec.Context["stdio_log_error"] == nil || rec.Context["pid"] != 7 {
		t.Fatalf("records %+v; want the death recorded with why its stdio log could not be read", log.Records())
	}
}
