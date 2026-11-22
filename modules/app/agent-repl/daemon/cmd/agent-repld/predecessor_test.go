package main

import (
	"os"
	"path/filepath"
	"testing"

	"agent-repld/internal/daemonaddr"
	"agent-repld/internal/dlog"
)

func TestABootRecordsItsPredecessorsDisorderlyDeath(t *testing.T) {
	tests := []struct {
		name        string
		ad          string
		stdio       string
		wantPID     int
		wantPanic   bool
		wantMessage string
		wantFields  dlog.Context
	}{
		{name: "a predecessor that panicked", ad: "127.0.0.1:59704\npid=61885\n",
			stdio:   `{"message":"x","pid":61885}` + "\npanic: boom\n\ngoroutine 1 [running]:\nagent-repld/internal/x.f(0x1)\n\tagent-repld/internal/x/x.go:9 +0x10\n",
			wantPID: 61885, wantPanic: true, wantMessage: "the daemon before this one died of a panic, leaving its advertisement stale",
			wantFields: dlog.Context{"pid": 61885, "panic": "panic: boom", "site": "agent-repld/internal/x.f at agent-repld/internal/x/x.go:9"}},
		{name: "a predecessor killed without a panic", ad: "127.0.0.1:59704\npid=61885\n", stdio: `{"message":"x","pid":61885}` + "\n",
			wantPID: 61885, wantMessage: "the daemon before this one died without its orderly exit, leaving its advertisement stale, and its stdio log holds no panic of its (killed, or its output lost)",
			wantFields: dlog.Context{"pid": 61885}},
		{name: "a shared address's advertisement names its private address", ad: "127.0.0.1:59704\npid=61885\nprivate=127.0.0.1:59800\n", stdio: `{"message":"x","pid":61885}` + "\n",
			wantPID: 61885, wantMessage: "the daemon before this one died without its orderly exit, leaving its advertisement stale, and its stdio log holds no panic of its (killed, or its output lost)",
			wantFields: dlog.Context{"pid": 61885, "stale_address": "127.0.0.1:59704", "private_address": "127.0.0.1:59800"}},
		{name: "a legacy advertisement naming no pid", ad: "127.0.0.1:59704\n",
			wantMessage: "the daemon before this one died without its orderly exit, leaving its advertisement stale, and its stdio log holds no panic of its (killed, or its output lost)",
			wantFields:  dlog.Context{"stale_address": "127.0.0.1:59704"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			dir := t.TempDir()
			addr := filepath.Join(dir, "daemon.addr")
			if tt.stdio != "" {
				if err := os.WriteFile(filepath.Join(dir, dlog.StdioLogName), []byte(tt.stdio), 0o644); err != nil {
					t.Fatal(err)
				}
			}
			log := dlog.NewTestLogger()

			// Act
			d := readPredecessorDeath(addr, dir, daemonaddr.ParseAdvertisement(tt.ad), log)

			// Assert
			if d.PID != tt.wantPID || d.Panicked != tt.wantPanic || d.Address != "127.0.0.1:59704" {
				t.Fatalf("death = %+v; want pid %d, panicked %v", d, tt.wantPID, tt.wantPanic)
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
		})
	}
}
