package shimclient

import (
	"errors"
	"path/filepath"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

func TestDied(t *testing.T) {
	tests := []struct {
		name     string
		pid      int
		reaped   *ExitInfo
		exiting  bool
		wantDied bool
		wantPID  int
		wantSig  string
	}{
		{name: "a reaped exit is the verdict", pid: 4242, reaped: &ExitInfo{PID: 4242, Code: -1, Signal: "killed"}, wantDied: true, wantPID: 4242, wantSig: "killed"},
		{name: "an exiting process not yet reaped is a death", pid: 4242, exiting: true, wantDied: true, wantPID: 4242},
		{name: "a running process is no death", pid: 4242, exiting: false, wantDied: false},
		{name: "an unknown pid decides nothing", pid: 0, exiting: true, wantDied: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			c := newClient(dlog.NewTestLogger(), ids.WorkspaceID("ws-1"), filepath.Join(t.TempDir(), "s.sock"), defaultBackoff, nil, nil)
			c.pid = tt.pid
			c.processExiting = func(int) (bool, error) { return tt.exiting, nil }
			if tt.reaped != nil {
				c.publishExit(*tt.reaped)
			}

			// Act.
			info, died := c.Died()

			// Assert.
			if died != tt.wantDied {
				t.Fatalf("Died() died = %v, want %v", died, tt.wantDied)
			}
			if died && (info.PID != tt.wantPID || info.Signal != tt.wantSig) {
				t.Fatalf("Died() info = %+v, want pid %d signal %q", info, tt.wantPID, tt.wantSig)
			}
		})
	}
}

func TestDiedRecordsAnUnreadableProcessStateAtError(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	c := newClient(log, ids.WorkspaceID("ws-1"), filepath.Join(t.TempDir(), "s.sock"), defaultBackoff, nil, nil)
	c.pid = 4242
	c.processExiting = func(int) (bool, error) { return false, errors.New("sysctl refused") }

	// Act.
	_, died := c.Died()

	// Assert.
	if died {
		t.Fatal("Died() = true on an unreadable process state, want false")
	}
	records := log.Records()
	if len(records) != 1 || records[0].Level != dlog.LevelError || records[0].Operation != "daemon.shimclient.exit" ||
		records[0].Context["pid"] != 4242 || records[0].Context["error"] != "sysctl refused" || records[0].Context["workspace_id"] != "ws-1" {
		t.Fatalf("records = %+v, want one ERROR naming the workspace, pid and cause", records)
	}
}
