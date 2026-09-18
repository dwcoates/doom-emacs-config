//go:build integration

package integration

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"claude-repld/integration/harness"
)

// TestADaemonStandsDownWhenWhatItOwnsOnDiskVanishes removes, from under a
// serving daemon, each thing it owns on disk, and holds it to exiting on its
// own -- within ONE harness wait -- non-zero, with the loss named.
//
// A daemon whose state root was deleted used to serve on indefinitely: the
// integration suite's own orphans, reparented to launchd with their roots
// gone, went on sweeping and retrying until someone killed them by hand.
func TestADaemonStandsDownWhenWhatItOwnsOnDiskVanishes(t *testing.T) {
	t.Parallel()
	cases := []struct {
		name string
		// open serves a workspace with a live shim first.
		open   bool
		remove func(d *harness.Daemon) string
		// want is the loss the exit must name.
		want string
	}{
		{
			name:   "the whole state root",
			remove: func(d *harness.Daemon) string { return d.StateDir },
			want:   "is gone",
		},
		{
			name:   "the whole state root under a served workspace",
			open:   true,
			remove: func(d *harness.Daemon) string { return d.StateDir },
			want:   "is gone",
		},
		{
			name:   "daemon.lock",
			remove: func(d *harness.Daemon) string { return filepath.Join(d.StateDir, "daemon.lock") },
			want:   "daemon.lock",
		},
		{
			name:   "daemon.addr",
			remove: func(d *harness.Daemon) string { return d.AddrFile() },
			want:   "daemon.addr",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			// Arrange
			var d *harness.Daemon
			if tc.open {
				f := newOpened(t, harness.Opts{})
				d = f.d
			} else {
				d = newDaemon(t, harness.Opts{})
			}
			d.ExpectWarnings("daemon.cmd.state_root")

			// Act
			if err := os.RemoveAll(tc.remove(d)); err != nil {
				t.Fatalf("remove: %v", err)
			}
			code := d.AwaitExit()

			// Assert
			if code == 0 {
				t.Fatalf("the daemon exited 0 after losing %s, want a non-zero stand-down\nstderr:\n%s", tc.name, d.Stderr())
			}
			stderr := d.Stderr()
			if !strings.Contains(stderr, "stood down") || !strings.Contains(stderr, tc.want) {
				t.Fatalf("the exit does not name the loss %q:\n%s", tc.want, stderr)
			}
			// THE RECORD IS READ OFF THE TERMINAL MIRROR, because the run log
			// may have gone with the root it lived in.
			if !hasStandDownRecord(stderr, tc.want) {
				t.Fatalf("no ERROR daemon.cmd.state_root record naming %q:\n%s", tc.want, stderr)
			}
		})
	}
}

// hasStandDownRecord reports whether the terminal mirror carries the ERROR
// stand-down record with a cause naming want.
func hasStandDownRecord(stderr, want string) bool {
	for _, line := range strings.Split(stderr, "\n") {
		var rec harness.LogRecord
		if json.Unmarshal([]byte(line), &rec) != nil {
			continue
		}
		cause, _ := rec.Context["cause"].(string)
		if rec.Level == "error" && rec.Operation == "daemon.cmd.state_root" && strings.Contains(cause, want) {
			return true
		}
	}
	return false
}
