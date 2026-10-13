package workspace

import (
	"context"
	"errors"
	"net"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/envc"
	"agent-repld/internal/ids"
	"agent-repld/internal/sessionwatcher"
	"agent-repld/internal/shimclient"
)

// holdSocketPath is a socket path SHORT enough for AF_UNIX: t.TempDir()
// embeds the test name, which pushes sun_path past the kernel's limit.
func holdSocketPath(t *testing.T) string {
	t.Helper()
	dir, err := os.MkdirTemp("", "wh")
	if err != nil {
		t.Fatalf("temp dir: %v", err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(dir) })
	return filepath.Join(dir, "hold.sock")
}

// countingStart is a WatcherStarter that counts its calls on a channel.
func countingStart(calls chan<- ids.WorkspaceID) WatcherStarter {
	return func(_ context.Context, ws ids.WorkspaceID, _ shimclient.Client, _ sessionwatcher.Session, _ sessionwatcher.Sinks, _ dlog.Logger) (sessionwatcher.Watcher, error) {
		calls <- ws
		return nil, nil
	}
}

// callStart runs START once, off the test goroutine, answering its error.
func callStart(start WatcherStarter, ws ids.WorkspaceID, log dlog.Logger) <-chan error {
	done := make(chan error, 1)
	go func() {
		_, err := start(context.Background(), ws, nil, sessionwatcher.Session{}, sessionwatcher.Sinks{}, log)
		done <- err
	}()
	return done
}

func TestHoldFirstWatcherStartFromEnvRefusesWhatItCannotHonor(t *testing.T) {
	tests := []struct {
		name    string
		forbid  string
		socket  string
		wantErr string
	}{
		{name: "set without the vendor guard", forbid: "", socket: "/tmp/hold.sock", wantErr: "honored only with " + envc.EnvForbidVendorCalls},
		{name: "a relative socket path", forbid: "1", socket: "hold.sock", wantErr: "not an absolute socket path"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			t.Setenv(envc.EnvForbidVendorCalls, tc.forbid)
			getenv := func(string) string { return tc.socket }

			// Act
			got, err := HoldFirstWatcherStartFromEnv(envc.Load(), getenv, countingStart(make(chan ids.WorkspaceID, 1)))

			// Assert
			if err == nil || !strings.Contains(err.Error(), tc.wantErr) || got != nil {
				t.Fatalf("HoldFirstWatcherStartFromEnv = (%v, %v), want no starter and an error naming %q", got != nil, err, tc.wantErr)
			}
		})
	}
}

func TestHoldFirstWatcherStartFromEnvUnsetStartsUnheld(t *testing.T) {
	// Arrange: no hold is named, and nothing listens anywhere.
	calls := make(chan ids.WorkspaceID, 1)
	start, err := HoldFirstWatcherStartFromEnv(envc.Load(), func(string) string { return "" }, countingStart(calls))
	if err != nil {
		t.Fatalf("HoldFirstWatcherStartFromEnv: %v", err)
	}

	// Act
	err = <-callStart(start, "ws-1", dlog.NewTestLogger())

	// Assert
	if err != nil || len(calls) != 1 {
		t.Fatalf("start = %v with %d calls, want the wrapped start called once", err, len(calls))
	}
}

func TestTheFirstWatcherStartIsHeldUntilTheTestReleasesIt(t *testing.T) {
	// Arrange
	socket := holdSocketPath(t)
	ln, err := net.Listen("unix", socket)
	if err != nil {
		t.Fatalf("listen: %v", err)
	}
	defer ln.Close()
	calls := make(chan ids.WorkspaceID, 1)
	log := dlog.NewTestLogger()
	start := holdFirstWatcherStart(socket, dialHold, countingStart(calls))
	done := callStart(start, "ws-1", log)
	conn, err := ln.Accept()
	if err != nil {
		t.Fatalf("accept: %v", err)
	}
	// Assert: held. The start reads until the release, so it cannot have run.
	if len(calls) != 0 {
		t.Fatal("the wrapped start ran while the hold was still open")
	}

	// Act
	conn.Close()

	// Assert: released, then started.
	if err := <-done; err != nil {
		t.Fatalf("held start = %v, want a success once released", err)
	}
	if got := <-calls; got != "ws-1" {
		t.Fatalf("the wrapped start ran for %q, want ws-1", got)
	}
}

func TestAHeldWatcherStartRecordsItsHoldAndRelease(t *testing.T) {
	// Arrange
	socket := holdSocketPath(t)
	ln, err := net.Listen("unix", socket)
	if err != nil {
		t.Fatalf("listen: %v", err)
	}
	defer ln.Close()
	log := dlog.NewTestLogger()
	done := callStart(holdFirstWatcherStart(socket, dialHold, countingStart(make(chan ids.WorkspaceID, 1))), "ws-1", log)
	conn, err := ln.Accept()
	if err != nil {
		t.Fatalf("accept: %v", err)
	}

	// Act
	conn.Close()
	if err := <-done; err != nil {
		t.Fatalf("held start: %v", err)
	}

	// Assert
	want := []string{
		"the session watcher's start is held until the test releases it",
		"the test released the session watcher's start",
	}
	records := log.Records()
	if len(records) != len(want) {
		t.Fatalf("records = %+v, want exactly the hold and its release", records)
	}
	for i, r := range records {
		if r.Level != "info" || r.Operation != opWatcherHold || r.Message != want[i] ||
			r.Context["workspace_id"] != "ws-1" || r.Context["socket"] != socket {
			t.Fatalf("record %d = %+v, want info %s %q naming ws-1 and %s", i, r, opWatcherHold, want[i], socket)
		}
	}
}

func TestOnlyTheFirstWatcherStartIsHeld(t *testing.T) {
	// Arrange: the first start was held and released, and the hold is gone.
	socket := holdSocketPath(t)
	ln, err := net.Listen("unix", socket)
	if err != nil {
		t.Fatalf("listen: %v", err)
	}
	calls := make(chan ids.WorkspaceID, 2)
	start := holdFirstWatcherStart(socket, dialHold, countingStart(calls))
	first := callStart(start, "ws-1", dlog.NewTestLogger())
	conn, err := ln.Accept()
	if err != nil {
		t.Fatalf("accept: %v", err)
	}
	conn.Close()
	if err := <-first; err != nil {
		t.Fatalf("first start: %v", err)
	}
	ln.Close()

	// Act
	err = <-callStart(start, "ws-2", dlog.NewTestLogger())

	// Assert
	if err != nil {
		t.Fatalf("second start = %v, want it unheld", err)
	}
	if got := []ids.WorkspaceID{<-calls, <-calls}; got[0] != "ws-1" || got[1] != "ws-2" {
		t.Fatalf("wrapped starts = %v, want ws-1 then ws-2", got)
	}
}

func TestAWatcherStartWhoseHoldCannotBeReachedFails(t *testing.T) {
	// Arrange: the hold names a socket nobody listens on.
	socket := holdSocketPath(t)
	calls := make(chan ids.WorkspaceID, 1)
	log := dlog.NewTestLogger()

	// Act
	err := <-callStart(holdFirstWatcherStart(socket, dialHold, countingStart(calls)), "ws-1", log)

	// Assert: refused, recorded at error with its cause, and never started.
	if err == nil || !strings.Contains(err.Error(), socket) {
		t.Fatalf("start = %v, want an error naming %s", err, socket)
	}
	if len(calls) != 0 {
		t.Fatal("the wrapped start ran without its hold")
	}
	records := log.Records()
	if len(records) != 1 {
		t.Fatalf("records = %+v, want one error record", records)
	}
	r := records[0]
	if r.Level != "error" || r.Operation != opWatcherHold || r.Context["workspace_id"] != "ws-1" ||
		r.Context["socket"] != socket || r.Context["cause"] == nil {
		t.Fatalf("record = %+v, want an error on %s naming the workspace, socket and cause", r, opWatcherHold)
	}
}

// resetConn is a hold connection whose read fails: the hold ended without
// the test's close.
type resetConn struct{ net.Conn }

func (resetConn) Read([]byte) (int, error) { return 0, errors.New("connection reset by peer") }
func (resetConn) Close() error             { return nil }

func TestAWatcherStartWhoseHoldEndsWithoutItsReleaseFails(t *testing.T) {
	// Arrange
	calls := make(chan ids.WorkspaceID, 1)
	log := dlog.NewTestLogger()
	dial := func(context.Context, string) (net.Conn, error) { return resetConn{}, nil }

	// Act
	err := <-callStart(holdFirstWatcherStart("/tmp/hold.sock", dial, countingStart(calls)), "ws-1", log)

	// Assert: refused, recorded at error with its cause, and never started.
	if err == nil || !strings.Contains(err.Error(), "connection reset by peer") {
		t.Fatalf("start = %v, want the hold's read error", err)
	}
	if len(calls) != 0 {
		t.Fatal("the wrapped start ran after its hold failed")
	}
	records := log.Records()
	last := records[len(records)-1]
	if last.Level != "error" || last.Operation != opWatcherHold || last.Message != "the test's hold on the session watcher's start ended without its release" ||
		last.Context["workspace_id"] != "ws-1" || last.Context["cause"] != "connection reset by peer" {
		t.Fatalf("last record = %+v, want the hold's failure at error with its cause", last)
	}
}
