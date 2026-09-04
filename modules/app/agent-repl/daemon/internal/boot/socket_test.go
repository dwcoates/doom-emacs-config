package boot

import (
	"context"
	"errors"
	"net"
	"os"
	"path/filepath"
	"testing"

	"claude-repld/internal/sessionlock"
	"claude-repld/internal/shimsocket"
)

// TestAdoptsASurvivorTheLockDoesNotClaimButTheSocketReaches pins the run-9
// defect: a surviving shim was still bound to the workspace socket while the
// workspace lock read FREE, and the boot spawned over it — the newcomer could
// not bind, and the daemon then dialed the path and reached the survivor,
// which refused StartSession `already_started`. A survivor the boot can REACH
// is adopted.
func TestAdoptsASurvivorTheLockDoesNotClaimButTheSocketReaches(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ws := h.register(t, t.TempDir(), sessionlock.StateFree)
	h.socketProbes[h.deps.Layout.ShimSocket(string(ws.ID))] = shimsocket.StateLive

	// Act.
	_, err := h.seq.Run(context.Background())

	// Assert.
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if got := h.supervisor.calls(); len(got) != 1 || got[0] != ws.ID {
		t.Fatalf("Adopt calls = %v, want exactly %v: a reachable survivor is adopted, never spawned over", got, ws.ID)
	}
}

// TestASurvivorTheSocketReachesIsReportedAdoptedRatherThanClientless pins the
// report: a workspace whose shim was adopted must not also have its in-flight
// turns closed as orphans.
func TestASurvivorTheSocketReachesIsReportedAdoptedRatherThanClientless(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ws := h.register(t, t.TempDir(), sessionlock.StateFree)
	h.socketProbes[h.deps.Layout.ShimSocket(string(ws.ID))] = shimsocket.StateLive

	// Act.
	report, err := h.seq.Run(context.Background())

	// Assert.
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if len(report.Adopted) != 1 || report.Adopted[0] != ws.ID {
		t.Fatalf("report.Adopted = %v, want [%v]", report.Adopted, ws.ID)
	}
}

// TestASocketProbeThatCouldNotTellIsNeverReadAsFree pins the refusal: an
// undetermined socket answers "could not tell whether anybody is there", which
// is never grounds to spawn a second shim onto the path.
func TestASocketProbeThatCouldNotTellIsNeverReadAsFree(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ws := h.register(t, t.TempDir(), sessionlock.StateFree)
	h.socketProbeErrs[h.deps.Layout.ShimSocket(string(ws.ID))] = errors.New("permission denied")

	// Act.
	report, err := h.seq.Run(context.Background())

	// Assert.
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if len(report.Undetermined) != 1 || report.Undetermined[0] != ws.ID {
		t.Fatalf("report.Undetermined = %v, want [%v]", report.Undetermined, ws.ID)
	}
}

// TestStaleSocketSwept pins the second half: an AF_UNIX path is not reclaimed on process death, so a dead shim's
// socket file would fail the next spawn's bind for a reason that no longer
// exists. Its name is kept SHORT deliberately: t.TempDir() embeds the test
// name, and a longer one pushes the socket's sun_path past the kernel's
// 104-byte limit and fails the bind for a reason that is not under test.
func TestStaleSocketSwept(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ws := h.register(t, t.TempDir(), sessionlock.StateFree)
	socket := h.deps.Layout.ShimSocket(string(ws.ID))
	if err := os.MkdirAll(filepath.Dir(socket), 0o755); err != nil {
		t.Fatalf("mkdir %q: %v", filepath.Dir(socket), err)
	}
	// The socket file is bound in a SHORT temp directory and moved into place:
	// t.TempDir() embeds the test name, which pushes sun_path past the
	// kernel's 104-byte limit, and a bound-then-abandoned file is exactly what
	// a dead shim leaves behind wherever it is created.
	short, err := os.MkdirTemp("", "b")
	if err != nil {
		t.Fatalf("temp dir: %v", err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(short) })
	staged := filepath.Join(short, "s.sock")
	listener, err := net.Listen("unix", staged)
	if err != nil {
		t.Fatalf("listen %q: %v", staged, err)
	}
	listener.(*net.UnixListener).SetUnlinkOnClose(false)
	if err := listener.Close(); err != nil {
		t.Fatalf("close %q: %v", staged, err)
	}
	if err := os.Rename(staged, socket); err != nil {
		t.Fatalf("rename %q to %q: %v", staged, socket, err)
	}
	h.socketProbes[socket] = shimsocket.StateStale

	// Act.
	if _, err := h.seq.Run(context.Background()); err != nil {
		t.Fatalf("Run: %v", err)
	}

	// Assert.
	if _, statErr := os.Lstat(socket); !os.IsNotExist(statErr) {
		t.Fatalf("the dead shim's socket %q survived the boot (stat err %v)", socket, statErr)
	}
}
