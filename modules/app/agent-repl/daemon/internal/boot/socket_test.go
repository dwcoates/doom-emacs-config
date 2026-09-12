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

// TestASurvivorTheSocketReachesIsReportedInert pins WHICH survivor it is: the
// shim takes the workspace lock at StartSession, never at process start, so a
// live listener behind a FREE lock has no session on it. It is adopted as a
// process and recorded as inert.
func TestASurvivorTheSocketReachesIsReportedInert(t *testing.T) {
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
	if len(report.AdoptedInert) != 1 || report.AdoptedInert[0] != ws.ID {
		t.Fatalf("report.AdoptedInert = %v, want [%v]", report.AdoptedInert, ws.ID)
	}
}

// TestAnInertSurvivorIsNotAnAdoptedSession pins the bounce accounting's input:
// an inert shim never started a session, so there is no session whose survival
// a bounce could have decided and none is handed to Reconcile.
func TestAnInertSurvivorIsNotAnAdoptedSession(t *testing.T) {
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
	if len(report.AdoptedSessions) != 0 {
		t.Fatalf("report.AdoptedSessions = %+v, want none: an inert shim carries no session", report.AdoptedSessions)
	}
}

// TestAnInertSurvivorIsNotWarnedAbout pins the level: free-lock-and-listening
// is what an inert shim looks like BY CONTRACT, so recording it as a
// disagreement between two kernel facts states something untrue.
func TestAnInertSurvivorIsNotWarnedAbout(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ws := h.register(t, t.TempDir(), sessionlock.StateFree)
	h.socketProbes[h.deps.Layout.ShimSocket(string(ws.ID))] = shimsocket.StateLive

	// Act.
	if _, err := h.seq.Run(context.Background()); err != nil {
		t.Fatalf("Run: %v", err)
	}

	// Assert.
	if h.hasRecord("warn", "daemon.boot.adopt") {
		t.Fatal("an inert survivor's adoption was warned about; it is the ordinary state of a shim with no session")
	}
}

// TestALockHeldSurvivorIsAnAdoptedSessionNamingItsPID pins the other arm: a
// HELD lock means a session was started on that process, so it reaches the
// bounce accounting, and it names the pid rather than the zero value an empty
// manifest entry used to supply.
func TestALockHeldSurvivorIsAnAdoptedSessionNamingItsPID(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ws := h.register(t, t.TempDir(), sessionlock.StateHeld)

	// Act.
	report, err := h.seq.Run(context.Background())

	// Assert.
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if len(report.AdoptedSessions) != 1 {
		t.Fatalf("report.AdoptedSessions = %+v, want one", report.AdoptedSessions)
	}
	got := report.AdoptedSessions[0]
	if got.Workspace != ws.ID || got.ShimPID != adoptedShimPID {
		t.Fatalf("adopted session = %+v, want workspace %v and pid %d", got, ws.ID, adoptedShimPID)
	}
}
