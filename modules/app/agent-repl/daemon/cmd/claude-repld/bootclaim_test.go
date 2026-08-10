package main

import (
	"errors"
	"net"
	"os"
	"path/filepath"
	"testing"
)

// freeLoopbackAddr returns a loopback address that is free right now, by
// binding one and handing back the address after closing it. Used only for the
// "no incumbent" arrangements; the incumbent cases keep their listener open.
func freeLoopbackAddr(t *testing.T) string {
	t.Helper()
	l, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatalf("reserve loopback addr: %v", err)
	}
	addr := l.Addr().String()
	if err := l.Close(); err != nil {
		t.Fatalf("release loopback addr: %v", err)
	}
	return addr
}

// shortSocketPath returns a socket path inside a temp dir with a SHORT name.
// t.TempDir() embeds the test's name, and a unix socket path over the
// platform's sun_path limit (104 bytes on darwin) fails to bind with
// "invalid argument" — a test-harness artifact, not a daemon behavior.
func shortSocketPath(t *testing.T, name string) string {
	t.Helper()
	dir, err := os.MkdirTemp("", "arb")
	if err != nil {
		t.Fatalf("temp socket dir: %v", err)
	}
	t.Cleanup(func() {
		if err := os.RemoveAll(dir); err != nil {
			t.Errorf("remove temp socket dir: %v", err)
		}
	})
	return filepath.Join(dir, name)
}

// staleSocketFile leaves a real-but-dead unix socket file at path: the listener
// is closed with unlink-on-close disabled, which is exactly what a daemon that
// died without cleaning up leaves behind.
func staleSocketFile(t *testing.T, path string) {
	t.Helper()
	addr, err := net.ResolveUnixAddr("unix", path)
	if err != nil {
		t.Fatalf("resolve unix addr: %v", err)
	}
	l, err := net.ListenUnix("unix", addr)
	if err != nil {
		t.Fatalf("bind stale socket: %v", err)
	}
	l.SetUnlinkOnClose(false)
	if err := l.Close(); err != nil {
		t.Fatalf("close stale socket: %v", err)
	}
	if _, err := os.Stat(path); err != nil {
		t.Fatalf("stale socket file did not survive its listener: %v", err)
	}
}

// TestBootOrderLeavesIncumbentFrontendSocketServing is the regression test for
// the duplicate-daemon outage: a second daemon booting against a live one must
// lose the exclusive claim BEFORE it reaches the unlink-and-rebind of the
// incumbent's frontend socket, so the incumbent keeps serving a socket Emacs
// can still dial.
func TestBootOrderLeavesIncumbentFrontendSocketServing(t *testing.T) {
	// Arrange: an incumbent daemon holding both endpoints.
	incumbentHTTP, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatalf("incumbent http listen: %v", err)
	}
	defer incumbentHTTP.Close()
	sockPath := shortSocketPath(t, "frontend.sock")
	incumbentUDS, err := net.Listen("unix", sockPath)
	if err != nil {
		t.Fatalf("incumbent uds listen: %v", err)
	}
	defer incumbentUDS.Close()

	// Act: the duplicate runs boot's acquisition order — exclusive claim first,
	// unix bind only through the claim it won.
	claim, err := claimBootExclusivity(incumbentHTTP.Addr().String())
	if err == nil {
		if _, bindErr := claim.ListenFrontendUDS(sockPath); bindErr != nil {
			t.Fatalf("unexpected frontend bind failure: %v", bindErr)
		}
	}

	// Assert: the duplicate identified the incumbent, and the incumbent's
	// socket is still the one at the path and still accepting.
	if !errors.Is(err, errIncumbentDaemon) {
		t.Fatalf("claim error = %v, want errIncumbentDaemon", err)
	}
	conn, dialErr := net.Dial("unix", sockPath)
	if dialErr != nil {
		t.Fatalf("incumbent frontend socket no longer dialable: %v", dialErr)
	}
	defer conn.Close()
	accepted, acceptErr := incumbentUDS.Accept()
	if acceptErr != nil {
		t.Fatalf("incumbent did not accept on its own socket: %v", acceptErr)
	}
	accepted.Close()
}

// TestBootClaimCleansStaleFrontendSocketWhenNoIncumbent keeps the other half of
// the guarantee honest: with nobody holding the claim, the leftovers of a dead
// daemon are still cleaned and rebound.
func TestBootClaimCleansStaleFrontendSocketWhenNoIncumbent(t *testing.T) {
	// Arrange.
	sockPath := shortSocketPath(t, "frontend.sock")
	staleSocketFile(t, sockPath)

	// Act.
	claim, err := claimBootExclusivity(freeLoopbackAddr(t))
	if err != nil {
		t.Fatalf("claim on a free address: %v", err)
	}
	defer claim.HTTPListener().Close()
	l, err := claim.ListenFrontendUDS(sockPath)
	if err != nil {
		t.Fatalf("bind over stale socket: %v", err)
	}
	defer l.Close()

	// Assert.
	conn, err := net.Dial("unix", sockPath)
	if err != nil {
		t.Fatalf("rebound frontend socket not dialable: %v", err)
	}
	defer conn.Close()
}

// TestBootClaimRefusesFrontendBindWithoutClaim covers the structural half: the
// unlink-and-rebind path is unreachable without the exclusive claim, and
// refusing it leaves the existing socket file alone.
func TestBootClaimRefusesFrontendBindWithoutClaim(t *testing.T) {
	// Arrange.
	sockPath := shortSocketPath(t, "frontend.sock")
	incumbentUDS, err := net.Listen("unix", sockPath)
	if err != nil {
		t.Fatalf("incumbent uds listen: %v", err)
	}
	defer incumbentUDS.Close()

	// Act.
	var unclaimed *bootClaim
	l, err := unclaimed.ListenFrontendUDS(sockPath)

	// Assert.
	if !errors.Is(err, errNoBootClaim) {
		if l != nil {
			l.Close()
		}
		t.Fatalf("bind error = %v, want errNoBootClaim", err)
	}
	if _, statErr := os.Stat(sockPath); statErr != nil {
		t.Fatalf("refused bind removed the socket anyway: %v", statErr)
	}
}

// TestBootClaimRefusesShimBindWithoutClaim is the shim socket's copy of the
// same refusal: it is the other unix path whose bind unlinks.
func TestBootClaimRefusesShimBindWithoutClaim(t *testing.T) {
	// Arrange.
	sockPath := shortSocketPath(t, "shim.sock")
	incumbentUDS, err := net.Listen("unix", sockPath)
	if err != nil {
		t.Fatalf("incumbent uds listen: %v", err)
	}
	defer incumbentUDS.Close()

	// Act.
	var unclaimed *bootClaim
	err = unclaimed.ListenShim(nil, sockPath)

	// Assert.
	if !errors.Is(err, errNoBootClaim) {
		t.Fatalf("shim bind error = %v, want errNoBootClaim", err)
	}
	if _, statErr := os.Stat(sockPath); statErr != nil {
		t.Fatalf("refused shim bind removed the socket anyway: %v", statErr)
	}
}

// TestClaimBootExclusivityClassifiesFailures keeps a malformed address from
// being reported as an incumbent: the two failures call for different operator
// responses, so they must not collapse into one message.
func TestClaimBootExclusivityClassifiesFailures(t *testing.T) {
	incumbent, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatalf("incumbent http listen: %v", err)
	}
	defer incumbent.Close()

	tests := []struct {
		name          string
		addr          string
		wantIncumbent bool
	}{
		{name: "address held by a live daemon", addr: incumbent.Addr().String(), wantIncumbent: true},
		{name: "malformed address", addr: "127.0.0.1:not-a-port", wantIncumbent: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			claim, err := claimBootExclusivity(tc.addr)

			// Assert.
			if claim != nil {
				claim.HTTPListener().Close()
				t.Fatalf("claim unexpectedly won on %s", tc.addr)
			}
			if got := errors.Is(err, errIncumbentDaemon); got != tc.wantIncumbent {
				t.Fatalf("errors.Is(%v, errIncumbentDaemon) = %v, want %v", err, got, tc.wantIncumbent)
			}
		})
	}
}
