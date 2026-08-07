package pprofsurface

import (
	"context"
	"errors"
	"net"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// shortSock keeps a socket path inside the platform's sun_path limit, which
// t.TempDir()'s long names blow on darwin.
func shortSock(t *testing.T, name string) string {
	t.Helper()
	dir, err := os.MkdirTemp("", "pp")
	if err != nil {
		t.Fatalf("mkdtemp: %v", err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(dir) })
	return filepath.Join(dir, name)
}

// serve runs the surface and returns a client bound to whichever transport it
// listens on. The serve goroutine's completion is awaited by Cleanup, so no
// test synchronizes on elapsed time.
func serve(t *testing.T, s *Surface) *http.Client {
	t.Helper()
	done := make(chan error, 1)
	go func() { done <- s.Serve() }()
	t.Cleanup(func() {
		if err := s.Close(); err != nil {
			t.Errorf("close surface: %v", err)
		}
		if err := <-done; err != nil && !errors.Is(err, http.ErrServerClosed) {
			t.Errorf("serve: %v", err)
		}
	})
	network, address := s.Network(), s.Address()
	return &http.Client{Transport: &http.Transport{
		DialContext: func(ctx context.Context, _, _ string) (net.Conn, error) {
			return (&net.Dialer{}).DialContext(ctx, network, address)
		},
	}}
}

func TestOpenIsOffByDefault(t *testing.T) {
	// Arrange, Act. An unset AGENT_REPL_PPROF_ADDR reaches Open as "".
	surface, err := Open("")

	// Assert. No listener exists at all: the capability is absent, not bound
	// and firewalled.
	if err != nil {
		t.Fatalf("Open(\"\") = %v, want nil error", err)
	}
	if surface != nil {
		t.Fatalf("Open(\"\") = %+v, want no surface", surface)
	}
}

func TestOpenServesProfilesOnAnExplicitLoopbackPort(t *testing.T) {
	// Arrange.
	surface, err := Open("127.0.0.1:0")
	if err != nil {
		t.Fatalf("Open = %v, want a bound surface", err)
	}
	client := serve(t, surface)

	// Act.
	response, err := client.Get("http://" + surface.Address() + Path)
	if err != nil {
		t.Fatalf("GET %s: %v", Path, err)
	}
	defer response.Body.Close()

	// Assert.
	if response.StatusCode != http.StatusOK {
		t.Fatalf("GET %s = %d, want 200", Path, response.StatusCode)
	}
}

func TestOpenServesProfilesOnAUnixSocket(t *testing.T) {
	// Arrange.
	path := shortSock(t, "pprof.sock")
	surface, err := Open(path)
	if err != nil {
		t.Fatalf("Open(%q) = %v, want a bound surface", path, err)
	}
	client := serve(t, surface)

	// Act. The host is ignored by the dialer above; only the socket matters.
	response, err := client.Get("http://pprof" + Path + "cmdline")
	if err != nil {
		t.Fatalf("GET over unix socket: %v", err)
	}
	defer response.Body.Close()

	// Assert.
	if response.StatusCode != http.StatusOK {
		t.Fatalf("GET cmdline = %d, want 200", response.StatusCode)
	}
}

func TestOpenRestrictsAUnixSocketToItsOwner(t *testing.T) {
	// Arrange.
	path := shortSock(t, "pprof.sock")
	surface, err := Open(path)
	if err != nil {
		t.Fatalf("Open(%q) = %v, want a bound surface", path, err)
	}
	serve(t, surface)

	// Act.
	info, err := os.Stat(surface.Address())
	if err != nil {
		t.Fatalf("stat socket: %v", err)
	}

	// Assert.
	if perm := info.Mode().Perm(); perm != 0o600 {
		t.Fatalf("socket mode = %v, want 0600 so no other user can read this process's heap", perm)
	}
}

func TestOpenReclaimsAStaleSocket(t *testing.T) {
	// Arrange. A daemon that died holding the path leaves this behind.
	path := shortSock(t, "pprof.sock")
	stale, err := net.Listen("unix", path)
	if err != nil {
		t.Fatalf("stage stale socket: %v", err)
	}
	if err := stale.Close(); err != nil {
		t.Fatalf("close stale listener: %v", err)
	}

	// Act.
	surface, err := Open(path)

	// Assert.
	if err != nil {
		t.Fatalf("Open over a stale socket = %v, want the capability reusable", err)
	}
	serve(t, surface)
}

func TestOpenRefusesUnsafeAddresses(t *testing.T) {
	tests := []struct {
		name string
		addr string
	}{
		{name: "a wildcard bind would publish stacks to the network", addr: "0.0.0.0:6060"},
		{name: "an empty host is a wildcard bind", addr: ":6060"},
		{name: "a routable host is not loopback", addr: "10.0.0.4:6060"},
		{name: "a bare port is neither a socket path nor an address", addr: "6060"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			surface, err := Open(tc.addr)

			// Assert.
			if err == nil {
				surface.Close()
				t.Fatalf("Open(%q) = nil error, want a loud refusal", tc.addr)
			}
		})
	}
}

func TestOpenRefusesToReplaceANonSocketPath(t *testing.T) {
	// Arrange.
	path := shortSock(t, "notasocket")
	if err := os.WriteFile(path, []byte("precious"), 0o600); err != nil {
		t.Fatalf("stage file: %v", err)
	}

	// Act.
	surface, err := Open(path)

	// Assert. A profiling knob must never delete an operator's file.
	if err == nil {
		surface.Close()
		t.Fatal("Open over a regular file = nil error, want a refusal")
	}
	if _, statErr := os.Stat(path); statErr != nil {
		t.Fatalf("staged file after refusal: %v, want it untouched", statErr)
	}
}

func TestURLNamesTheTransportItsClientMustDial(t *testing.T) {
	tests := []struct {
		name       string
		addr       func(t *testing.T) string
		wantPrefix string
	}{
		{
			name:       "a loopback port is an http base",
			addr:       func(*testing.T) string { return "127.0.0.1:0" },
			wantPrefix: "http://127.0.0.1:",
		},
		{
			name:       "a socket reports the path to dial",
			addr:       func(t *testing.T) string { return shortSock(t, "pprof.sock") },
			wantPrefix: "unix://",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			surface, err := Open(tc.addr(t))
			if err != nil {
				t.Fatalf("Open = %v", err)
			}
			serve(t, surface)

			// Act.
			got := surface.URL()

			// Assert.
			if !strings.HasPrefix(got, tc.wantPrefix) || !strings.HasSuffix(got, Path) {
				t.Fatalf("URL() = %q, want %q…%q", got, tc.wantPrefix, Path)
			}
		})
	}
}

func TestCloseRemovesTheSocketItCreated(t *testing.T) {
	// Arrange.
	path := shortSock(t, "pprof.sock")
	surface, err := Open(path)
	if err != nil {
		t.Fatalf("Open = %v", err)
	}
	done := make(chan error, 1)
	go func() { done <- surface.Serve() }()

	// Act.
	if err := surface.Close(); err != nil {
		t.Fatalf("Close = %v, want nil", err)
	}
	if err := <-done; err != nil && !errors.Is(err, http.ErrServerClosed) {
		t.Fatalf("Serve = %v", err)
	}

	// Assert. A surviving socket would make the next enable look like a
	// conflict rather than a fresh bind.
	if _, statErr := os.Stat(path); !errors.Is(statErr, os.ErrNotExist) {
		t.Fatalf("socket after Close: %v, want it gone", statErr)
	}
}
