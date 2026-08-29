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

	"claude-repld/internal/dlog"
)

// socketPath is a unix socket path short enough for sockaddr_un. t.TempDir()
// embeds the test's name, which on macOS can overrun the 104-byte sun_path.
func socketPath(t *testing.T) string {
	t.Helper()
	dir, err := os.MkdirTemp("", "pp")
	if err != nil {
		t.Fatalf("MkdirTemp: %v", err)
	}
	t.Cleanup(func() { os.RemoveAll(dir) })
	return filepath.Join(dir, "pprof.sock")
}

// findRecord returns the first captured record with the operation.
func findRecord(records []dlog.Record, operation string) (dlog.Record, bool) {
	for _, rec := range records {
		if rec.Operation == operation {
			return rec, true
		}
	}
	return dlog.Record{}, false
}

func TestOpenIsOffWhenTheAddressIsEmpty(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()

	// Act.
	s, err := Open(context.Background(), "", log)

	// Assert: empty is OFF and is the default; there is no always-on listener.
	if err != nil {
		t.Fatalf("Open(\"\") = %v, want no error", err)
	}
	if s != nil {
		s.Close()
		t.Fatalf("Open(\"\") returned a surface")
	}
}

func TestOpenRecordsTheDisabledDecision(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()

	// Act.
	if _, err := Open(context.Background(), "", log); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Assert: the decision is recorded either way.
	if _, ok := findRecord(log.Records(), "daemon.pprof.disabled"); !ok {
		t.Fatalf("records = %+v, want daemon.pprof.disabled", log.Records())
	}
}

func TestOpenRefusesANonLocalBind(t *testing.T) {
	tests := []struct {
		name string
		addr string
	}{
		{name: "wildcard IPv4", addr: "0.0.0.0:6060"},
		{name: "wildcard IPv6", addr: "[::]:6060"},
		{name: "no host binds every interface", addr: ":6060"},
		{name: "routable address", addr: "8.8.8.8:6060"},
		{name: "a name that may resolve anywhere", addr: "example.com:6060"},
		{name: "neither socket nor host:port", addr: "6060"},
		{name: "non-numeric port", addr: "127.0.0.1:profiling"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			log := dlog.NewTestLogger()

			// Act.
			s, err := Open(context.Background(), tc.addr, log)

			// Assert: refused here rather than opened and regretted.
			if err == nil {
				s.Close()
				t.Fatalf("Open(%q) opened a listener", tc.addr)
			}
			if !errors.Is(err, ErrNotLocal) {
				t.Fatalf("error = %v, want ErrNotLocal", err)
			}
		})
	}
}

func TestOpenRecordsTheRefusal(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()

	// Act.
	if _, err := Open(context.Background(), "0.0.0.0:6060", log); err == nil {
		t.Fatalf("Open accepted a wildcard bind")
	}

	// Assert.
	rec, ok := findRecord(log.Records(), "daemon.pprof.refused")
	if !ok {
		t.Fatalf("records = %+v, want daemon.pprof.refused", log.Records())
	}
	if rec.Level != "error" {
		t.Fatalf("level = %q, want error", rec.Level)
	}
}

func TestOpenAcceptsAnExplicitlyLoopbackBind(t *testing.T) {
	tests := []struct {
		name string
		addr string
	}{
		{name: "loopback IPv4", addr: "127.0.0.1:0"},
		{name: "localhost by name", addr: "localhost:0"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			log := dlog.NewTestLogger()

			// Act.
			s, err := Open(context.Background(), tc.addr, log)
			if err != nil {
				t.Fatalf("Open(%q): %v", tc.addr, err)
			}
			defer s.Close()

			// Assert.
			if s.Network() != "tcp" {
				t.Fatalf("Network = %q, want tcp", s.Network())
			}
			if strings.HasSuffix(s.Address(), ":0") {
				t.Fatalf("Address = %q, want the actually-bound port", s.Address())
			}
			if s.URL() != "http://"+s.Address()+"/debug/pprof/" {
				t.Fatalf("URL = %q", s.URL())
			}
		})
	}
}

func TestOpenServesTheProfilingIndex(t *testing.T) {
	// Arrange.
	s, err := Open(context.Background(), "127.0.0.1:0", dlog.NewTestLogger())
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	defer s.Close()

	// Act.
	resp, err := http.Get(s.URL())
	if err != nil {
		t.Fatalf("GET %s: %v", s.URL(), err)
	}
	defer resp.Body.Close()

	// Assert.
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("status = %d, want 200", resp.StatusCode)
	}
}

func TestOpenServesOnAUnixSocket(t *testing.T) {
	// Arrange.
	socket := socketPath(t)

	// Act.
	s, err := Open(context.Background(), socket, dlog.NewTestLogger())
	if err != nil {
		t.Fatalf("Open(%q): %v", socket, err)
	}
	defer s.Close()

	// Assert.
	if s.Network() != "unix" {
		t.Fatalf("Network = %q, want unix", s.Network())
	}
	client := &http.Client{Transport: &http.Transport{
		DialContext: func(ctx context.Context, _, _ string) (net.Conn, error) {
			return (&net.Dialer{}).DialContext(ctx, "unix", socket)
		},
	}}
	resp, err := client.Get("http://unix/debug/pprof/")
	if err != nil {
		t.Fatalf("GET over the socket: %v", err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("status = %d, want 200", resp.StatusCode)
	}
}

func TestCloseRemovesTheSocket(t *testing.T) {
	// Arrange.
	socket := socketPath(t)
	s, err := Open(context.Background(), socket, dlog.NewTestLogger())
	if err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Act.
	if err := s.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Assert.
	if _, err := os.Stat(socket); !os.IsNotExist(err) {
		t.Fatalf("the socket survived Close (stat err = %v)", err)
	}
}

func TestCloseStopsServing(t *testing.T) {
	// Arrange.
	s, err := Open(context.Background(), "127.0.0.1:0", dlog.NewTestLogger())
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	url := s.URL()

	// Act.
	if err := s.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Assert.
	resp, err := http.Get(url)
	if err == nil {
		resp.Body.Close()
		t.Fatalf("the profiling surface still serves after Close")
	}
}

func TestOpenRecordsTheEnabledSurfaceLoudly(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()

	// Act.
	s, err := Open(context.Background(), "127.0.0.1:0", log)
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	defer s.Close()

	// Assert: WARN, because an open profiling surface is a deliberate,
	// temporary state that should still be noticed tomorrow.
	rec, ok := findRecord(log.Records(), "daemon.pprof.enabled")
	if !ok {
		t.Fatalf("records = %+v, want daemon.pprof.enabled", log.Records())
	}
	if rec.Level != "warn" {
		t.Fatalf("level = %q, want warn", rec.Level)
	}
	for _, key := range []string{"network", "address", "url"} {
		if _, ok := rec.Context[key]; !ok {
			t.Fatalf("context = %v, missing %q", rec.Context, key)
		}
	}
}

func TestOpenReportsAnUnavailableBind(t *testing.T) {
	// Arrange: something already owns the socket path.
	socket := socketPath(t)
	incumbent, err := net.Listen("unix", socket)
	if err != nil {
		t.Fatalf("listen: %v", err)
	}
	defer incumbent.Close()
	log := dlog.NewTestLogger()

	// Act.
	s, err := Open(context.Background(), socket, log)

	// Assert: surfaced, never quietly skipped.
	if err == nil {
		s.Close()
		t.Fatalf("Open reused an occupied socket path")
	}
	if _, ok := findRecord(log.Records(), "daemon.pprof.open_failed"); !ok {
		t.Fatalf("records = %+v, want daemon.pprof.open_failed", log.Records())
	}
}

func TestResolveClassifiesTheAddress(t *testing.T) {
	tests := []struct {
		name        string
		addr        string
		wantNetwork string
	}{
		{name: "a path is a socket", addr: "/tmp/pprof.sock", wantNetwork: "unix"},
		{name: "a .sock suffix is a socket", addr: "pprof.sock", wantNetwork: "unix"},
		{name: "loopback host:port is tcp", addr: "127.0.0.1:6060", wantNetwork: "tcp"},
		{name: "loopback IPv6 is tcp", addr: "[::1]:6060", wantNetwork: "tcp"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			network, _, err := resolve(tc.addr)

			// Assert.
			if err != nil {
				t.Fatalf("resolve(%q): %v", tc.addr, err)
			}
			if network != tc.wantNetwork {
				t.Fatalf("network = %q, want %q", network, tc.wantNetwork)
			}
		})
	}
}

func TestResolveAbsolutizesASocketPath(t *testing.T) {
	// Arrange, Act.
	_, address, err := resolve("pprof.sock")

	// Assert.
	if err != nil {
		t.Fatalf("resolve: %v", err)
	}
	if !filepath.IsAbs(address) {
		t.Fatalf("address = %q, want an absolute socket path", address)
	}
}

func TestIsLoopbackHost(t *testing.T) {
	tests := []struct {
		name string
		host string
		want bool
	}{
		{name: "IPv4 loopback", host: "127.0.0.1", want: true},
		{name: "the rest of the loopback block", host: "127.9.9.9", want: true},
		{name: "IPv6 loopback", host: "::1", want: true},
		{name: "localhost by name", host: "localhost", want: true},
		{name: "LOCALHOST is the same name", host: "LOCALHOST", want: true},
		{name: "a routable address", host: "8.8.8.8", want: false},
		{name: "a name that may resolve anywhere", host: "example.com", want: false},
		{name: "empty", host: "", want: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			got := isLoopbackHost(tc.host)

			// Assert.
			if got != tc.want {
				t.Fatalf("isLoopbackHost(%q) = %v, want %v", tc.host, got, tc.want)
			}
		})
	}
}
