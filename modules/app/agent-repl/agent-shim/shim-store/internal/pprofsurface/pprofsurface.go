// Package pprofsurface owns shim-store's OPT-IN, LOCAL-ONLY Go profiling
// surface.
//
// There is deliberately no always-on listener. A profiling endpoint exposes
// goroutine stacks, command lines and heap contents, so it exists only when an
// operator asked for it by name, and only on a surface nothing off this machine
// can reach: a unix domain socket, or an explicitly loopback TCP address.
// Anything else — a wildcard bind, a routable host — is refused at construction
// rather than served and hoped about.
package pprofsurface

import (
	"errors"
	"fmt"
	"net"
	"net/http"
	"net/http/pprof"
	"os"
	"path/filepath"
	"strings"
	"time"
)

// EnvAddr is the environment variable the surface's address defaults from.
// Unset or empty means the capability is OFF, which is the shipped state.
//
// It is store-specific rather than the daemon's variable: both services are
// long-lived on one machine, and a single shared variable would silently open
// two profiling listeners — the second of which would fail to bind the first's
// socket — whenever an operator meant to profile just one of them.
const EnvAddr = "AGENT_REPL_STORE_PPROF_ADDR"

// Path is the mount point every profile hangs off, matching net/http/pprof's
// own convention so `go tool pprof` URLs need no translation.
const Path = "/debug/pprof/"

// loopbackHosts are the only TCP hosts a profiling listener may bind. A
// wildcard bind (an empty host, "0.0.0.0", "::") is NOT among them: it would
// publish the store's stacks and heap to the network.
var loopbackHosts = map[string]bool{"127.0.0.1": true, "::1": true, "localhost": true}

// Surface is a running profiling listener.
type Surface struct {
	network  string
	address  string
	listener net.Listener
	server   *http.Server
}

// Open binds the profiling surface described by addr.
//
// An empty addr returns (nil, nil): the capability is off, which is not an
// error and is the default. An addr containing a path separator is a unix
// domain socket path; anything else is a TCP address whose host must be
// loopback.
func Open(addr string) (*Surface, error) {
	if addr == "" {
		return nil, nil
	}
	if strings.ContainsRune(addr, os.PathSeparator) {
		return openUnix(addr)
	}
	return openTCP(addr)
}

func openUnix(path string) (*Surface, error) {
	absolute, err := filepath.Abs(path)
	if err != nil {
		return nil, fmt.Errorf("pprofsurface: resolve socket path %q: %w", path, err)
	}
	if err := os.MkdirAll(filepath.Dir(absolute), 0o700); err != nil {
		return nil, fmt.Errorf("pprofsurface: prepare socket directory for %q: %w", absolute, err)
	}
	// A LEFTOVER SOCKET IS RECLAIMED; ANYTHING ELSE IS REFUSED. A previous
	// store that died holding this path leaves a stale socket which would
	// otherwise make the capability permanently unusable — but unlinking
	// whatever happens to be at an operator-supplied path is how a profiling
	// knob deletes somebody's file, so the mode is proved first.
	switch info, statErr := os.Lstat(absolute); {
	case statErr == nil && info.Mode()&os.ModeSocket != 0:
		if err := os.Remove(absolute); err != nil {
			return nil, fmt.Errorf("pprofsurface: remove stale socket %q: %w", absolute, err)
		}
	case statErr == nil:
		return nil, fmt.Errorf("pprofsurface: refusing to replace non-socket %q (mode %s)", absolute, info.Mode())
	case !errors.Is(statErr, os.ErrNotExist):
		return nil, fmt.Errorf("pprofsurface: inspect socket path %q: %w", absolute, statErr)
	}
	listener, err := net.Listen("unix", absolute)
	if err != nil {
		return nil, fmt.Errorf("pprofsurface: listen on unix socket %q: %w", absolute, err)
	}
	if err := os.Chmod(absolute, 0o600); err != nil {
		listener.Close()
		return nil, fmt.Errorf("pprofsurface: restrict socket %q to its owner: %w", absolute, err)
	}
	return newSurface("unix", absolute, listener), nil
}

func openTCP(addr string) (*Surface, error) {
	host, port, err := net.SplitHostPort(addr)
	if err != nil {
		return nil, fmt.Errorf("pprofsurface: %q is neither a socket path nor a host:port address: %w", addr, err)
	}
	if !loopbackHosts[host] {
		return nil, fmt.Errorf("pprofsurface: refusing to serve profiles on non-loopback host %q (use 127.0.0.1, ::1, or a unix socket path); a profiling endpoint exposes goroutine stacks, the command line and heap contents", host)
	}
	listener, err := net.Listen("tcp", net.JoinHostPort(host, port))
	if err != nil {
		return nil, fmt.Errorf("pprofsurface: listen on %q: %w", addr, err)
	}
	return newSurface("tcp", listener.Addr().String(), listener), nil
}

func newSurface(network, address string, listener net.Listener) *Surface {
	return &Surface{
		network:  network,
		address:  address,
		listener: listener,
		// A private mux, never http.DefaultServeMux: importing net/http/pprof
		// registers on the default mux as a side effect, and a process that
		// serves anything else off that mux would publish profiles it never
		// mounted.
		server: &http.Server{Handler: Handler(), ReadHeaderTimeout: 10 * time.Second},
	}
}

// Handler is the profiling mux: index, cmdline, profile, symbol and trace, plus
// the named runtime profiles the index links to.
func Handler() http.Handler {
	mux := http.NewServeMux()
	mux.HandleFunc(Path, pprof.Index)
	mux.HandleFunc(Path+"cmdline", pprof.Cmdline)
	mux.HandleFunc(Path+"profile", pprof.Profile)
	mux.HandleFunc(Path+"symbol", pprof.Symbol)
	mux.HandleFunc(Path+"trace", pprof.Trace)
	return mux
}

// Network is "unix" or "tcp".
func (s *Surface) Network() string { return s.network }

// Address is the resolved socket path or host:port. For a TCP surface bound on
// port 0 this is the port the kernel actually chose, which is what makes the
// startup record it is logged in usable.
func (s *Surface) Address() string { return s.address }

// URL is the base a `go tool pprof` invocation targets. A unix surface has no
// URL form, so it reports the socket path its client must dial instead.
func (s *Surface) URL() string {
	if s.network == "unix" {
		return "unix://" + s.address + Path
	}
	return "http://" + s.address + Path
}

// Serve runs the listener until Close. It returns the serve error, which is
// http.ErrServerClosed after an ordinary Close.
func (s *Surface) Serve() error {
	if s == nil {
		return errors.New("pprofsurface: Serve called on an unopened surface")
	}
	return s.server.Serve(s.listener)
}

// Close stops the listener and removes a unix socket it created.
func (s *Surface) Close() error {
	if s == nil {
		return nil
	}
	err := s.server.Close()
	if s.network == "unix" {
		if removeErr := os.Remove(s.address); removeErr != nil && !errors.Is(removeErr, os.ErrNotExist) {
			return errors.Join(err, fmt.Errorf("pprofsurface: remove socket %q: %w", s.address, removeErr))
		}
	}
	return err
}
