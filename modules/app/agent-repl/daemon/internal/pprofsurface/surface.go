package pprofsurface

import (
	"context"
	"errors"
	"fmt"
	"net"
	"net/http"
	"net/http/pprof"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"time"

	"claude-repld/internal/dlog"
)

// ErrNotLocal reports a refused bind: the profiling surface exposes goroutine
// dumps, heap contents and the command line of a process that holds the
// operator's source tree, so it is reachable from this machine or not at all.
var ErrNotLocal = errors.New("the profiling surface binds only a unix socket or an explicitly loopback host:port")

// shutdownGrace bounds Close. The surface serves an operator, not the
// product; an in-flight profile must not hold up the daemon's exit.
const shutdownGrace = 2 * time.Second

// surface is the Surface implementation.
type surface struct {
	network string
	address string
	url     string
	ln      net.Listener
	srv     *http.Server
	// socketPath is the unix socket to unlink on Close, empty for tcp.
	socketPath string
}

// Network implements Surface.
func (s *surface) Network() string { return s.network }

// Address implements Surface.
func (s *surface) Address() string { return s.address }

// URL implements Surface.
func (s *surface) URL() string { return s.url }

// Close implements Surface.
func (s *surface) Close() error {
	ctx, cancel := context.WithTimeout(context.Background(), shutdownGrace)
	defer cancel()
	err := s.srv.Shutdown(ctx)
	if s.socketPath != "" {
		if rmErr := os.Remove(s.socketPath); rmErr != nil && !os.IsNotExist(rmErr) && err == nil {
			err = fmt.Errorf("remove the profiling socket %q: %w", s.socketPath, rmErr)
		}
	}
	if err != nil {
		return fmt.Errorf("stop the profiling surface at %q: %w", s.address, err)
	}
	return nil
}

// open is Open's body.
//
// The caller opens this BEFORE booting any dependency, so a boot that wedges
// on a dependency is still diagnosable through it — that ordering is the
// caller's to keep, and it is the whole reason the surface exists.
func open(ctx context.Context, addr string, log dlog.Logger) (Surface, error) {
	if addr == "" {
		log.Debug("daemon.pprof.disabled",
			"the profiling surface is off; no listener was opened",
			dlog.Context{"addr": addr})
		return nil, nil
	}
	network, address, err := resolve(addr)
	if err != nil {
		log.Error("daemon.pprof.refused",
			"the profiling surface was refused a non-local bind",
			dlog.Context{"addr": addr, "cause": err.Error()})
		return nil, err
	}
	ln, err := net.Listen(network, address)
	if err != nil {
		wrapped := fmt.Errorf("open the profiling surface on %s %s: %w", network, address, err)
		log.Error("daemon.pprof.open_failed",
			"the profiling surface could not bind",
			dlog.Context{"network": network, "address": address, "cause": err.Error()})
		return nil, wrapped
	}
	s := &surface{network: network, ln: ln}
	if network == "unix" {
		s.address = address
		s.socketPath = address
		s.url = "http+unix://" + address + "/debug/pprof/"
	} else {
		s.address = ln.Addr().String()
		s.url = "http://" + s.address + "/debug/pprof/"
	}
	s.srv = &http.Server{
		Handler:     mux(),
		BaseContext: func(net.Listener) context.Context { return ctx },
	}
	go func() {
		// ErrServerClosed is the ordinary end of Close; nothing else can be
		// reported from here, so the surface's failure is its listener's.
		if err := s.srv.Serve(ln); err != nil && !errors.Is(err, http.ErrServerClosed) {
			log.Error("daemon.pprof.serve_failed",
				"the profiling surface stopped serving",
				dlog.Context{"network": s.network, "address": s.address, "cause": err.Error()})
		}
	}()
	// WARN, not INFO: an open profiling surface is a deliberate, temporary,
	// operator-visible state, and a daemon that still has one open tomorrow
	// should say so loudly enough to be noticed.
	log.Warn("daemon.pprof.enabled",
		"the profiling surface is open",
		dlog.Context{"network": s.network, "address": s.address, "url": s.url})
	return s, nil
}

// mux serves net/http/pprof on its own mux rather than through
// http.DefaultServeMux, so importing the package cannot leak the profiling
// handlers onto the daemon's own server.
func mux() *http.ServeMux {
	m := http.NewServeMux()
	m.HandleFunc("/debug/pprof/", pprof.Index)
	m.HandleFunc("/debug/pprof/cmdline", pprof.Cmdline)
	m.HandleFunc("/debug/pprof/profile", pprof.Profile)
	m.HandleFunc("/debug/pprof/symbol", pprof.Symbol)
	m.HandleFunc("/debug/pprof/trace", pprof.Trace)
	return m
}

// resolve classifies addr as a unix socket path or a loopback host:port, and
// refuses everything else. A wildcard bind is refused here rather than opened
// and regretted.
func resolve(addr string) (network, address string, err error) {
	if isSocketPath(addr) {
		abs, absErr := filepath.Abs(addr)
		if absErr != nil {
			return "", "", fmt.Errorf("resolve the profiling socket path %q: %w", addr, absErr)
		}
		return "unix", abs, nil
	}
	host, port, splitErr := net.SplitHostPort(addr)
	if splitErr != nil {
		return "", "", fmt.Errorf("%w: %q is neither a socket path nor host:port: %w", ErrNotLocal, addr, splitErr)
	}
	if _, convErr := strconv.Atoi(port); convErr != nil {
		return "", "", fmt.Errorf("%w: %q has a non-numeric port: %w", ErrNotLocal, addr, convErr)
	}
	if host == "" {
		return "", "", fmt.Errorf("%w: %q omits the host, which binds every interface", ErrNotLocal, addr)
	}
	if !isLoopbackHost(host) {
		return "", "", fmt.Errorf("%w: %q is not loopback", ErrNotLocal, addr)
	}
	return "tcp", net.JoinHostPort(host, port), nil
}

// isSocketPath reports whether addr names a unix socket rather than a
// host:port. A path is recognized by containing a separator or by the .sock
// suffix; "127.0.0.1:6060" has neither.
func isSocketPath(addr string) bool {
	return strings.ContainsRune(addr, filepath.Separator) || strings.HasSuffix(addr, ".sock")
}

// isLoopbackHost reports whether a host binds only this machine. "localhost"
// is accepted by name because that is how an operator types it; anything that
// does not parse as an IP and is not localhost is refused rather than
// resolved, since a name can resolve to a routable address.
func isLoopbackHost(host string) bool {
	if strings.EqualFold(host, "localhost") {
		return true
	}
	ip := net.ParseIP(host)
	if ip == nil {
		return false
	}
	return ip.IsLoopback()
}
