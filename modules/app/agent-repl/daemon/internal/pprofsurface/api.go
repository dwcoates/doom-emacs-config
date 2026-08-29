// Package pprofsurface is the daemon's OPT-IN Go profiling listener.
//
// Empty is OFF and is the default: there is no always-on listener, and a
// wildcard or routable bind is refused at construction. The decision is
// recorded either way. See daemon/AGENTS.md "Telemetry" (`-pprof`).
package pprofsurface

import (
	"context"

	"claude-repld/internal/dlog"
)

// Surface is a running profiling listener.
type Surface interface {
	// Network is the resolved network ("unix" or "tcp").
	Network() string
	// Address is the resolved address. For a port-0 bind this is the only
	// place the chosen port appears.
	Address() string
	// URL is the browsable profiling root.
	URL() string
	// Close stops the listener.
	Close() error
}

// Open starts the profiling surface at addr, which is either a unix socket
// path or an explicitly loopback host:port. An empty addr is OFF: Open returns
// a nil Surface and a nil error, having recorded daemon.pprof.disabled. A
// wildcard or routable bind is refused here rather than opened; the enabled
// case records daemon.pprof.enabled at WARN with the resolved network,
// address and url.
// The caller opens this BEFORE booting any dependency, so a boot wedged on a
// dependency is still diagnosable through it.
func Open(ctx context.Context, addr string, log dlog.Logger) (Surface, error) {
	return open(ctx, addr, log)
}
