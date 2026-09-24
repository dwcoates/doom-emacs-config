package main

import (
	"errors"
	"net/http"

	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/pprofsurface"
)

// openPprofSurface opens the OPT-IN profiling surface and starts serving it.
//
// An empty addr is the shipped state: no listener exists, and that is recorded
// at debug. An enabled surface is recorded ONCE at warn, because a listener
// exposing goroutine stacks, the command line and heap contents is never
// routine. An unsafe or unbindable addr is returned as an error, never served.
func openPprofSurface(addr string, logf *logging.Bound) (*pprofsurface.Surface, error) {
	surface, err := pprofsurface.Open(addr)
	if err != nil {
		return nil, err
	}
	if surface == nil {
		logf.With(logging.Context{Operation: "pprof.disabled", Level: "debug"}).LogVerbose(
			"Go profiling surface is off env=%s flag=--pprof", pprofsurface.EnvAddr)
		return nil, nil
	}
	logf.With(logging.Context{Operation: "pprof.enabled", Level: "warn"}).Log(
		"Go profiling surface is LISTENING; it exposes goroutine stacks, the command line and heap contents network=%s address=%s url=%s env=%s",
		surface.Network(), surface.Address(), surface.URL(), pprofsurface.EnvAddr)
	go func() {
		if serveErr := surface.Serve(); serveErr != nil && !errors.Is(serveErr, http.ErrServerClosed) {
			logf.With(logging.Context{Operation: "pprof.serve", Level: "error"}).Log("pprof surface serve ended: %v", serveErr)
		}
	}()
	return surface, nil
}

// closePprofSurface stops the surface, recording a failed close rather than
// dropping it. A nil surface (the capability off) closes as a no-op.
func closePprofSurface(surface *pprofsurface.Surface, logf *logging.Bound) {
	if err := surface.Close(); err != nil {
		logf.With(logging.Context{Operation: "pprof.close", Level: "error"}).Log("closing pprof surface failed: %v", err)
	}
}
