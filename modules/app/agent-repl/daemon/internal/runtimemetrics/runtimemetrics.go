// Package runtimemetrics is the daemon's always-on metrics surface
// (docs/PLAN-runtime-metrics.md): the shared agentrepl/metrics listener, the
// daemon's registry, and the collectors only the daemon owns.
//
// Every collector here runs on the scrape's own goroutine, inside the
// /metrics handler, and reads only what it can read without a lock the
// daemon's request handling takes. A slow scrape therefore never stalls a
// prompt or a switch.
package runtimemetrics

import (
	"fmt"

	"agent-repld/internal/buildid"
	"agent-repld/internal/dlog"

	"agentrepl/logging/metrics"

	"github.com/prometheus/client_golang/prometheus"
)

// Operation is the canonical log operation of every record this package
// writes.
const Operation = "daemon.metrics"

// Surface is the daemon's open metrics surface.
type Surface struct {
	reg *prometheus.Registry
	srv *metrics.Server
	log dlog.Logger
}

// Open opens the daemon's metrics surface under root, hashing exe for the
// build info. An empty root is OFF and answers (nil, nil); every method of a
// nil Surface is the off surface's.
func Open(log dlog.Logger, root, exe string) (*Surface, error) {
	if root == "" {
		log.Info(Operation, "the metrics surface is off: no metrics root was configured", nil)
		return nil, nil
	}
	build, err := buildid.File(exe)
	if err != nil {
		return nil, fmt.Errorf("runtimemetrics: hash the daemon binary %q: %w", exe, err)
	}
	reg, err := metrics.NewRegistry(metrics.ProgramDaemon, build)
	if err != nil {
		return nil, fmt.Errorf("runtimemetrics: %w", err)
	}
	if err := reg.Register(newDeployedVersionCollector(log, DeployedVersionPath(root))); err != nil {
		return nil, fmt.Errorf("runtimemetrics: register the deployed version: %w", err)
	}
	srv, err := metrics.Open(metrics.Config{
		Program:  metrics.ProgramDaemon,
		Root:     root,
		Registry: reg,
		OnError: func(op string, err error) {
			log.Error(Operation, "the metrics surface failed", dlog.Context{"step": op, "error": err.Error()})
		},
	})
	if err != nil {
		return nil, fmt.Errorf("runtimemetrics: %w", err)
	}
	log.Info(Operation, "the metrics surface is listening", dlog.Context{
		"address": srv.Addr(),
		"target":  srv.TargetPath(),
		"build":   build,
	})
	return &Surface{reg: reg, srv: srv, log: log}, nil
}

// Registry answers the registry every daemon collector registers on, nil
// when the surface is off.
func (s *Surface) Registry() *prometheus.Registry {
	if s == nil {
		return nil
	}
	return s.reg
}

// Advertise publishes the surface's target file. The daemon calls it at the
// moment it publishes daemon.addr, so the target and the address always name
// the same owner.
func (s *Surface) Advertise() error {
	if s == nil {
		return nil
	}
	if err := s.srv.Advertise(); err != nil {
		return fmt.Errorf("runtimemetrics: %w", err)
	}
	s.log.Debug(Operation, "advertised the metrics target", dlog.Context{"address": s.srv.Addr(), "target": s.srv.TargetPath()})
	return nil
}

// Close stops the listener, recording a failure to stop.
func (s *Surface) Close() {
	if s == nil {
		return
	}
	if err := s.srv.Close(); err != nil {
		s.log.Error(Operation, "the metrics surface did not stop cleanly", dlog.Context{"error": err.Error()})
	}
}
