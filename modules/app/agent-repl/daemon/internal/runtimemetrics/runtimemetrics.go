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
	"os"

	"agent-repld/internal/dlog"
	"agent-repld/internal/procmetrics"

	"agentrepl/logging/metrics"

	"github.com/prometheus/client_golang/prometheus"
)

// Operation is the canonical log operation of every record this package
// writes.
const Operation = "daemon.metrics"

// Surface is the daemon's open metrics surface.
type Surface struct {
	srv       *metrics.Server
	log       dlog.Logger
	version   *deployedVersion
	latencies *latencies
	restarts  *restarts
	rpcs      *prometheus.HistogramVec
	records   *prometheus.CounterVec
	merges    merges
}

// Open opens, without advertising, the daemon's metrics surface under root.
// An empty root is OFF and answers (nil, nil); every method of a nil Surface
// is the off surface's.
func Open(log dlog.Logger, root string) (*Surface, error) {
	if root == "" {
		log.Info(Operation, "the metrics surface is off: no metrics root was configured", nil)
		return nil, nil
	}
	version := newDeployedVersion()
	lat := newLatencies(log)
	rst := newRestarts()
	rpcs := newRPCs()
	records := newRecords()
	mrg := newMerges()
	srv, err := metrics.OpenRunning(metrics.ProgramDaemon, root, func(op string, err error) {
		log.Error(Operation, "the metrics surface failed", dlog.Context{"step": op, "error": err.Error()})
	}, version, lat, rst, rpcs, records, mrg.steps, mrg.spans)
	if err != nil {
		return nil, fmt.Errorf("runtimemetrics: %w", err)
	}
	log.Info(Operation, "the metrics surface is listening", dlog.Context{
		"address": srv.Addr(),
		"target":  srv.TargetPath(),
	})
	return &Surface{srv: srv, log: log, version: version, latencies: lat, restarts: rst, rpcs: rpcs, records: records, merges: mrg}, nil
}

// Registry answers the registry every daemon collector registers on, nil
// when the surface is off.
func (s *Surface) Registry() *prometheus.Registry {
	if s == nil {
		return nil
	}
	return s.srv.Registry()
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

// RegisterProcesses registers the process and host collector
// (procmetrics) for this daemon, whose shims listen in sockDir and whose
// state root is on diskPath's volume. The off surface registers nothing.
func (s *Surface) RegisterProcesses(reader procmetrics.Reader, sockDir, diskPath string, onBattery func() (bool, error)) error {
	if s == nil {
		return nil
	}
	c, err := procmetrics.New(procmetrics.Deps{
		Reader: reader, Self: os.Getpid(), SockDir: sockDir, DiskPath: diskPath, OnBattery: onBattery, Log: s.log,
	})
	if err != nil {
		return fmt.Errorf("runtimemetrics: %w", err)
	}
	if err := s.srv.Registry().Register(c); err != nil {
		return fmt.Errorf("runtimemetrics: register the process collector: %w", err)
	}
	return nil
}
