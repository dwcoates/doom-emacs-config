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
	"sync/atomic"

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
	// root is the metrics root the surface advertises under, which the merge
	// queue hands its test runs so they push their timings to the same store.
	root      string
	srv       *metrics.Server
	log       dlog.Logger
	version   *deployedVersion
	latencies *latencies
	restarts  *restarts
	rpcs      *prometheus.HistogramVec
	records   *prometheus.CounterVec
	merges    merges
	cache     cacheTelemetry
	shimLife  shimLifeTelemetry
	// processes is the registered process collector, whose latest scrape the
	// rail's memory popups read (Latest); nil until RegisterProcesses.
	processes atomic.Pointer[procmetrics.Collector]
	// emacs is the Emacs whose WatchDaemon stream stands, which the process
	// collector measures.
	emacs *procmetrics.EmacsProcess
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
	cache := newCacheTelemetry()
	life := newShimLifeTelemetry()
	srv, err := metrics.OpenRunning(metrics.ProgramDaemon, root, func(op string, err error) {
		log.Error(Operation, "the metrics surface failed", dlog.Context{"step": op, "error": err.Error()})
	}, version, lat, rst, rpcs, records, mrg.steps, mrg.spans, cache.lateness, cache.lapses, cache.coldIdle, life.starts, life.standDowns, life.gateAnswers)
	if err != nil {
		return nil, fmt.Errorf("runtimemetrics: %w", err)
	}
	log.Info(Operation, "the metrics surface is listening", dlog.Context{
		"address": srv.Addr(),
		"target":  srv.TargetPath(),
	})
	return &Surface{root: root, srv: srv, log: log, version: version, latencies: lat, restarts: rst, rpcs: rpcs, records: records, merges: mrg, cache: cache, shimLife: life,
		emacs: procmetrics.NewEmacsProcess(log)}, nil
}

// Root is the metrics root the surface was opened under; the off surface's
// is empty.
func (s *Surface) Root() string {
	if s == nil {
		return ""
	}
	return s.root
}

// AttachEmacs measures the Emacs of pid while the WatchDaemon stream that
// stated it stands, answering the release the stream calls when it ends. The
// off surface measures nothing.
func (s *Surface) AttachEmacs(pid int) (release func()) {
	if s == nil {
		return func() {}
	}
	return s.emacs.Attach(pid)
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
		Reader: reader, Self: os.Getpid(), Emacs: s.emacs, SockDir: sockDir, DiskPath: diskPath, OnBattery: onBattery, Log: s.log,
	})
	if err != nil {
		return fmt.Errorf("runtimemetrics: %w", err)
	}
	if err := s.srv.Registry().Register(c); err != nil {
		return fmt.Errorf("runtimemetrics: register the process collector: %w", err)
	}
	s.processes.Store(c)
	return nil
}

// Latest answers the process collector's latest scrape, false while none has
// run: before the metrics store's first scrape, with no collector registered,
// and on the off surface, which measures nothing.
func (s *Surface) Latest() (procmetrics.Snapshot, bool) {
	if s == nil {
		return procmetrics.Snapshot{}, false
	}
	c := s.processes.Load()
	if c == nil {
		return procmetrics.Snapshot{}, false
	}
	return c.Latest()
}
