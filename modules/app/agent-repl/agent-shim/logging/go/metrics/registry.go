package metrics

import (
	"fmt"

	"github.com/prometheus/client_golang/prometheus"
	"github.com/prometheus/client_golang/prometheus/collectors"
)

// The programs that expose a metrics surface. The set is closed: a target
// file is named for its program and carries it as its `instance` label, so an
// unknown name would mint a target nothing queries for.
const (
	ProgramDaemon  = "daemon"
	ProgramStore   = "store"
	ProgramSidecar = "sidecar"
)

// Programs lists every program in the closed set, in a stable order.
var Programs = []string{ProgramDaemon, ProgramStore, ProgramSidecar}

// BuildInfoName is the series every runtime exposes with value 1, labeled by
// its program and the content hash of the binary it runs. Grouping any other
// series by build joins on it.
const BuildInfoName = "agentrepl_build_info"

// ValidProgram reports whether program is in the closed set.
func ValidProgram(program string) bool {
	for _, p := range Programs {
		if p == program {
			return true
		}
	}
	return false
}

// NewRegistry answers a registry carrying the Go runtime collector and the
// program's build info series. build is the binary's content hash
// (buildid); an empty build or an unknown program is a caller bug.
func NewRegistry(program, build string) (*prometheus.Registry, error) {
	if !ValidProgram(program) {
		return nil, fmt.Errorf("metrics: unknown program %q (want one of %v)", program, Programs)
	}
	if build == "" {
		return nil, fmt.Errorf("metrics: program %q has no build hash", program)
	}
	reg := prometheus.NewRegistry()
	info := prometheus.NewGauge(prometheus.GaugeOpts{
		Name:        BuildInfoName,
		Help:        "Always 1. Labels name the program and the content hash of the binary it runs.",
		ConstLabels: prometheus.Labels{"program": program, "build": build},
	})
	info.Set(1)
	for _, c := range []prometheus.Collector{collectors.NewGoCollector(), info} {
		if err := reg.Register(c); err != nil {
			return nil, fmt.Errorf("metrics: register a collector for %q: %w", program, err)
		}
	}
	return reg, nil
}
