package metrics

import (
	"fmt"

	"agentrepl/logging/buildreport"

	"github.com/prometheus/client_golang/prometheus"
)

// OpenRunning opens, WITHOUT advertising, the metrics surface of the running
// binary: its registry carries the binary's content hash and every extra
// collector the runtime owns. An empty root answers (nil, nil): the surface is
// off. The daemon advertises later, once it owns its workspaces.
func OpenRunning(program, root string, onError func(op string, err error), extra ...prometheus.Collector) (*Server, error) {
	return openRunning(program, root, onError, buildreport.Self, extra...)
}

// OpenSelf opens AND advertises the metrics surface of a runtime that owns
// its role from boot: the launchd services (store, sidecar), which launchd
// never runs twice at once.
func OpenSelf(program, root string, onError func(op string, err error)) (*Server, error) {
	return openSelf(program, root, onError, buildreport.Self)
}

func openRunning(program, root string, onError func(op string, err error), self func() (buildreport.Report, error), extra ...prometheus.Collector) (*Server, error) {
	if root == "" {
		return nil, nil
	}
	report, err := self()
	if err != nil {
		return nil, fmt.Errorf("metrics: determine the running build of %q: %w", program, err)
	}
	reg, err := NewRegistry(program, report.Build)
	if err != nil {
		return nil, err
	}
	for _, c := range extra {
		if err := reg.Register(c); err != nil {
			return nil, fmt.Errorf("metrics: register a collector for %q: %w", program, err)
		}
	}
	return Open(Config{Program: program, Root: root, Registry: reg, OnError: onError})
}

func openSelf(program, root string, onError func(op string, err error), self func() (buildreport.Report, error)) (*Server, error) {
	s, err := openRunning(program, root, onError, self)
	if err != nil || s == nil {
		return nil, err
	}
	if err := s.Advertise(); err != nil {
		if closeErr := s.Close(); closeErr != nil {
			return nil, fmt.Errorf("%w (and closing the listener failed: %v)", err, closeErr)
		}
		return nil, err
	}
	return s, nil
}
