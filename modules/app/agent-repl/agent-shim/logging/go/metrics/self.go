package metrics

import (
	"fmt"

	"agentrepl/logging/buildreport"
)

// OpenSelf opens AND advertises the metrics surface of a runtime that owns
// its role from boot: the launchd services (store, sidecar), which launchd
// never runs twice at once. Its registry carries the content hash of the
// running binary. An empty root answers (nil, nil): the surface is off.
//
// The daemon does not use it: a joining daemon must not advertise until it
// owns its workspaces, so it opens and advertises in two steps.
func OpenSelf(program, root string, onError func(op string, err error)) (*Server, error) {
	return openSelf(program, root, onError, buildreport.Self)
}

func openSelf(program, root string, onError func(op string, err error), self func() (buildreport.Report, error)) (*Server, error) {
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
	s, err := Open(Config{Program: program, Root: root, Registry: reg, OnError: onError})
	if err != nil {
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
