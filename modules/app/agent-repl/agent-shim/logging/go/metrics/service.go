package metrics

import (
	"fmt"

	"github.com/prometheus/client_golang/prometheus"
)

// Record is a launchd service's canonical logger, as its metrics surface
// records its decisions through it: LEVEL is "debug", "info" or "error", and
// MESSAGE the whole record text. Each service adapts its own logger to it.
type Record func(level, message string)

// OpenService opens and advertises a launchd service's metrics surface
// (OpenSelf) and records the decision either way through record: off (no
// root) at debug, listening at info, and a surface that cannot open at error,
// returned as the boot error it is. Every later failure (a scrape, the
// listener ending) is recorded at error too. Its registry carries every extra
// collector the service owns.
func OpenService(program, root string, record Record, extra ...prometheus.Collector) (*Server, error) {
	return openService(program, root, record, OpenSelf, extra...)
}

func openService(program, root string, record Record, open func(program, root string, onError func(op string, err error), extra ...prometheus.Collector) (*Server, error), extra ...prometheus.Collector) (*Server, error) {
	surface, err := open(program, root, func(op string, err error) {
		record("error", fmt.Sprintf("the metrics surface failed step=%s: %v", op, err))
	}, extra...)
	if err != nil {
		record("error", fmt.Sprintf("the metrics surface could not open root=%s: %v", root, err))
		return nil, err
	}
	if surface == nil {
		record("debug", "the metrics surface is off flag=--metrics-root")
		return nil, nil
	}
	record("info", fmt.Sprintf("the metrics surface is listening address=%s target=%s", surface.Addr(), surface.TargetPath()))
	return surface, nil
}

// CloseService stops an open service surface, recording a failure to stop
// at error. A nil surface (off) closes as a no-op.
func CloseService(surface *Server, record Record) {
	if surface == nil {
		return
	}
	if err := surface.Close(); err != nil {
		record("error", fmt.Sprintf("the metrics surface did not stop cleanly: %v", err))
	}
}
