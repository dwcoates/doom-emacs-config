package metrics

import (
	"github.com/prometheus/client_golang/prometheus"

	"agentrepl/logging/logsite"
)

// Record is a launchd service's canonical logger, as its metrics surface
// records its decisions through it: LEVEL is "debug", "info" or "error",
// MESSAGE the record's template, keyed here where it is written, and VALUES
// the named values its placeholders take, which land in the record's context.
// Each service adapts its own logger's forwarded-message path to it.
type Record func(level string, message logsite.Message, values map[string]any)

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
		record("error", logsite.Msg("the metrics surface failed step={step}: {cause}"), map[string]any{"step": op, "cause": err.Error()})
	}, extra...)
	if err != nil {
		record("error", logsite.Msg("the metrics surface could not open root={root}: {cause}"), map[string]any{"root": root, "cause": err.Error()})
		return nil, err
	}
	if surface == nil {
		record("debug", logsite.Msg("the metrics surface is off flag=--metrics-root"), nil)
		return nil, nil
	}
	record("info", logsite.Msg("the metrics surface is listening address={address} target={target}"), map[string]any{"address": surface.Addr(), "target": surface.TargetPath()})
	return surface, nil
}

// CloseService stops an open service surface, recording a failure to stop
// at error. A nil surface (off) closes as a no-op.
func CloseService(surface *Server, record Record) {
	if surface == nil {
		return
	}
	if err := surface.Close(); err != nil {
		record("error", logsite.Msg("the metrics surface did not stop cleanly: {cause}"), map[string]any{"cause": err.Error()})
	}
}
