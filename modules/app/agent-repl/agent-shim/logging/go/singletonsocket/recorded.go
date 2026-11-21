package singletonsocket

import (
	"errors"
	"net"
	"time"

	"agentrepl/logging/recordlog"
)

// ListenRecorded binds a standalone service's singleton socket at PATH, its
// occupancy probed within PROBE, and records the outcome in LOG under the
// log's runtime: a live listener is never stolen and a stale one is
// reclaimed. It is zettel's and the test service's one listen.
func ListenRecorded(path string, probe time.Duration, log *recordlog.Logger) (net.Listener, error) {
	runtime := log.Runtime()
	ln, res, err := Listen(path, probe)
	if err != nil {
		var step *Error
		if errors.As(err, &step) {
			log.Error(runtime+".listen", "binding the {service} socket failed", recordlog.Context{"service": runtime, "socket": path, "step": string(step.Step), "cause": err.Error()})
		} else {
			log.Error(runtime+".listen", "binding the {service} socket failed at a step it does not name", recordlog.Context{"service": runtime, "socket": path, "cause": err.Error()})
		}
		return nil, err
	}
	if res.Reclaimed {
		log.Warn(runtime+".listen.reclaim", "reclaimed a stale socket a previous {service} left behind", recordlog.Context{"service": runtime, "socket": res.Path})
	}
	log.Info(runtime+".listen", "listening on the {service} socket", recordlog.Context{"service": runtime, "socket": res.Path})
	return ln, nil
}
