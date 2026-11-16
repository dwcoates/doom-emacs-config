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
		ctx := recordlog.Context{"socket": path, "cause": err.Error()}
		if errors.As(err, &step) {
			ctx["step"] = string(step.Step)
		}
		log.Error(runtime+".listen", "binding the "+runtime+" socket failed", ctx)
		return nil, err
	}
	if res.Reclaimed {
		log.Warn(runtime+".listen.reclaim", "reclaimed a stale socket a previous "+runtime+" left behind", recordlog.Context{"socket": res.Path})
	}
	log.Info(runtime+".listen", "listening on the "+runtime+" socket", recordlog.Context{"socket": res.Path})
	return ln, nil
}
