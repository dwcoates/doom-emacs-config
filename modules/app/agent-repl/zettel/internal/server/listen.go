package server

import (
	"errors"
	"net"
	"time"

	"agentrepl/logging/singletonsocket"
	"agentrepl/zettel/internal/zlog"
)

// probeTimeout bounds the dial that decides whether an existing socket has a
// live zettel behind it.
const probeTimeout = 2 * time.Second

// Listen binds zettel's singleton socket, logging the outcome: zettel is a
// singleton and the socket is its token, so a live listener is never stolen
// and a stale one is reclaimed.
func Listen(path string, log *zlog.Logger) (net.Listener, error) {
	ln, res, err := singletonsocket.Listen(path, probeTimeout)
	if err != nil {
		var step *singletonsocket.Error
		ctx := zlog.Context{"socket": path, "cause": err.Error()}
		if errors.As(err, &step) {
			ctx["step"] = string(step.Step)
		}
		log.Error("zettel.listen", "binding the zettel socket failed", ctx)
		return nil, err
	}
	if res.Reclaimed {
		log.Warn("zettel.listen.reclaim", "reclaimed a stale socket a previous zettel left behind", zlog.Context{"socket": res.Path})
	}
	log.Info("zettel.listen", "listening on the zettel socket", zlog.Context{"socket": res.Path})
	return ln, nil
}
