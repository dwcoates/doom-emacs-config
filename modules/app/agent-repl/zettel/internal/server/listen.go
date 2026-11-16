package server

import (
	"net"
	"time"

	"agentrepl/logging/singletonsocket"
	"agentrepl/zettel/internal/zlog"
)

// probeTimeout bounds the dial that decides whether an existing socket has a
// live zettel behind it.
const probeTimeout = 2 * time.Second

// Listen binds zettel's singleton socket, logging the outcome
// (singletonsocket.ListenRecorded): zettel is a singleton and the socket is
// its token, so a live listener is never stolen and a stale one is
// reclaimed.
func Listen(path string, log *zlog.Logger) (net.Listener, error) {
	return singletonsocket.ListenRecorded(path, probeTimeout, log)
}
