package main

import (
	"context"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agentrepl/logging/logsite"
)

// rosterWrite is the one shape of a roster sink that keeps a change durable:
// it writes each change through WRITE and records a write that fails at ERROR
// under OP with MESSAGE, naming the workspace, the cause, and the change's own
// FIELDS. A failed write changes nothing the roster draws, which already reads
// the live value; only the next daemon's restore misses it.
func rosterWrite[V any](
	log dlog.Logger,
	op string,
	message logsite.Message,
	write func(ctx context.Context, ws ids.WorkspaceID, v V) error,
	fields func(v V) dlog.Context,
) func(ws ids.WorkspaceID, v V) {
	return func(ws ids.WorkspaceID, v V) {
		if err := write(context.Background(), ws, v); err != nil {
			ctx := fields(v)
			ctx["workspace"], ctx["cause"] = string(ws), err.Error()
			log.Log(dlog.LevelError, op, message, ctx)
		}
	}
}
