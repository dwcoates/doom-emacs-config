package main

import (
	"context"
	"errors"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
	"agentrepl/logging/logsite"
)

// rosterWrite is the one shape of a roster sink that keeps a change durable:
// it writes each change through WRITE and records a write that fails at ERROR
// under OP with MESSAGE, naming the workspace, the cause, and the change's own
// FIELDS. A failed write changes nothing the roster draws, which already reads
// the live value; only the next daemon's restore misses it.
//
// A WORKSPACE FORGOTTEN BEFORE ITS WRITE RAN IS NO FAILURE. The writes are made
// off the roster's callers (rosterWrites), so a change the roster reported just
// before the workspace was forgotten reaches the state database after its row
// is gone (wsm.ErrNotFound): no daemon will restore that workspace, so nothing
// is lost. It is recorded at INFO with the same fields.
func rosterWrite[V any](
	log dlog.Logger,
	op string,
	message logsite.Message,
	write func(ctx context.Context, ws ids.WorkspaceID, v V) error,
	fields func(v V) dlog.Context,
) func(ws ids.WorkspaceID, v V) {
	return func(ws ids.WorkspaceID, v V) {
		err := write(context.Background(), ws, v)
		if err == nil {
			return
		}
		ctx := fields(v)
		ctx["workspace"], ctx["cause"] = string(ws), err.Error()
		if errors.Is(err, wsm.ErrNotFound) {
			log.Info(op, "the workspace was forgotten before its change was kept durable; no daemon restores it, so nothing is lost", ctx)
			return
		}
		log.Log(dlog.LevelError, op, message, ctx)
	}
}
