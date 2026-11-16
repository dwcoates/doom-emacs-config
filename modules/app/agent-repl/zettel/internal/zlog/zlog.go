// Package zlog is zettel's one logging helper: zettel's records are written
// by the shared record writer (agentrepl/logging/recordlog) under the
// runtime "zettel".
package zlog

import (
	"io"
	"time"

	sharedlogging "agentrepl/logging"
	"agentrepl/logging/recordlog"
)

// Runtime is the name every zettel record carries in its `runtime` field.
const Runtime = "zettel"

// Context is a record's structured fields.
type Context = recordlog.Context

// Logger writes zettel's records.
type Logger = recordlog.Logger

// New builds zettel's Logger (recordlog.New under Runtime).
func New(sink, terminal io.Writer, window *sharedlogging.Window, clock func() time.Time) *Logger {
	return recordlog.New(Runtime, sink, terminal, window, clock)
}
