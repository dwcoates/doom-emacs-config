package server

import (
	"fmt"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// StallBeat watches the webhost's main-thread stalls for WebKit's
// memory-pressure beat (internal/stallbeat).
type StallBeat interface {
	// Observe takes one stall: when the webhost recorded it and how long the
	// web content process's main thread was blocked.
	Observe(at time.Time, blockedMS int64)
}

// SupersededPages raises a page that a newer page replaced and that goes on
// subscribing (internal/pagesupersede).
type SupersededPages interface {
	// Served records a page's accepted subscription naming a workspace.
	Served(page string, ws ids.WorkspaceID)
}

// webhostStallOperation is the webhost's record of one main-thread stall
// (webapp/src/host/load-census.ts).
const webhostStallOperation = "webhost.stall"

// observeWebhostStall hands a webhost's stall record to the beat detector. A
// stall record whose moment or duration cannot be read is recorded at ERROR
// and observed no further: the webhost writes both on every stall, so either
// missing is a defect in the record.
func (s *server) observeWebhostStall(record *agentreplv1.ClientLogRecord) {
	if record.GetOperation() != webhostStallOperation {
		return
	}
	at, blocked, err := stallOf(record)
	if err != nil {
		s.log.Error("daemon.server.webhost_stall_unreadable", "a webhost stall record could not be read; the stall beat did not see it",
			dlog.Context{"cause": err.Error(), "timestamp": record.GetTimestamp()})
		return
	}
	s.deps.StallBeat.Observe(at, blocked)
}

// stallOf answers a stall record's moment and its blocked milliseconds.
func stallOf(record *agentreplv1.ClientLogRecord) (time.Time, int64, error) {
	at, err := time.Parse(time.RFC3339Nano, record.GetTimestamp())
	if err != nil {
		return time.Time{}, 0, fmt.Errorf("timestamp %q: %w", record.GetTimestamp(), err)
	}
	blocked, ok := record.GetContext().AsMap()["blocked_ms"].(float64)
	if !ok {
		return time.Time{}, 0, fmt.Errorf("blocked_ms is %#v, not a number", record.GetContext().AsMap()["blocked_ms"])
	}
	return at, int64(blocked), nil
}
