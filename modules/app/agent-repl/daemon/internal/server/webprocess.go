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
	// Observe takes one stall: the web view document that recorded it
	// (`webhost_document`), when it did, and how long that document's web
	// content process's main thread was blocked.
	Observe(document string, at time.Time, blockedMS int64)
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
// stall record whose document, moment or duration cannot be read is recorded
// at ERROR and observed no further: the webhost writes all three on every
// stall, so any missing is a defect in the record.
func (s *server) observeWebhostStall(record *agentreplv1.ClientLogRecord) {
	if record.GetOperation() != webhostStallOperation {
		return
	}
	document, at, blocked, err := stallOf(record)
	if err != nil {
		s.log.Error("daemon.server.webhost_stall_unreadable", "a webhost stall record could not be read; the stall beat did not see it",
			dlog.Context{"cause": err.Error(), "timestamp": record.GetTimestamp()})
		return
	}
	s.deps.StallBeat.Observe(document, at, blocked)
}

// stallOf answers a stall record's web view document, its moment and its
// blocked milliseconds.
func stallOf(record *agentreplv1.ClientLogRecord) (string, time.Time, int64, error) {
	fields := record.GetContext().AsMap()
	document, ok := fields["webhost_document"].(string)
	if !ok || document == "" {
		return "", time.Time{}, 0, fmt.Errorf("webhost_document is %#v, not a document's name", fields["webhost_document"])
	}
	at, err := time.Parse(time.RFC3339Nano, record.GetTimestamp())
	if err != nil {
		return "", time.Time{}, 0, fmt.Errorf("timestamp %q: %w", record.GetTimestamp(), err)
	}
	blocked, ok := fields["blocked_ms"].(float64)
	if !ok {
		return "", time.Time{}, 0, fmt.Errorf("blocked_ms is %#v, not a number", fields["blocked_ms"])
	}
	return document, at, int64(blocked), nil
}
