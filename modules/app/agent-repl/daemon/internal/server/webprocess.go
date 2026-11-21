package server

import (
	"fmt"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/stallbeat"
)

// StallBeat watches the webhost's main-thread stalls for WebKit's
// memory-pressure beat (internal/stallbeat).
type StallBeat interface {
	// Observe takes one stall: the web view document that recorded it
	// (`webhost_document`), when it did, and how long that document's web
	// content process's main thread was blocked.
	Observe(document string, at time.Time, blockedMS int64)
	// Census takes a document's latest statement of its size, named in the
	// error the beat raises.
	Census(doc stallbeat.Document, at time.Time, c stallbeat.Census)
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

// pageCensusOperation is a workspace page's statement of its size
// (webapp/src/page-census.ts), and webhostCensusOperation a webhost
// document's (webapp/src/host/load-census.ts).
const (
	pageCensusOperation    = "page.census"
	webhostCensusOperation = "webhost.census"
)

// observePageCensus hands WS's page census to the stall beat. A census that
// cannot be read is recorded at ERROR and observed no further: the page
// writes every field on every census.
func (s *server) observePageCensus(log dlog.Logger, ws ids.WorkspaceID, record *agentreplv1.ClientLogRecord) {
	if record.GetOperation() != pageCensusOperation {
		return
	}
	at, c, err := pageCensusOf(record)
	if err != nil {
		unreadableCensus(log, record, err)
		return
	}
	s.deps.StallBeat.Census(stallbeat.Document{Kind: stallbeat.KindPage, Name: string(ws)}, at, c)
}

// observeWebhostCensus hands a webhost document's census to the stall beat,
// recording one it cannot read at ERROR.
func (s *server) observeWebhostCensus(record *agentreplv1.ClientLogRecord) {
	if record.GetOperation() != webhostCensusOperation {
		return
	}
	at, doc, c, err := webhostCensusOf(record)
	if err != nil {
		unreadableCensus(s.log, record, err)
		return
	}
	s.deps.StallBeat.Census(stallbeat.Document{Kind: stallbeat.KindWebhost, Name: doc}, at, c)
}

// unreadableCensus records a census that could not be read on LOG, the
// census's own workspace's or, for a webhost document, the central one.
func unreadableCensus(log dlog.Logger, record *agentreplv1.ClientLogRecord, err error) {
	log.Error("daemon.server.census_unreadable", "a document's census could not be read; the stall beat did not see it",
		dlog.Context{"operation": record.GetOperation(), "cause": err.Error(), "timestamp": record.GetTimestamp()})
}

// pageCensusOf answers a page census's moment and size.
func pageCensusOf(record *agentreplv1.ClientLogRecord) (time.Time, stallbeat.Census, error) {
	at, err := time.Parse(time.RFC3339Nano, record.GetTimestamp())
	if err != nil {
		return time.Time{}, stallbeat.Census{}, fmt.Errorf("timestamp %q: %w", record.GetTimestamp(), err)
	}
	fields := record.GetContext().AsMap()
	var c stallbeat.Census
	animations := int64(0)
	for _, f := range []struct {
		name string
		into *int64
	}{{"dom_nodes", &c.DomNodes}, {"feed_rows", &c.FeedRows}, {"animations", &animations}} {
		if *f.into, err = numberField(fields, f.name); err != nil {
			return time.Time{}, stallbeat.Census{}, err
		}
	}
	c.Animations = &animations
	visibility, ok := fields["visibility"].(string)
	if !ok || visibility == "" {
		return time.Time{}, stallbeat.Census{}, fmt.Errorf("visibility is %#v, not a state", fields["visibility"])
	}
	c.Visibility = visibility
	return at, c, nil
}

// webhostCensusOf answers a webhost census's moment, document and size.
func webhostCensusOf(record *agentreplv1.ClientLogRecord) (time.Time, string, stallbeat.Census, error) {
	at, err := time.Parse(time.RFC3339Nano, record.GetTimestamp())
	if err != nil {
		return time.Time{}, "", stallbeat.Census{}, fmt.Errorf("timestamp %q: %w", record.GetTimestamp(), err)
	}
	fields := record.GetContext().AsMap()
	doc, ok := fields["webhost_document"].(string)
	if !ok || doc == "" {
		return time.Time{}, "", stallbeat.Census{}, fmt.Errorf("webhost_document is %#v, not a document", fields["webhost_document"])
	}
	host, ok := fields["host"].(map[string]any)
	if !ok {
		return time.Time{}, "", stallbeat.Census{}, fmt.Errorf("host is %#v, not the document's size", fields["host"])
	}
	var c stallbeat.Census
	if c.DomNodes, err = numberField(host, "dom_nodes"); err != nil {
		return time.Time{}, "", stallbeat.Census{}, fmt.Errorf("host: %w", err)
	}
	if c.FeedRows, err = numberField(host, "feed_rows"); err != nil {
		return time.Time{}, "", stallbeat.Census{}, fmt.Errorf("host: %w", err)
	}
	return at, doc, c, nil
}

// numberField answers FIELDS[NAME] as a whole number.
func numberField(fields map[string]any, name string) (int64, error) {
	n, ok := fields[name].(float64)
	if !ok {
		return 0, fmt.Errorf("%s is %#v, not a number", name, fields[name])
	}
	return int64(n), nil
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
