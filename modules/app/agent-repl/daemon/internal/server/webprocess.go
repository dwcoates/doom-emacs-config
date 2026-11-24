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
// recording one it cannot read at ERROR, and the host document's frames to the
// held-page table (observeHostFrames).
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
	if doc == hostDocument {
		s.observeHostFrames(record)
	}
}

// hostDocument is the webhost document that holds every workspace's frame
// (webapp/src/host/document.ts); the sidebar document holds none.
const hostDocument = "host"

// opHostFrames is the operation every record of the host's frames against
// Emacs's report is written under.
const opHostFrames = "daemon.server.host_frames"

// observeHostFrames hands the frames the host document's census names to the
// held-page table, the truth of which pages the web view holds, and records
// each disagreement with Emacs's report that begins standing, at WARN once per
// cause with both sets, and its clearing at INFO.
//
// A census with no `frames` is a host of a build older than the report, which
// a deploy reloads onto the build it serves: recorded at DEBUG, the table
// keeps what it had. So is a census from a document whose page stream does
// not stand here (it ended, or the document dialed another daemon): the table
// takes no frames nobody's stream scopes. A `connection_id`, `frames` or
// `trigger` that cannot be read is the census's defect, at ERROR.
func (s *server) observeHostFrames(record *agentreplv1.ClientLogRecord) {
	fields := record.GetContext().AsMap()
	if _, stated := fields["frames"]; !stated {
		s.log.Debug(opHostFrames, "the host's census names no frames: a host of an older build", dlog.Context{"timestamp": record.GetTimestamp()})
		return
	}
	conn, frames, tick, err := hostFramesOf(fields)
	if err != nil {
		unreadableCensus(s.log, record, err)
		return
	}
	check, taken := s.deps.HeldPages.HostReported(conn, frames, tick)
	if !taken {
		s.log.Debug(opHostFrames, "the host's census came from a document whose page stream does not stand here; its frames are not taken",
			dlog.Context{"connection_id": conn, "timestamp": record.GetTimestamp()})
		return
	}
	for _, d := range check.Began {
		s.log.Warn(opHostFrames, "the pages the web host holds and the pages Emacs reports holding disagree; the count follows the host", dlog.Context{
			"cause": string(d.Cause), "workspaces": workspaceNames(d.Workspaces),
			"host_frames": workspaceNames(d.Host), "emacs_pages": workspaceNames(d.Emacs),
		})
	}
	for _, cause := range check.Cleared {
		s.log.Info(opHostFrames, "the web host's frames and Emacs's report agree again", dlog.Context{"cause": string(cause)})
	}
}

// hostFramesOf answers a host census's document connection, its frames and
// whether it was a tick.
func hostFramesOf(fields map[string]any) (string, []ids.WorkspaceID, bool, error) {
	conn, ok := fields["connection_id"].(string)
	if !ok || conn == "" {
		return "", nil, false, fmt.Errorf("connection_id is %#v, not a document's connection", fields["connection_id"])
	}
	raw, ok := fields["frames"].([]any)
	if !ok {
		return "", nil, false, fmt.Errorf("frames is %#v, not a list of workspaces", fields["frames"])
	}
	frames := make([]ids.WorkspaceID, 0, len(raw))
	for _, v := range raw {
		id, ok := v.(string)
		if !ok || id == "" {
			return "", nil, false, fmt.Errorf("frames holds %#v, not a workspace id", v)
		}
		frames = append(frames, ids.WorkspaceID(id))
	}
	switch trigger := fields["trigger"]; trigger {
	case "tick":
		return conn, frames, true, nil
	case "change":
		return conn, frames, false, nil
	default:
		return "", nil, false, fmt.Errorf("trigger is %#v, not tick or change", trigger)
	}
}

// workspaceNames answers WSS as strings, for a record's context.
func workspaceNames(wss []ids.WorkspaceID) []string {
	out := make([]string, len(wss))
	for i, ws := range wss {
		out[i] = string(ws)
	}
	return out
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
