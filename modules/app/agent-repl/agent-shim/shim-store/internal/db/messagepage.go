package db

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"strings"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/shim-store/internal/logging"
	"google.golang.org/protobuf/proto"
)

// MessagePageSize is the page's width, and it is a property of the TYPE:
// corev1.MessagePage declares exactly ten StoredMessage slots and no eleventh
// field, so a producer holding an eleventh message has nowhere to put it. The
// constant exists to bound the SQL, and it must never diverge from the number
// of slots the schema declares — pageSlot below fails loudly if it does.
const MessagePageSize = 10

// ownerSelectSQL selects the owners of the ten most recent messages strictly
// below the anchor.
//
// UNOWNED IS STRUCTURAL, NOT FILTERED. A durable record that composes no
// message — session and turn boundaries, heartbeats, latency samples, query
// lifecycle, usage observations, rewinds, file-plane diagnostics — renders as
// nothing and must never occupy a page slot. Its ownership column is SQL NULL
// rather than an empty string, and the index this statement reads
// (event_message_page) is PARTIAL on `top_level_message_id IS NOT NULL`. An
// unowned row is therefore not IN the structure being scanned: it cannot come
// back from a query that reads the index, whatever a WHERE clause remembers to
// say. An empty string would instead be a VALUE that sorts into SELECT
// DISTINCT, and the page would return ten "messages" several of which are turn
// boundaries — a short page the user is never told about.
//
// ONE INDEXED BACKWARD PASS, STOPPED EARLY. The statement is the design
// record's `SELECT DISTINCT top_level_message_id ... ORDER BY seq DESC LIMIT
// 10` with the DISTINCT and the LIMIT applied by the reader rather than by
// SQL: under DISTINCT, `seq` is no longer a column of the result and SQLite
// cannot order by it, and a message owning several records has several seqs to
// order by anyway. Walking the seq-descending index and stopping at the tenth
// new owner reads exactly the rows the page needs and not one row more —
// SQLite is never asked to visit the session's older history at all, which a
// GROUP BY over the whole session would force it to do.
const ownerSelectSQL = `SELECT top_level_message_id
  FROM event
  WHERE session_id = ? AND seq < ? AND top_level_message_id IS NOT NULL
  ORDER BY seq DESC`

// MessagePage answers one MessagePageRequest: at most ten messages, newest
// first, each carrying every durable record composing it.
//
// The unit is the MESSAGE and never the record. A page bounded by record count
// is a fragment, and a reader forced to keep asking until it happens to hold
// ten messages is running the unbounded scan under a new name. So the store
// resolves ownership itself and returns owned records WHOLE: a message owning
// hundreds of records arrives complete and still costs exactly one slot.
func (d *DB) MessagePage(ctx context.Context, req *corev1.MessagePageRequest) (*corev1.MessagePage, error) {
	if req == nil {
		panic("shim-store db: MessagePage requires a request")
	}
	sessionID := req.GetSessionId()
	fields := logging.Fields{Operation: "message-page", Table: "event", Session: sessionID, RequestID: req.GetRequestId()}
	d.log.LogVerbose(fields, "resolving message page anchor=%T", req.GetAnchor())
	if sessionID == "" {
		err := fmt.Errorf("shim-store query: message page without a session_id (request_id=%q)", req.GetRequestId())
		return nil, d.queryError("message-page", "event", "", err)
	}

	anchor, err := d.resolveAnchor(req)
	if err != nil {
		return nil, err
	}

	started := time.Now()
	var records int64
	defer func() { d.observeQuery(StatementMessagePage, "event", sessionID, started, records) }()

	owners, err := d.pageOwners(ctx, sessionID, anchor)
	if err != nil {
		return nil, err
	}
	page := &corev1.MessagePage{RequestId: req.GetRequestId()}
	if len(owners) == 0 {
		// Nothing owned below the anchor. That is the retained floor and it is
		// reported as such: retention is the store's own fact, never something
		// a caller may infer from a short page.
		page.Boundary = &corev1.MessagePage_Floor{Floor: &corev1.HistoryAtRetainedFloor{}}
		d.log.Log(fields, "message page reached the retained floor anchor=%d messages=0", anchor)
		return page, nil
	}

	messages, oldestSeq, recordCount, err := d.messagesFor(ctx, sessionID, owners)
	if err != nil {
		return nil, err
	}
	records = recordCount
	for i, m := range messages {
		if err := setPageSlot(page, i, m); err != nil {
			return nil, d.queryError("message-page", "event", sessionID, err)
		}
	}
	// last_page_seq is the OLDEST seq this page covers, minted here so a caller
	// never computes a position of its own — the next request copies it back
	// verbatim. Because it is the minimum over every record of every message on
	// this page, a continuation anchored at `seq < last_page_seq` cannot
	// re-select any message already served: overlap is impossible by
	// construction rather than by the caller's arithmetic.
	page.LastPageSeq = oldestSeq

	more, err := d.ownedRecordExistsBelow(ctx, sessionID, oldestSeq)
	if err != nil {
		return nil, err
	}
	if more {
		page.Boundary = &corev1.MessagePage_More{More: &corev1.HistoryRemainsBelow{}}
	} else {
		page.Boundary = &corev1.MessagePage_Floor{Floor: &corev1.HistoryAtRetainedFloor{}}
	}
	d.log.Log(fields, "message page served anchor=%d messages=%d records=%d last_page_seq=%d more=%t",
		anchor, len(messages), recordCount, oldestSeq, more)
	return page, nil
}

// resolveAnchor turns the request's anchor arm into the exclusive upper seq
// bound the page reads below.
//
// HEAD IS A FACT THE STORE RESOLVES. MessagePageHead is empty on purpose: a
// cold reader does not know the head seq and must never be made to name one.
// The store reads its own high-water mark and pages below it.
func (d *DB) resolveAnchor(req *corev1.MessagePageRequest) (uint64, error) {
	fields := logging.Fields{Operation: "message-page-anchor", Table: "event", Session: req.GetSessionId(), RequestID: req.GetRequestId()}
	switch a := req.GetAnchor().(type) {
	case *corev1.MessagePageRequest_Head:
		head, err := d.MaxSeq(req.GetSessionId())
		if err != nil {
			return 0, err
		}
		d.log.LogVerbose(fields, "anchored at the head head_seq=%d", head)
		// Exclusive bound, so the head record itself is on the page.
		return head + 1, nil
	case *corev1.MessagePageRequest_BeforeSeq:
		d.log.LogVerbose(fields, "anchored below a served page before_seq=%d", a.BeforeSeq)
		return a.BeforeSeq, nil
	default:
		err := fmt.Errorf("shim-store query: message page with no anchor arm (session=%q request_id=%q)", req.GetSessionId(), req.GetRequestId())
		return 0, d.queryError("message-page-anchor", "event", req.GetSessionId(), err)
	}
}

// pageOwners runs the bounded backward pass: at most ten owning message ids,
// newest first.
func (d *DB) pageOwners(ctx context.Context, sessionID string, anchor uint64) ([]string, error) {
	rows, err := d.sql.QueryContext(ctx, ownerSelectSQL, sessionID, anchor)
	if err != nil {
		return nil, d.queryError("message-page-owners", "event", sessionID,
			fmt.Errorf("shim-store query: message page owners (session=%q anchor=%d): %w", sessionID, anchor, err))
	}
	defer rows.Close()
	var owners []string
	seen := make(map[string]bool, MessagePageSize)
	for len(owners) < MessagePageSize && rows.Next() {
		var id string
		if err := rows.Scan(&id); err != nil {
			return nil, d.queryError("message-page-owners-scan", "event", sessionID,
				fmt.Errorf("shim-store query: scanning message page owner (session=%q anchor=%d): %w", sessionID, anchor, err))
		}
		if seen[id] {
			continue
		}
		seen[id] = true
		owners = append(owners, id)
	}
	// rows.Err() after an EARLY BREAK still reports a scan that failed
	// mid-stream, so the page is never assembled from a truncated read that
	// looked like a satisfied limit.
	if err := rows.Err(); err != nil {
		return nil, d.queryError("message-page-owners-iterate", "event", sessionID,
			fmt.Errorf("shim-store query: iterating message page owners (session=%q anchor=%d): %w", sessionID, anchor, err))
	}
	return owners, nil
}

// messagesFor loads every record owned by the selected messages, in one
// indexed pass, and assembles them newest message first with each message's
// records oldest first. It also reports the oldest seq the page covers and how
// many records it carries.
func (d *DB) messagesFor(ctx context.Context, sessionID string, owners []string) ([]*corev1.StoredMessage, uint64, int64, error) {
	placeholders := strings.TrimSuffix(strings.Repeat("?,", len(owners)), ",")
	query := `SELECT top_level_message_id, seq, payload FROM event
	  WHERE session_id = ? AND top_level_message_id IN (` + placeholders + `)
	  ORDER BY seq ASC`
	args := make([]any, 0, len(owners)+1)
	args = append(args, sessionID)
	for _, o := range owners {
		args = append(args, o)
	}
	rows, err := d.sql.QueryContext(ctx, query, args...)
	if err != nil {
		return nil, 0, 0, d.queryError("message-page-records", "event", sessionID,
			fmt.Errorf("shim-store query: message page records (session=%q messages=%d): %w", sessionID, len(owners), err))
	}
	defer rows.Close()

	byOwner := make(map[string]*corev1.StoredMessage, len(owners))
	for _, o := range owners {
		byOwner[o] = &corev1.StoredMessage{MessageId: o}
	}
	var oldestSeq uint64
	var count int64
	for rows.Next() {
		var owner string
		var seq uint64
		var blob []byte
		if err := rows.Scan(&owner, &seq, &blob); err != nil {
			return nil, 0, 0, d.queryError("message-page-records-scan", "event", sessionID,
				fmt.Errorf("shim-store query: scanning message page record (session=%q): %w", sessionID, err))
		}
		ev := &corev1.Event{}
		if err := proto.Unmarshal(blob, ev); err != nil {
			return nil, 0, 0, d.queryError("message-page-records-unmarshal", "event", sessionID,
				fmt.Errorf("shim-store query: unmarshaling message page record (session=%q seq=%d): %w", sessionID, seq, err))
		}
		msg, ok := byOwner[owner]
		if !ok {
			return nil, 0, 0, d.queryError("message-page-records", "event", sessionID,
				fmt.Errorf("shim-store query: message page record names unselected owner (session=%q seq=%d owner=%q)", sessionID, seq, owner))
		}
		msg.Records = append(msg.Records, ev)
		if oldestSeq == 0 || seq < oldestSeq {
			oldestSeq = seq
		}
		count++
	}
	if err := rows.Err(); err != nil {
		return nil, 0, 0, d.queryError("message-page-records-iterate", "event", sessionID,
			fmt.Errorf("shim-store query: iterating message page records (session=%q): %w", sessionID, err))
	}
	out := make([]*corev1.StoredMessage, 0, len(owners))
	for _, o := range owners {
		out = append(out, byOwner[o])
	}
	return out, oldestSeq, count, nil
}

// ownedRecordExistsBelow answers WHETHER older history remains — never how
// much and never where. It asks only about OWNED records: a session boundary
// or a heartbeat sitting below the page is not a message, and reporting it as
// remaining history would hand the caller a "load earlier" affordance that
// resolves to nothing.
func (d *DB) ownedRecordExistsBelow(ctx context.Context, sessionID string, seq uint64) (bool, error) {
	var one int
	row := d.sql.QueryRowContext(ctx,
		`SELECT 1 FROM event
		   WHERE session_id = ? AND seq < ? AND top_level_message_id IS NOT NULL
		   LIMIT 1`, sessionID, seq)
	switch err := row.Scan(&one); {
	case err == nil:
		return true, nil
	case errors.Is(err, sql.ErrNoRows):
		return false, nil
	default:
		return false, d.queryError("message-page-boundary", "event", sessionID,
			fmt.Errorf("shim-store query: message page boundary (session=%q below_seq=%d): %w", sessionID, seq, err))
	}
}

// setPageSlot writes one message into its numbered slot. Slots are filled from
// message_1 upward, so a caller reading them in order reads the page newest
// message first. An index past the declared slots is a loud failure rather
// than a silent drop: dropping would deliver a short page as if it were the
// truth.
func setPageSlot(page *corev1.MessagePage, i int, m *corev1.StoredMessage) error {
	switch i {
	case 0:
		page.Message_1 = m
	case 1:
		page.Message_2 = m
	case 2:
		page.Message_3 = m
	case 3:
		page.Message_4 = m
	case 4:
		page.Message_5 = m
	case 5:
		page.Message_6 = m
	case 6:
		page.Message_7 = m
	case 7:
		page.Message_8 = m
	case 8:
		page.Message_9 = m
	case 9:
		page.Message_10 = m
	default:
		return fmt.Errorf("shim-store query: message page slot %d exceeds the %d the schema declares (message_id=%q)", i+1, MessagePageSize, m.GetMessageId())
	}
	return nil
}
