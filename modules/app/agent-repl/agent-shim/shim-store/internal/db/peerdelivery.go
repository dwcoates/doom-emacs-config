package db

import (
	"context"
	"database/sql"
	"errors"
	"strings"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-store/internal/logging"
	"google.golang.org/protobuf/proto"
)

// peerdelivery.go — the delivery index: which books hold the two ends of each
// cross-session message, by the vendor's delivery id for it, and the lookup
// store.v1.GetPeerDelivery answers through it.
//
// WHY IT EXISTS. A cross-session message names neither end as one of the
// host's sessions: the sender's record names the recipient only as the address
// the caller wrote, and the recipient's record names the sender only by the
// name it asserted. The one value both records share is the delivery id: the
// sender's SendMessage success carries it (AgentSendMessagePeerSession), and so
// does the recipient's peer message (PeerMessageInterSession). Every session on
// the host writes into this store, so both records are here whichever sessions
// hold them, and this index is what finds them.
//
// IT IS AN INDEX OVER PAGE LINES, kept like turn_prompt: one row per page line
// holding either end, at the row's position, naming which end it is, written in
// the write's own transaction and backfilled once from the stored frames. A
// retired line keeps its row and is excluded by the reader's kind check,
// exactly as a page excludes it.

const peerDeliveryTable = "peer_delivery"

// peerDeliveryDDL is the in-place table and its lookup index (db.go
// inPlaceTables). The book is copied beside the delivery id because the lookup
// is delivery-then-book and an index cannot span two tables. `end_kind` is
// which end of the message the line records: deliveryEndSender or
// deliveryEndRecipient.
const peerDeliveryDDL = `CREATE TABLE IF NOT EXISTS peer_delivery (
  position      INTEGER PRIMARY KEY,
  book_agent_id TEXT    NOT NULL,
  delivery_id   TEXT    NOT NULL,
  end_kind      TEXT    NOT NULL
);
CREATE INDEX IF NOT EXISTS peer_delivery_by_id ON peer_delivery(delivery_id)`

const (
	// peerDeliveryUpsertSQL binds (position, book, delivery id, end). A row's
	// book never changes once stamped, and a re-derived line restates the same
	// id and end.
	peerDeliveryUpsertSQL = `INSERT INTO peer_delivery (position, book_agent_id, delivery_id, end_kind) VALUES (?,?,?,?)
  ON CONFLICT(position) DO UPDATE SET book_agent_id = excluded.book_agent_id, delivery_id = excluded.delivery_id,
    end_kind = excluded.end_kind`

	// peerDeliveryDeleteSQL binds (position): the row is not, or is no longer,
	// either end of a cross-session message.
	peerDeliveryDeleteSQL = `DELETE FROM peer_delivery WHERE position = ?`

	// peerDeliveryBackfillSQL binds (the page-line kind): every page line
	// stored before the table existed. Read once, in the create's own
	// transaction.
	peerDeliveryBackfillSQL = `SELECT position, book_agent_id, frame FROM entry WHERE kind = ? AND book_agent_id IS NOT NULL`

	// peerDeliveryBooksSQL binds (delivery id, the page-line kind): every
	// (end, book) holding a live page line with that delivery id. At package
	// scope so the suite EXPLAINs the production text itself.
	peerDeliveryBooksSQL = `SELECT DISTINCT d.end_kind, d.book_agent_id FROM peer_delivery d
	  CROSS JOIN entry e ON e.position = d.position
	  WHERE d.delivery_id = ? AND e.kind = ?
	  ORDER BY d.end_kind ASC, d.book_agent_id ASC`

	// lineageRootSQL binds (an agent id): the ROOT of that agent's lineage, the
	// main agent its spawn chain leads up to, through a spawning agent, a
	// workflow's spawner, or detached work's owner, exactly the edges the
	// downward sessionLineageCTE walks. A main agent is its own root. Bounded,
	// so a corrupt cycle ends rather than looping.
	lineageRootSQL = `WITH RECURSIVE up(agent_id, depth) AS (
	    SELECT ?, 0
	    UNION
	    SELECT COALESCE(a.spawned_by_agent, f.spawner_agent, w.owner_agent), up.depth + 1
	    FROM up
	    JOIN agent a ON a.agent_id = up.agent_id
	    LEFT JOIN workflow f ON a.spawned_by_workflow = f.run_agent_id
	    LEFT JOIN detached_work w ON a.spawned_by_workflow = w.work_id
	    WHERE COALESCE(a.spawned_by_agent, f.spawner_agent, w.owner_agent) IS NOT NULL AND up.depth < 64
	  )
	  SELECT agent_id FROM up ORDER BY depth DESC LIMIT 1`
)

// The two ends of a cross-session message, as end_kind spells them.
const (
	deliveryEndSender    = "sender"
	deliveryEndRecipient = "recipient"
)

// errAmbiguousDelivery is the cause of one end of a delivery found in the books
// of two sessions: one message is sent by one session and delivered to one, so
// the index's invariant is broken and the store refuses rather than choosing.
var errAmbiguousDelivery = errors.New("one end of one delivery is held by more than one session")

// peerDeliveryOf is the delivery id a stored entry's page line records and
// which end of the message it is, or ("", "") when it records neither: the
// SENDER's end is a SendMessage success to another session, the RECIPIENT's
// an inter-session peer message.
func peerDeliveryOf(line *storev1.StorePageLine) (deliveryID, end string) {
	item := line.GetAgentItem()
	if id := item.GetPeerMessage().GetInterSession().GetDeliveryId().GetValue(); id != "" {
		return id, deliveryEndRecipient
	}
	send := item.GetAgentFrame().GetUpdate().GetActivity().GetSendMessage()
	if id := send.GetSuccess().GetPeerSession().GetDeliveryId().GetValue(); id != "" {
		return id, deliveryEndSender
	}
	return "", ""
}

// writePeerDelivery keeps a booked row's peer_delivery row, IN THE WRITE'S OWN
// TRANSACTION: present exactly when the row is a page line recording either end
// of a cross-session message. It is the one writer of the table besides its
// backfill.
func (d *DB) writePeerDelivery(ctx context.Context, tx *sql.Tx, position int64, book string, r routed) error {
	delivery, end := "", ""
	if r.kind == kindPageLine {
		delivery, end = peerDeliveryOf(r.pageLine)
	}
	if delivery == "" {
		if _, err := tx.ExecContext(ctx, peerDeliveryDeleteSQL, position); err != nil {
			return storagef(err, "clearing the peer delivery of the row at position %d", position)
		}
		return nil
	}
	if _, err := tx.ExecContext(ctx, peerDeliveryUpsertSQL, position, book, delivery, end); err != nil {
		return storagef(err, "recording the row at position %d as the %s end of delivery %q", position, end, delivery)
	}
	return nil
}

// backfillPeerDeliveries indexes every peer message stored before
// peer_delivery existed. A frame that cannot be decoded is a LOUD failure, as
// on every read path: the open fails (never nukes) rather than leaving a
// delivery out of the index.
func backfillPeerDeliveries(ctx context.Context, tx *sql.Tx) error {
	rows, err := tx.QueryContext(ctx, peerDeliveryBackfillSQL, kindPageLine)
	if err != nil {
		return storagef(err, "reading the page lines to index their peer deliveries")
	}
	type delivery struct {
		position int64
		book     string
		id       string
		end      string
	}
	var deliveries []delivery
	for rows.Next() {
		var position int64
		var book string
		var frame []byte
		if err := rows.Scan(&position, &book, &frame); err != nil {
			rows.Close() //nolint:errcheck // the scan already failed
			return storagef(err, "scanning a page line to index its peer delivery")
		}
		entry := &storev1.StoreEntry{}
		if err := proto.Unmarshal(frame, entry); err != nil {
			rows.Close() //nolint:errcheck // the decode already failed
			return storagef(err, "stored frame at position %d cannot be decoded", position)
		}
		if id, end := peerDeliveryOf(entry.GetAgentUpdate().GetServeableFrame()); id != "" {
			deliveries = append(deliveries, delivery{position: position, book: book, id: id, end: end})
		}
	}
	if err := rows.Err(); err != nil {
		rows.Close() //nolint:errcheck // the iteration already failed
		return storagef(err, "iterating the page lines to index their peer deliveries")
	}
	if err := rows.Close(); err != nil {
		return storagef(err, "closing the page-line listing")
	}
	for _, p := range deliveries {
		if _, err := tx.ExecContext(ctx, peerDeliveryUpsertSQL, p.position, p.book, p.id, p.end); err != nil {
			return storagef(err, "indexing the peer delivery %q at position %d", p.id, p.position)
		}
	}
	return nil
}

// PeerDelivery answers WHICH SESSIONS hold the two ends of one cross-session
// message: for each end, the main agent at the root of the lineage of the book
// its page line is filed in, or "" when no recorded session holds that end (an
// outside session, or a record not written yet), which is an ordinary answer
// recorded at info.
//
// AN END HELD BY TWO SESSIONS breaks the index's invariant, and the store
// refuses to choose, as a storage failure recorded at ERROR naming every
// candidate.
//
// UNSCOPED BY DESIGN: the answer is always another session. An empty delivery
// id is refused before any statement runs.
func (d *DB) PeerDelivery(ctx context.Context, deliveryID string) (sender, recipient string, err error) {
	base := logging.Fields{Operation: "store.db.peer-delivery", Table: peerDeliveryTable}
	if deliveryID == "" {
		return "", "", d.refuse(base, invalidFieldf("delivery_id", "delivery_id is empty — the lookup names the delivery it resolves"))
	}
	started := d.mono()
	rows, err := d.read.QueryContext(ctx, peerDeliveryBooksSQL, deliveryID, kindPageLine)
	if err != nil {
		return "", "", d.refuse(base, storagef(err, "reading the books holding delivery %s", deliveryID))
	}
	books := map[string][]string{}
	for rows.Next() {
		var end, book string
		if err := rows.Scan(&end, &book); err != nil {
			rows.Close() //nolint:errcheck // the scan already failed
			return "", "", d.refuse(base, storagef(err, "scanning a book holding delivery %s", deliveryID))
		}
		books[end] = append(books[end], book)
	}
	if err := rows.Err(); err != nil {
		rows.Close() //nolint:errcheck // the iteration already failed
		return "", "", d.refuse(base, storagef(err, "iterating the books holding delivery %s", deliveryID))
	}
	if err := rows.Close(); err != nil {
		return "", "", d.refuse(base, storagef(err, "closing the books holding delivery %s", deliveryID))
	}
	ends := map[string]string{}
	for _, end := range []string{deliveryEndSender, deliveryEndRecipient} {
		session, err := d.endSession(ctx, deliveryID, end, books[end])
		if err != nil {
			return "", "", d.refuse(base, err)
		}
		ends[end] = session
	}
	found := 0
	for _, session := range ends {
		if session != "" {
			found++
		}
	}
	d.observeQuery(StatementPeerDelivery, peerDeliveryTable, base, started, int64(found))
	d.traceStatement(ctx, StatementPeerDelivery, peerDeliveryTable, base, int64(found))
	d.log.Log(base, "delivery %s: sender %s, recipient %s", deliveryID,
		recordedOr(ends[deliveryEndSender]), recordedOr(ends[deliveryEndRecipient]))
	return ends[deliveryEndSender], ends[deliveryEndRecipient], nil
}

// endSession is the one session holding one end of a delivery, from the books
// its lines are filed in: "" for none, a storage failure for more than one.
func (d *DB) endSession(ctx context.Context, deliveryID, end string, books []string) (string, error) {
	var sessions []string
	seen := map[string]bool{}
	for _, book := range books {
		root, err := d.lineageRoot(ctx, book)
		if err != nil {
			return "", storagef(err, "reading the lineage root of book %s for the %s end of delivery %s", book, end, deliveryID)
		}
		if !seen[root] {
			seen[root] = true
			sessions = append(sessions, root)
		}
	}
	switch len(sessions) {
	case 0:
		return "", nil
	case 1:
		return sessions[0], nil
	default:
		return "", storagef(errAmbiguousDelivery, "the %s end of delivery %s is held by %d sessions (%s)",
			end, deliveryID, len(sessions), strings.Join(sessions, ", "))
	}
}

// recordedOr spells a session for the log, or that no recorded session holds it.
func recordedOr(session string) string {
	if session == "" {
		return "not recorded"
	}
	return session
}

// lineageRoot is the main agent at the root of `agent`'s lineage; an agent the
// record holds no row for is its own root.
func (d *DB) lineageRoot(ctx context.Context, agent string) (string, error) {
	var root string
	if err := d.read.QueryRowContext(ctx, lineageRootSQL, agent).Scan(&root); err != nil {
		return "", err
	}
	return root, nil
}
