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

// peerdelivery.go — the delivery index: which book holds the delivered copy of
// each cross-session message, by the vendor's delivery id for it
// (conversation.v1.PeerMessageInterSession.delivery_id), and the lookup
// store.v1.GetPeerRecipient answers through it.
//
// WHY IT EXISTS. A send to another Claude session names its recipient only as
// the address the caller wrote; the one value both ends share is the delivery
// id. Every session on the host writes into this store, so the recipient's copy
// is here whichever session holds it, and this index is what finds it.
//
// IT IS AN INDEX OVER PAGE LINES, kept like turn_prompt: one row per page line
// holding an inter-session peer message that states a delivery id, at the
// row's position, written in the write's own transaction and backfilled once
// from the stored frames. A retired line keeps its row and is excluded by the
// reader's kind check, exactly as a page excludes it.

const peerDeliveryTable = "peer_delivery"

// peerDeliveryDDL is the in-place table and its lookup index (db.go
// inPlaceTables). The book is copied beside the delivery id because the lookup
// is delivery-then-book and an index cannot span two tables.
const peerDeliveryDDL = `CREATE TABLE IF NOT EXISTS peer_delivery (
  position      INTEGER PRIMARY KEY,
  book_agent_id TEXT    NOT NULL,
  delivery_id   TEXT    NOT NULL
);
CREATE INDEX IF NOT EXISTS peer_delivery_by_id ON peer_delivery(delivery_id)`

const (
	// peerDeliveryUpsertSQL binds (position, book, delivery id). A row's book
	// never changes once stamped, and a re-derived line restates the same id.
	peerDeliveryUpsertSQL = `INSERT INTO peer_delivery (position, book_agent_id, delivery_id) VALUES (?,?,?)
  ON CONFLICT(position) DO UPDATE SET book_agent_id = excluded.book_agent_id, delivery_id = excluded.delivery_id`

	// peerDeliveryDeleteSQL binds (position): the row is not, or is no longer,
	// a peer message stating a delivery id.
	peerDeliveryDeleteSQL = `DELETE FROM peer_delivery WHERE position = ?`

	// peerDeliveryBackfillSQL binds (the page-line kind): every page line
	// stored before the table existed. Read once, in the create's own
	// transaction.
	peerDeliveryBackfillSQL = `SELECT position, book_agent_id, frame FROM entry WHERE kind = ? AND book_agent_id IS NOT NULL`

	// peerDeliveryBooksSQL binds (delivery id, the page-line kind): every book
	// holding a live page line with that delivery id. At package scope so the
	// suite EXPLAINs the production text itself.
	peerDeliveryBooksSQL = `SELECT DISTINCT d.book_agent_id FROM peer_delivery d
	  CROSS JOIN entry e ON e.position = d.position
	  WHERE d.delivery_id = ? AND e.kind = ?
	  ORDER BY d.book_agent_id ASC`

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

// errAmbiguousDelivery is the cause of a delivery id found in the books of two
// sessions: one message is delivered to one session, so the index's invariant
// is broken and the store refuses rather than choosing.
var errAmbiguousDelivery = errors.New("one delivery id is held by more than one session")

// peerDeliveryOf is the delivery id a stored entry's inter-session peer
// message states, or "" when it is not one or states none.
func peerDeliveryOf(entry *storev1.StoreEntry) string {
	peer := entry.GetAgentUpdate().GetServeableFrame().GetAgentItem().GetPeerMessage()
	return peer.GetInterSession().GetDeliveryId().GetValue()
}

// writePeerDelivery keeps a booked row's peer_delivery row, IN THE WRITE'S OWN
// TRANSACTION: present exactly when the row is a page line holding an
// inter-session peer message that states a delivery id. It is the one writer
// of the table besides its backfill.
func (d *DB) writePeerDelivery(ctx context.Context, tx *sql.Tx, position int64, book string, r routed) error {
	delivery := ""
	if r.kind == kindPageLine {
		delivery = peerDeliveryOf(r.entry)
	}
	if delivery == "" {
		if _, err := tx.ExecContext(ctx, peerDeliveryDeleteSQL, position); err != nil {
			return storagef(err, "clearing the peer delivery of the row at position %d", position)
		}
		return nil
	}
	if _, err := tx.ExecContext(ctx, peerDeliveryUpsertSQL, position, book, delivery); err != nil {
		return storagef(err, "recording the row at position %d as the delivery %q", position, delivery)
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
		if id := peerDeliveryOf(entry); id != "" {
			deliveries = append(deliveries, delivery{position: position, book: book, id: id})
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
		if _, err := tx.ExecContext(ctx, peerDeliveryUpsertSQL, p.position, p.book, p.id); err != nil {
			return storagef(err, "indexing the peer delivery %q at position %d", p.id, p.position)
		}
	}
	return nil
}

// PeerRecipient answers WHICH SESSION holds the delivered copy of one
// cross-session message: the main agent at the root of the lineage of the book
// its page line is filed in.
//
// THREE ANSWERS. One session: `found` is true. None: `found` is false with no
// error, an ordinary answer recorded at info (an outside session, or a copy not
// written yet). Two or more: the index's invariant is broken, and the store
// refuses to choose, as a storage failure recorded at ERROR naming every
// candidate.
//
// UNSCOPED BY DESIGN: the answer is always another session. An empty delivery
// id is refused before any statement runs.
func (d *DB) PeerRecipient(ctx context.Context, deliveryID string) (session string, found bool, err error) {
	base := logging.Fields{Operation: "store.db.peer-recipient", Table: peerDeliveryTable}
	if deliveryID == "" {
		return "", false, d.refuse(base, invalidFieldf("delivery_id", "delivery_id is empty — the lookup names the delivery it resolves"))
	}
	started := d.mono()
	books, err := d.scanStrings(ctx, peerDeliveryBooksSQL, deliveryID, kindPageLine)
	if err != nil {
		return "", false, d.refuse(base, storagef(err, "reading the books holding delivery %s", deliveryID))
	}
	roots := map[string]bool{}
	var sessions []string
	for _, book := range books {
		root, err := d.lineageRoot(ctx, book)
		if err != nil {
			return "", false, d.refuse(base, storagef(err, "reading the lineage root of book %s for delivery %s", book, deliveryID))
		}
		if !roots[root] {
			roots[root] = true
			sessions = append(sessions, root)
		}
	}
	d.observeQuery(StatementPeerRecipient, peerDeliveryTable, base, started, int64(len(sessions)))
	d.traceStatement(ctx, StatementPeerRecipient, peerDeliveryTable, base, int64(len(sessions)))
	switch len(sessions) {
	case 0:
		d.log.Log(base, "no recorded session holds delivery %s", deliveryID)
		return "", false, nil
	case 1:
		d.log.Log(base, "delivery %s is held by session %s", deliveryID, sessions[0])
		return sessions[0], true, nil
	default:
		return "", false, d.refuse(base, storagef(errAmbiguousDelivery,
			"delivery %s is held by %d sessions (%s)", deliveryID, len(sessions), strings.Join(sessions, ", ")))
	}
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
