package statedb

import (
	"database/sql"
	"errors"
	"fmt"
)

// keepalivewindow.go — THE DURABLE LEDGER OF WHEN THE DAEMON WAS PINGING.
//
// WHAT IT IS FOR. Keep-alive turns are conversation PLUMBING: they are
// persisted like any other turn, never deleted, and never rendered. Something
// has to decide, for each conversation item reaching a frontend, whether it
// belongs to a ping — and that decision must come out the same way on a live
// push and on a replay three days later.
//
// IDENTITY FIRST, TIME ONLY AS THE FALLBACK. A stream-plane conversation item
// carries the request id the daemon submitted under, and a ping's request id IS
// its turn id — this table's own primary key. So for every item that carries an
// id the exclusion is a KEY LOOKUP against the row, and it answers the same way
// whatever either clock says. That is what HasTurn is for.
//
// FILE-PLANE items are the ones with nothing to match: transcript records the
// vendor wrote back carry no prompt origin and no request id (the sidecar's
// file-plane events leave request_id empty), so the origin lives only on the
// control frame the daemon sent. What such an item does carry is its own
// instant — and a ping occupies a closed interval of wall-clock time on exactly
// one workspace. Placing an item in that interval is decidable from the item
// alone, which is what Covers answers.
//
// THE INTERVAL'S TWO BOUNDS MUST COME FROM THE CLOCK THAT STAMPED THE ITEMS.
// Item timestamps are VENDOR-clocked, so a bound taken from the daemon's own
// clock decides the comparison by clock agreement rather than by evidence. Both
// bounds are therefore stamped from the ping's own turn-boundary produced_at_ms
// — the start re-stamped at TurnStarted (Open's ON CONFLICT re-stamp), the end
// at TurnEnded — with the pre-submit daemon read kept only as a provisional
// lower bound for a ping that never started.
//
// Either way the verdict is re-derivable from the item alone, which is the
// property the merge lease's provenance ledger relies on for the same reason: a
// verdict re-derived from live state would answer differently on a resync than
// it did on the original push.
//
// A ROW IS NEVER DELETED. The whole contract is "withheld, not deleted": the
// turns stay in the store and stay excluded forever, so the evidence that
// excluded them has to last as long as they do.

// KeepAliveWindow is one cache keep-alive turn's wall-clock interval.
type KeepAliveWindow struct {
	// TurnID is the ping's request id, the identity the daemon minted for it.
	TurnID string
	// Workspace scopes the window. A ping on one workspace says nothing about
	// items on another, and the vendor conversation ids differ per workspace.
	Workspace string
	// StartedAtMs is when the daemon committed to submitting the ping.
	StartedAtMs int64
	// EndedAtMs is when the ping's turn ended, or 0 while it is still running.
	// An OPEN window excludes everything after its start, which is the correct
	// reading: the turn really is in flight, and anything the vendor writes
	// during it belongs to the ping.
	EndedAtMs int64
}

// KeepAliveWindows owns the keep_alive_window table.
type KeepAliveWindows struct{ db *sql.DB }

// NewKeepAliveWindows installs the table on the shared state store.
//
// The DDL is idempotent and purely ADDITIVE, so it carries no schema version of
// its own — the same discipline the prompt-receipt table follows, and for the
// same reason: a table that appears by CREATE TABLE IF NOT EXISTS cannot make
// an older binary's reading of the rest of the database wrong.
func NewKeepAliveWindows(db *sql.DB) (*KeepAliveWindows, error) {
	if db == nil {
		return nil, fmt.Errorf("statedb: NewKeepAliveWindows needs an open state store")
	}
	if _, err := db.Exec(`
		CREATE TABLE IF NOT EXISTS keep_alive_window (
			turn_id       TEXT    PRIMARY KEY,
			workspace     TEXT    NOT NULL,
			started_at_ms INTEGER NOT NULL,
			ended_at_ms   INTEGER NOT NULL DEFAULT 0
		);
		CREATE INDEX IF NOT EXISTS keep_alive_window_workspace
			ON keep_alive_window(workspace, started_at_ms);
	`); err != nil {
		return nil, fmt.Errorf("statedb: create keep_alive_window: %w", err)
	}
	return &KeepAliveWindows{db: db}, nil
}

// Open records a ping's start. It is written BEFORE the prompt reaches the
// shim, for the prompt receipt's reason inverted: a ping the vendor ran with no
// window behind it would be rendered as though the user had typed it, and that
// is the one outcome the exclusion exists to prevent. A window for a ping whose
// submit then failed is harmless — it excludes an interval in which nothing was
// written.
//
// THE RE-STAMP IS THE POINT OF THE ON CONFLICT CLAUSE, not defensive
// idempotence. The first Open carries the daemon's own clock read, the only
// bound available before the ping has even been submitted; the ping's
// TurnStarted then supplies that same instant on the VENDOR clock that stamped
// the items this window is compared against, and re-opening the row replaces
// the provisional bound with it. A window that never receives a start boundary
// keeps the provisional bound, which is exactly the ping that never ran.
func (k *KeepAliveWindows) Open(w KeepAliveWindow) error {
	if w.TurnID == "" || w.Workspace == "" || w.StartedAtMs <= 0 {
		return fmt.Errorf("statedb: refusing to open an incomplete keep-alive window %+v", w)
	}
	if _, err := k.db.Exec(`
		INSERT INTO keep_alive_window(turn_id, workspace, started_at_ms, ended_at_ms)
		VALUES (?,?,?,0)
		ON CONFLICT(turn_id) DO UPDATE SET started_at_ms = excluded.started_at_ms`,
		w.TurnID, w.Workspace, w.StartedAtMs); err != nil {
		return fmt.Errorf("statedb: open keep-alive window %s: %w", w.TurnID, err)
	}
	return nil
}

// ErrKeepAliveWindowInverted reports a close whose end instant precedes the
// window's own start.
//
// IT IS A DEFECT REPORT, NOT A DEGRADED MODE. An interval with ended < started
// covers NOTHING — every Covers read against it is false — so a window written
// that way silently stops excluding the very ping it was opened for, and the
// ping renders as the user's own prompt. The only way that pair of instants can
// arise is a disagreement between the clock that stamped the start and the one
// that stamped the end, which is a fact about the daemon worth surfacing rather
// than absorbing.
var ErrKeepAliveWindowInverted = errors.New("statedb: keep-alive window close would invert the interval")

// Close stamps a ping's end. Closing an unknown turn is a no-op rather than an
// error: a daemon that restarted mid-ping legitimately sees the end of a window
// it did not open.
//
// AN INVERTED INTERVAL IS UNREPRESENTABLE, in the write itself. The UPDATE
// stamps MAX(?, started_at_ms), so no caller — not this daemon, not a later one
// — can leave a row whose end precedes its start, whatever the two clocks that
// produced those instants believe. The clamp lands on the window's own start,
// which is the same empty interval ReconcileOpenWindows stamps for a ping that
// never ran, and is the honest reading of "the end is not after the start".
//
// THE CLAMP IS REPORTED, ALWAYS. Close returns ErrKeepAliveWindowInverted for
// the clamped write, so the caller's established fail-hard path (the log line
// plus the system-failure card in sessioncontroller/keepalivesubmit.go) fires on
// exactly the occasions the interval was not the one the boundary asked for.
// Bounding the row and staying silent would trade a permanent rendering blackout
// for an invisible one.
func (k *KeepAliveWindows) Close(turnID string, endedAtMs int64) error {
	if turnID == "" || endedAtMs <= 0 {
		return fmt.Errorf("statedb: refusing to close keep-alive window %q at %d", turnID, endedAtMs)
	}
	// Read for the DIAGNOSIS only; the UPDATE below is what enforces the bound,
	// so a row that moves between the two cannot produce an inverted write.
	var startedAtMs int64
	switch err := k.db.QueryRow(
		`SELECT started_at_ms FROM keep_alive_window WHERE turn_id = ? AND ended_at_ms = 0`,
		turnID).Scan(&startedAtMs); {
	case errors.Is(err, sql.ErrNoRows):
		// Unknown or already closed — the no-op this method documents.
		return nil
	case err != nil:
		return fmt.Errorf("statedb: read keep-alive window %s before close: %w", turnID, err)
	}
	if _, err := k.db.Exec(`
		UPDATE keep_alive_window SET ended_at_ms = MAX(?, started_at_ms)
		WHERE turn_id = ? AND ended_at_ms = 0`,
		endedAtMs, turnID); err != nil {
		return fmt.Errorf("statedb: close keep-alive window %s: %w", turnID, err)
	}
	if endedAtMs < startedAtMs {
		return fmt.Errorf("%w: turn_id=%s started_at_ms=%d ended_at_ms=%d — the end was clamped to the start, so the window covers only the instant the ping began; the two instants came from clocks that disagree",
			ErrKeepAliveWindowInverted, turnID, startedAtMs, endedAtMs)
	}
	return nil
}

// HasTurn reports whether turnID names one of workspace's keep-alive windows —
// the IDENTITY question, and the one the exclusion asks first.
//
// It is answered off the table's PRIMARY KEY and is therefore independent of
// every clock in the system: an item carrying a ping's request id belongs to
// that ping whether or not any interval agrees. Workspace-scoped for Covers's
// reason — a ping on one workspace says nothing about items on another — even
// though a minted turn id is unique on its own.
func (k *KeepAliveWindows) HasTurn(workspace, turnID string) (bool, error) {
	if workspace == "" || turnID == "" {
		return false, nil
	}
	var n int
	if err := k.db.QueryRow(
		`SELECT COUNT(1) FROM keep_alive_window WHERE workspace = ? AND turn_id = ?`,
		workspace, turnID).Scan(&n); err != nil {
		return false, fmt.Errorf("statedb: read keep-alive window %q for %q: %w", turnID, workspace, err)
	}
	return n > 0, nil
}

// Covers reports whether tsMs falls inside any of workspace's keep-alive
// windows — the ONE question the exclusion asks.
//
// AN OPEN WINDOW HAS NO UPPER BOUND, deliberately. While a ping is in flight
// everything the vendor writes for that workspace belongs to it, and treating
// an unclosed window as empty would leak exactly the records a live ping
// produces, which are the ones a user is most likely to be watching for.
func (k *KeepAliveWindows) Covers(workspace string, tsMs int64) (bool, error) {
	if workspace == "" || tsMs <= 0 {
		return false, nil
	}
	var n int
	if err := k.db.QueryRow(`
		SELECT COUNT(1) FROM keep_alive_window
		WHERE workspace = ? AND started_at_ms <= ?
		  AND (ended_at_ms = 0 OR ended_at_ms >= ?)`,
		workspace, tsMs, tsMs).Scan(&n); err != nil {
		return false, fmt.Errorf("statedb: read keep-alive windows for %q at %d: %w", workspace, tsMs, err)
	}
	return n > 0, nil
}

// TurnEndLookup answers, from durable evidence, when a turn ended. Satisfied by
// *TurnAccountings.
//
// It is an interface rather than a concrete dependency so the reconciliation
// states exactly what it needs — one durable instant per turn id — and a test
// can supply that without a second table.
type TurnEndLookup interface {
	// EndedAtMs reports a turn's durable end instant, and whether one exists.
	EndedAtMs(turnID string) (int64, bool, error)
}

// ReconcileOpenWindows closes every window a previous daemon left open, and
// reports how many it closed.
//
// WHY THIS EXISTS. A window is opened before the ping is submitted and closed
// at the ping's turn end. Everything between those two writes is a hole: a
// crash, a SIGTERM, an account switch, a session delete — anything that stops
// the daemon mid-ping — leaves ended_at_ms=0 with nobody alive who knows the
// turn id. An open window has NO UPPER BOUND, so that row goes on excluding
// every conversation item on its workspace from every rendering, forever. The
// in-memory claim was the only thing bound to close it, and in-memory state
// does not survive the deaths this repairs.
//
// AT BOOT, EVERY OPEN ROW IS AN ORPHAN. No ping can be in flight before any
// session controller exists, so "open" and "abandoned" are the same set — which
// is what makes this a reconciliation rather than a guess, and why it must run
// during store bring-up and not later.
//
// THE END IS STAMPED FROM DURABLE DATA, NEVER FROM NOW. Now is when the repair
// happened to run, which may be days after the ping; stamping it would extend
// the exclusion across every real turn in between and delete them from the
// rendering — the same blackout, merely bounded. The honest end is the turn's
// own recorded end when the store holds one, and otherwise the window's own
// start: an interval containing only the instant the daemon committed to a ping
// it never finished.
func (k *KeepAliveWindows) ReconcileOpenWindows(turns TurnEndLookup) (int, error) {
	if turns == nil {
		return 0, fmt.Errorf("statedb: keep-alive window reconciliation needs a turn end lookup")
	}
	rows, err := k.db.Query(
		`SELECT turn_id, started_at_ms FROM keep_alive_window WHERE ended_at_ms = 0`)
	if err != nil {
		return 0, fmt.Errorf("statedb: list open keep-alive windows: %w", err)
	}
	type openWindow struct {
		turnID      string
		startedAtMs int64
	}
	var open []openWindow
	for rows.Next() {
		var w openWindow
		if err := rows.Scan(&w.turnID, &w.startedAtMs); err != nil {
			rows.Close()
			return 0, fmt.Errorf("statedb: scan open keep-alive window: %w", err)
		}
		open = append(open, w)
	}
	if err := rows.Err(); err != nil {
		rows.Close()
		return 0, fmt.Errorf("statedb: iterate open keep-alive windows: %w", err)
	}
	rows.Close()
	closed := 0
	for _, w := range open {
		endedAtMs := w.startedAtMs
		endedAt, ok, err := turns.EndedAtMs(w.turnID)
		if err != nil {
			return closed, fmt.Errorf("statedb: reconcile keep-alive window %s: %w", w.turnID, err)
		}
		if ok && endedAt > endedAtMs {
			endedAtMs = endedAt
		}
		if err := k.Close(w.turnID, endedAtMs); err != nil {
			return closed, err
		}
		closed++
	}
	return closed, nil
}

// List returns workspace's windows, oldest first. It backs the rewind's
// dropped-turn accounting and the tests.
func (k *KeepAliveWindows) List(workspace string) ([]KeepAliveWindow, error) {
	rows, err := k.db.Query(`
		SELECT turn_id, workspace, started_at_ms, ended_at_ms
		FROM keep_alive_window WHERE workspace = ? ORDER BY started_at_ms, turn_id`, workspace)
	if err != nil {
		return nil, fmt.Errorf("statedb: list keep-alive windows for %q: %w", workspace, err)
	}
	defer rows.Close()
	var out []KeepAliveWindow
	for rows.Next() {
		var w KeepAliveWindow
		if err := rows.Scan(&w.TurnID, &w.Workspace, &w.StartedAtMs, &w.EndedAtMs); err != nil {
			return nil, fmt.Errorf("statedb: scan keep-alive window: %w", err)
		}
		out = append(out, w)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("statedb: iterate keep-alive windows for %q: %w", workspace, err)
	}
	return out, nil
}
