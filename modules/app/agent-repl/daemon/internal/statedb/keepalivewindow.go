package statedb

import (
	"database/sql"
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
// WHY IT IS KEYED ON TIME RATHER THAN ON IDENTITY. File-plane conversation
// items carry NO prompt origin and NO turn id. The origin lives on the control
// frame the daemon sent, not on the transcript records the vendor wrote back,
// so there is nothing on the item itself to match a ping against. What every
// item does carry is its own instant — and a ping occupies a closed interval of
// wall-clock time on exactly one workspace. Placing an item in that interval is
// therefore decidable from the item alone, which is precisely the property the
// merge lease's provenance ledger relies on for the same reason: a verdict
// re-derived from live state would answer differently on a resync than it did
// on the original push.
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

// Close stamps a ping's end. Closing an unknown turn is a no-op rather than an
// error: a daemon that restarted mid-ping legitimately sees the end of a window
// it did not open.
func (k *KeepAliveWindows) Close(turnID string, endedAtMs int64) error {
	if turnID == "" || endedAtMs <= 0 {
		return fmt.Errorf("statedb: refusing to close keep-alive window %q at %d", turnID, endedAtMs)
	}
	if _, err := k.db.Exec(
		`UPDATE keep_alive_window SET ended_at_ms = ? WHERE turn_id = ? AND ended_at_ms = 0`,
		endedAtMs, turnID); err != nil {
		return fmt.Errorf("statedb: close keep-alive window %s: %w", turnID, err)
	}
	return nil
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
