package ssm

import (
	"database/sql"
	"fmt"
)

// Wiring is a workspace's position on the WIRED axis: the one fact that decides
// whether anything the agent reports may be shown at all.
type Wiring int

const (
	// WiringDormant — nothing is wired to this workspace and nothing is coming
	// up. A hibernated session, one that was never opened, every workspace of a
	// daemon that has just restarted.
	WiringDormant Wiring = iota
	// WiringStarting — a bring-up is ACTIVELY in flight. Strictly that: it is
	// not "we would like a session", it is "the spawn and the handshake are
	// under way right now", which is why the only producer is the bring-up path
	// itself and why every way that path can end closes it.
	WiringStarting
	// WiringWired — the bring-up gate CLOSED: the shim answered ShimReady, the
	// same verdict AwaitReady resolves on. The substrate is proven.
	WiringWired
)

// token maps a wiring to the signal token stored on the axis.
func (w Wiring) token() string {
	switch w {
	case WiringWired:
		return sigWired
	case WiringStarting:
		return sigStarting
	default:
		return sigDormant
	}
}

func (w Wiring) String() string { return w.token() }

// ApplyWired moves the workspace's WIRED axis, which is the axis every color in
// the vocabulary now stands on.
//
// THE LAW IT ENFORCES. A workspace's color is CONNECTION TRUTH: blue means
// there is no live backend session for this workspace, and every non-blue color
// is a GUARANTEE that the session substrate is fully wired — shim live,
// handshake complete, store link settled. So the agent axis and the vendor
// outcome are visible ONLY while this axis reads `wired`, and the rank table
// enforces that rather than any Go branch: `dormant` sits at 12 and `starting`
// at 14, both above `thinking` at 30 and `vendor_blocked` at 20.
//
// IT IS AN AXIS, NOT AN AGENT ROW, for the reason the paint axis was one and the
// permission row is not: it is a standing condition with a clearing token, not a
// report of how a turn ended. A `wired` row is superseded by nothing the agent
// does — only by the wiring going away.
//
// THE PRODUCER IS SESSIONDRV, which is the only component that knows. The
// opening edge is the bring-up gate's own ShimReady verdict; the closing edges
// are driver exit, hibernation, shim stop, and the bounce window a vendor
// session rotation opens. See internal/sessiondrv/wiredstate.go.
//
// REASON is a short note recorded as the cause detail, so the log names WHY a
// workspace went blue rather than only that it did.
func (m *Manager) ApplyWired(workspace string, wiring Wiring, reason string) error {
	if workspace == "" {
		return fmt.Errorf("ssm: ApplyWired got an empty workspace")
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	return m.applyWiredLocked(workspace, wiring, reason)
}

// applyWiredLocked appends a wired-axis row when the axis actually MOVES.
// Caller holds mu.
//
// A no-op is loud. A second `starting` means a bring-up believes it is starting
// a workspace this log already shows starting; a `dormant` over a `dormant` is a
// teardown of something already torn down. Both are ordinary — the bring-up path
// is re-entrant and teardown is idempotent by design — and both are worth seeing
// when a workspace's color is being explained.
func (m *Manager) applyWiredLocked(workspace string, wiring Wiring, reason string) error {
	current, err := wiredAxisTop(m.db, workspace)
	if err != nil {
		return err
	}
	want := wiring.token()
	if current == want {
		m.logf("ssm: wired axis unchanged ws=%s wiring=%s reason=%q — no row appended", workspace, want, reason)
		return nil
	}
	cause := causeWired
	if reason != "" {
		cause = causeWired + ":" + reason
	}
	if err := appendRow(m.db, workspace, "", want, cause, sql.NullInt64{}, m.nextAt(), ""); err != nil {
		return err
	}
	m.logf("ssm: wired axis ws=%s %s→%s reason=%q", workspace, orNone(current), want, reason)
	return m.reresolveLocked(workspace, cause, 0)
}

// orNone names an empty axis for the transition log.
func orNone(token string) string {
	if token == "" {
		return "∅"
	}
	return token
}

// wiredAxisTop returns the workspace's newest wired-axis token, or "" when the
// axis has no row at all.
//
// "" IS NOT `wired`. The resolution query treats an axis with no row as dormant
// — a workspace with agent history and nothing wired to it has no evidence of a
// live session — so a caller comparing against this must never read the empty
// answer as "already fine".
func wiredAxisTop(db *sql.DB, workspace string) (string, error) {
	var state string
	err := db.QueryRow(
		`SELECT state FROM workspace_state
		 WHERE workspace = ? AND state IN ('wired','starting','dormant')
		 ORDER BY at DESC LIMIT 1`, workspace).Scan(&state)
	if err == sql.ErrNoRows {
		return "", nil
	}
	if err != nil {
		return "", fmt.Errorf("ssm: wired-axis read for workspace %q: %w", workspace, err)
	}
	return state, nil
}

// dormantEveryWorkspaceLocked closes the wired axis for every workspace whose
// log still shows one wired or starting. Caller holds mu (warm).
//
// THE WIRING DOES NOT SURVIVE A RESTART and the agent-axis history does, which
// is the whole asymmetry. A daemon that comes back up has no shim connections at
// all, so a `wired` row left standing from the previous process would let a
// restored workspace paint whatever its last turn reported — a green tab, or a
// red one mid-turn — for a session nothing is connected to. That is precisely
// the claim the connection-truth law forbids.
//
// `starting` is closed for the same reason and one more: a bring-up that was in
// flight when the daemon died ended with the daemon, so nothing is coming.
//
// It appends the row WITHOUT resolving or pushing, because warm() is a restore
// rather than a transition: the cache is seeded from the log immediately after
// this runs, so it seeds on the dormant answer and no phantom transition is
// announced for a workspace nobody has been told about yet.
//
// A workspace with no wired row at all is LEFT ALONE. The resolution query
// already answers dormant for it, so a row would say nothing new and every
// restart would grow the log by one row per workspace forever.
//
// Phase B's reattach sweep re-wires the working set; until then every restored
// workspace is honestly dormant.
func (m *Manager) dormantEveryWorkspaceLocked() error {
	names, err := distinctWorkspaces(m.db)
	if err != nil {
		return err
	}
	closed := 0
	for _, ws := range names {
		top, err := wiredAxisTop(m.db, ws)
		if err != nil {
			return err
		}
		if top != sigWired && top != sigStarting {
			continue
		}
		if err := appendRow(m.db, ws, "", sigDormant, causeWired+":daemon_restart",
			sql.NullInt64{}, m.nextAt(), ""); err != nil {
			return err
		}
		closed++
	}
	if closed > 0 {
		m.logf("ssm: %d workspace(s) marked dormant at open — nothing is wired to a daemon that has just started", closed)
	}
	return nil
}
