package ssm

import (
	"database/sql"
	"fmt"
)

// Wiring is a workspace's position on the WIRED axis: the one fact that decides
// whether anything the agent reports may be shown at all.
type Wiring int

const (
	// WiringSevered — nothing is wired to this workspace, nothing is coming up,
	// and there is EVIDENCE THAT SOMETHING BROKE. A bring-up that could not be
	// completed, or a driver whose client.Run returned a terminal protocol
	// error.
	//
	// It is the ZERO VALUE deliberately. A Wiring nobody set says "the substrate
	// is broken", which is the loud reading; defaulting to WiringHibernated would
	// make an unset field claim a deliberate, benign teardown nobody performed.
	WiringSevered Wiring = iota
	// WiringStarting — a bring-up is ACTIVELY in flight. Strictly that: it is
	// not "we would like a session", it is "the spawn and the handshake are
	// under way right now", which is why the only producer is the bring-up path
	// itself and why every way that path can end closes it.
	WiringStarting
	// WiringWired — the bring-up gate CLOSED: the shim answered ShimReady, the
	// same verdict AwaitReady resolves on. The substrate is proven.
	WiringWired
	// WiringHibernated — nothing is wired to this workspace, nothing is coming
	// up, and NOTHING IS WRONG. We SIGTERMed the shim on purpose to reclaim its
	// ~500MB, or a daemon has just booted and has not reattached anything yet.
	//
	// It is the other half of the old WiringDormant, and the split is the point:
	// one token meant both "asleep by choice" and "the substrate is broken", so
	// the idle sweeper reaping an untouched workspace was indistinguishable from
	// a dead shim. A closing edge must now say WHICH it is, and there is no
	// default — every producer in sessiondrv/wiredstate.go picks explicitly.
	WiringHibernated
)

// token maps a wiring to the signal token stored on the axis.
//
// Every case is EXPLICIT rather than leaning on the default arm, because the two
// closed halves are one wrong branch apart: a hibernation that fell through to
// severed would paint every deliberate teardown as a breakage, which is exactly
// the conflation the split removed.
func (w Wiring) token() string {
	switch w {
	case WiringWired:
		return sigWired
	case WiringStarting:
		return sigStarting
	case WiringHibernated:
		return sigHibernated
	default:
		return sigSevered
	}
}

func (w Wiring) String() string { return w.token() }

// ApplyWired moves the workspace's WIRED axis, which is the axis every color in
// the vocabulary now stands on.
//
// THE LAW IT ENFORCES. A workspace's color is CONNECTION TRUTH: blue and teal
// both mean there is no live backend session for this workspace, and every OTHER
// color is a GUARANTEE that the session substrate is fully wired — shim live,
// handshake complete, store link settled. So the agent axis and the vendor
// outcome are visible ONLY while this axis reads `wired`, and the rank table
// enforces that rather than any Go branch: `severed` sits at 12, `starting` at
// 14 and `hibernated` at 15, all above `thinking` at 30 and `vendor_blocked` at
// 20.
//
// THE CLOSED HALF IS TWO WIRINGS, and every caller must choose. `severed` claims
// something broke; `hibernated` claims nothing did. One token used to serve both
// and therefore served neither: the idle sweeper reclaiming ~500MB from an
// untouched workspace painted a tab exactly like a dead shim, so blue stopped
// meaning anything at all.
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
// a workspace this log already shows starting; a `severed` over a `severed` is a
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
// "" IS NOT `wired`. The resolution query treats an axis with no row as
// hibernated — a workspace with agent history and nothing wired to it has no
// evidence of a live session, and equally none of a breakage — so a caller
// comparing against this must never read the empty answer as "already fine".
//
// The legacy `dormant` spelling stays in the IN-list for the same reason it
// stays in the resolution query: `workspace_state` is append-only, so a
// workspace's newest wired row can still be a pre-split one, and omitting it
// would make applyWiredLocked believe the axis had never moved at all.
func wiredAxisTop(db *sql.DB, workspace string) (string, error) {
	var state string
	err := db.QueryRow(
		`SELECT state FROM workspace_state
		 WHERE workspace = ? AND state IN ('wired','starting','severed','hibernated','dormant')
		 ORDER BY at DESC LIMIT 1`, workspace).Scan(&state)
	if err == sql.ErrNoRows {
		return "", nil
	}
	if err != nil {
		return "", fmt.Errorf("ssm: wired-axis read for workspace %q: %w", workspace, err)
	}
	return state, nil
}

// hibernateEveryWorkspaceLocked closes the wired axis for every workspace whose
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
// IT CLOSES TO `hibernated`, NOT `severed`, and that is the whole point of the
// name. A FRESHLY BOOTED DAEMON IS NOT A BROKEN ONE: nothing failed, nothing
// died mid-flight that this process could witness, and the bootsweep is about to
// reattach every shim that survived the bounce. Painting the whole tab-bar blue
// on every restart is exactly what spent blue's meaning — a user who watches
// every workspace go blue after an ordinary daemon bounce learns to ignore blue,
// and then misses the one workspace whose substrate really is severed.
//
// It appends the row WITHOUT resolving or pushing, because warm() is a restore
// rather than a transition: the cache is seeded from the log immediately after
// this runs, so it seeds on the hibernated answer and no phantom transition is
// announced for a workspace nobody has been told about yet.
//
// A workspace with no wired row at all is LEFT ALONE. The resolution query
// already answers hibernated for it, so a row would say nothing new and every
// restart would grow the log by one row per workspace forever.
//
// Phase B's reattach sweep re-wires the working set; until then every restored
// workspace is honestly asleep.
func (m *Manager) hibernateEveryWorkspaceLocked() error {
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
		if err := appendRow(m.db, ws, "", sigHibernated, causeWired+":daemon_restart",
			sql.NullInt64{}, m.nextAt(), ""); err != nil {
			return err
		}
		closed++
	}
	if closed > 0 {
		m.logf("ssm: %d workspace(s) marked hibernated at open — nothing is wired to a daemon that has just started, and a daemon that has just started is not a broken one", closed)
	}
	return nil
}
