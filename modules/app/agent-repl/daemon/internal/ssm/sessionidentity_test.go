package ssm

import (
	"database/sql"
	"path/filepath"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
	"claude-repld/internal/statedb"
)

// THE SESSION-STATUS AXIS HAS EXACTLY ONE IDENTITY PER SESSION.
//
// Events come back from the store filed under the VENDOR session uuid, while
// every party that closes, invalidates or re-claims a turn names the session by
// its daemon-minted s_<hex> id. When both reached workspace_state.session_id,
// one session held rows under two names and every claimant comparison between
// them read "a different session": the turn's own teardown declined to close
// its `thinking` and the session's own next prompt was refused as another
// session's, permanently. These tests hold both halves of the repair — the
// write path that canonicalizes, and the v7 migration that normalizes rows
// written before it.

// vendorAliasResolver models the production registry: a session answers to BOTH
// its daemon id and its vendor uuid(s), and reports the daemon id either way.
type vendorAliasResolver struct {
	workspace string
	daemonID  string
	// vendorIDs are the uuids the store files this session's events under.
	// More than one is a vendor uuid ROTATION (a `/clear`).
	vendorIDs []string
}

func (r vendorAliasResolver) Session(sessionID string) (Binding, bool) {
	if sessionID == r.daemonID {
		return Binding{Workspace: r.workspace, SessionID: r.daemonID}, true
	}
	for _, vendor := range r.vendorIDs {
		if sessionID == vendor {
			return Binding{Workspace: r.workspace, SessionID: r.daemonID}, true
		}
	}
	return Binding{}, false
}

// statusRow is one workspace_state row's two identities.
type statusRow struct {
	sessionID      string
	eventSessionID string
}

// topStatusRow reads the workspace's newest session-status row.
func topStatusRow(t *testing.T, db *sql.DB, workspace string) statusRow {
	t.Helper()
	var sid, esid sql.NullString
	err := db.QueryRow(
		`SELECT session_id, event_session_id FROM workspace_state
		  WHERE workspace = ? AND state IN `+sessionStatusMembers+`
		  ORDER BY at DESC LIMIT 1`, workspace).Scan(&sid, &esid)
	if err != nil {
		t.Fatalf("read top status row for %q: %v", workspace, err)
	}
	return statusRow{sessionID: sid.String, eventSessionID: esid.String}
}

// TestApplyStampsTheDaemonSessionIDOnAStoreStreamTurn: the `thinking` a
// TurnStarted writes is OWNED by the daemon session, even though the event that
// caused it arrived under the vendor uuid.
func TestApplyStampsTheDaemonSessionIDOnAStoreStreamTurn(t *testing.T) {
	// Arrange
	resolver := vendorAliasResolver{workspace: "ws1", daemonID: "s_daemon", vendorIDs: []string{"vendor-uuid"}}
	m, _, _ := openUnwiredTest(t, resolver)
	if err := m.ApplyWired("ws1", WiringWired, "test arrangement"); err != nil {
		t.Fatalf("ApplyWired: %v", err)
	}

	// Act: the turn arrives under the identity the STORE knows.
	if err := applyTest(m, evTurnStarted("vendor-uuid", 1)); err != nil {
		t.Fatalf("Apply: %v", err)
	}

	// Assert
	got := topStatusRow(t, m.db, "ws1")
	if got.sessionID != "s_daemon" {
		t.Fatalf("thinking row session_id = %q, want the daemon id %q; a row no claimant check can match wedges the workspace forever",
			got.sessionID, "s_daemon")
	}
}

// TestApplyRecordsTheStoreCoordinateOfATurnEvent: the vendor identity is not
// discarded by the canonicalization, it moves to the column idempotency reads.
func TestApplyRecordsTheStoreCoordinateOfATurnEvent(t *testing.T) {
	// Arrange
	resolver := vendorAliasResolver{workspace: "ws1", daemonID: "s_daemon", vendorIDs: []string{"vendor-uuid"}}
	m, _, _ := openUnwiredTest(t, resolver)
	if err := m.ApplyWired("ws1", WiringWired, "test arrangement"); err != nil {
		t.Fatalf("ApplyWired: %v", err)
	}

	// Act
	if err := applyTest(m, evTurnStarted("vendor-uuid", 1)); err != nil {
		t.Fatalf("Apply: %v", err)
	}

	// Assert
	if got := topStatusRow(t, m.db, "ws1"); got.eventSessionID != "vendor-uuid" {
		t.Fatalf("thinking row event_session_id = %q, want %q", got.eventSessionID, "vendor-uuid")
	}
}

// TestApplyStillDeduplicatesAReplayedEvent: canonicalizing the OWNER must not
// cost the idempotency the store coordinate provides.
func TestApplyStillDeduplicatesAReplayedEvent(t *testing.T) {
	// Arrange
	resolver := vendorAliasResolver{workspace: "ws1", daemonID: "s_daemon", vendorIDs: []string{"vendor-uuid"}}
	m, cl, _ := openUnwiredTest(t, resolver)
	if err := m.ApplyWired("ws1", WiringWired, "test arrangement"); err != nil {
		t.Fatalf("ApplyWired: %v", err)
	}
	if err := applyTest(m, evTurnStarted("vendor-uuid", 1)); err != nil {
		t.Fatalf("Apply: %v", err)
	}

	// Act: the same event again.
	if err := applyTest(m, evTurnStarted("vendor-uuid", 1)); err != nil {
		t.Fatalf("Apply (replay): %v", err)
	}

	// Assert
	if !cl.contains("replayed=true") {
		t.Fatalf("a replayed event was not recognized by the durable ledger: %v", cl.lines)
	}
	if got := cl.count("→RENDER_STATE_THINKING"); got != 1 {
		t.Fatalf("thinking transitions logged = %d, want 1 — the replay must not repaint", got)
	}
}

// TestApplyAcceptsTheFirstSeqOfARotatedVendorSpace: a vendor uuid rotation
// restarts the store's seq space at 1 under the SAME daemon session, so
// idempotency must be keyed on the store's identity rather than the owner's —
// otherwise the first event of the new space reads as a replay of the retired
// one and the turn it starts is dropped.
func TestApplyAcceptsTheFirstSeqOfARotatedVendorSpace(t *testing.T) {
	// Arrange: seq 1 already spent in the RETIRED vendor space.
	resolver := vendorAliasResolver{
		workspace: "ws1", daemonID: "s_daemon",
		vendorIDs: []string{"vendor-retired", "vendor-fresh"},
	}
	m, _, _ := openUnwiredTest(t, resolver)
	if err := m.ApplyWired("ws1", WiringWired, "test arrangement"); err != nil {
		t.Fatalf("ApplyWired: %v", err)
	}
	if err := applyTest(m, evTurnStarted("vendor-retired", 1)); err != nil {
		t.Fatalf("Apply (retired space): %v", err)
	}

	// Act: the NEW space's own first event.
	if err := applyTest(m, evTurnStarted("vendor-fresh", 1)); err != nil {
		t.Fatalf("Apply (fresh space): %v", err)
	}
	// Both spaces' turns end, so the workspace settles rather than standing on
	// a claim the retired space opened.
	// Oldest claim first: a LEGACY start carries no turn id, so the ledger can
	// only correlate its end by FIFO order within the claimant's own queue.
	if err := applyTest(m, evTurnEnded("vendor-retired", 2, false)); err != nil {
		t.Fatalf("Apply (retired space end): %v", err)
	}
	if err := applyTest(m, evTurnEnded("vendor-fresh", 2, false)); err != nil {
		t.Fatalf("Apply (fresh space end): %v", err)
	}

	// Assert
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_DONE {
		t.Fatalf("state = %s, want DONE; the rotated space's first event was swallowed as a duplicate", renderName(got))
	}
}

// --- the v7 normalization of rows written before the fix ---------------------

// wedgedStore writes a state store in the shape the bug left behind: schema v6,
// a `thinking` row on ws claimed by the VENDOR uuid, and a registry record
// carrying the vendor→daemon mapping. It returns the store's path.
//
// recordCWD is the workspace the session record claims. Passing a DIFFERENT
// workspace is how a test asks whether the migration will rewrite a row it has
// no mapping for.
func wedgedStore(t *testing.T, workspace, recordCWD, daemonID, vendorID string) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "state.db")
	db, err := statedb.Open(path)
	if err != nil {
		t.Fatalf("open store: %v", err)
	}
	defer db.Close()
	if err := migrate(db, t.Logf); err != nil {
		t.Fatalf("migrate: %v", err)
	}
	// The registry's identity table, which shares this store in production.
	if _, err := db.Exec(`
		CREATE TABLE IF NOT EXISTS session_record (
			session_id        TEXT PRIMARY KEY,
			cwd               TEXT NOT NULL DEFAULT '',
			claude_session_id TEXT NOT NULL DEFAULT '',
			created_at        TEXT NOT NULL DEFAULT ''
		)`); err != nil {
		t.Fatalf("create session_record: %v", err)
	}
	if _, err := db.Exec(
		`INSERT INTO session_record(session_id, cwd, claude_session_id, created_at) VALUES (?,?,?,?)`,
		daemonID, recordCWD, vendorID, "2026-08-01T00:00:00Z"); err != nil {
		t.Fatalf("insert session_record: %v", err)
	}
	// The bad row, exactly as the old write path produced it: the vendor uuid
	// in session_id, no store coordinate of its own.
	if _, err := db.Exec(
		`INSERT INTO workspace_state(workspace, session_id, state, cause_kind, cause_seq, at)
		 VALUES (?,?,?,?,?,?)`,
		workspace, vendorID, sigThinking, causeTurnStarted, 42, 1_700_000_000_000); err != nil {
		t.Fatalf("insert wedged thinking row: %v", err)
	}
	// Stamp it back to the schema the bad row was written under.
	if _, err := db.Exec(`UPDATE schema_meta SET version = 6`); err != nil {
		t.Fatalf("stamp v6: %v", err)
	}
	return path
}

// TestMigrationNormalizesAVendorIDThinkingRow: the standing claim becomes the
// daemon session's, so its own teardown and its own next prompt can reach it.
func TestMigrationNormalizesAVendorIDThinkingRow(t *testing.T) {
	// Arrange
	path := wedgedStore(t, "ws1", "ws1", "s_daemon", "vendor-uuid")

	// Act
	db, err := openDB(path, t.Logf)
	if err != nil {
		t.Fatalf("openDB: %v", err)
	}
	defer db.Close()

	// Assert
	if got := topStatusRow(t, db, "ws1"); got.sessionID != "s_daemon" {
		t.Fatalf("normalized session_id = %q, want %q", got.sessionID, "s_daemon")
	}
}

// TestMigrationKeepsTheStoreCoordinateOfANormalizedRow: the vendor uuid the row
// was filed under is preserved as the event identity, so the migration cannot
// make a replayed event look new.
func TestMigrationKeepsTheStoreCoordinateOfANormalizedRow(t *testing.T) {
	// Arrange
	path := wedgedStore(t, "ws1", "ws1", "s_daemon", "vendor-uuid")

	// Act
	db, err := openDB(path, t.Logf)
	if err != nil {
		t.Fatalf("openDB: %v", err)
	}
	defer db.Close()

	// Assert
	if got := topStatusRow(t, db, "ws1"); got.eventSessionID != "vendor-uuid" {
		t.Fatalf("normalized event_session_id = %q, want %q", got.eventSessionID, "vendor-uuid")
	}
}

// TestMigrationLeavesARowWithNoMappingAlone: a session_record for some OTHER
// workspace is not a mapping for this row. Rewriting on that evidence would
// hand one workspace's claim to a session that never made it — the migration
// repairs a known alias, it does not guess.
func TestMigrationLeavesARowWithNoMappingAlone(t *testing.T) {
	// Arrange: the record's cwd is a different workspace.
	path := wedgedStore(t, "ws1", "ws2", "s_daemon", "vendor-uuid")

	// Act
	db, err := openDB(path, t.Logf)
	if err != nil {
		t.Fatalf("openDB: %v", err)
	}
	defer db.Close()

	// Assert
	if got := topStatusRow(t, db, "ws1"); got.sessionID != "vendor-uuid" {
		t.Fatalf("session_id = %q, want it untouched at %q", got.sessionID, "vendor-uuid")
	}
}

// TestCloseStaleTurnClosesThePreviouslyDeclinedClaim: the exact wedge. Before
// the normalization this stop logged "DECLINED — the standing `thinking` is
// held by session=<vendor uuid>, which is not this stop's to spend".
func TestCloseStaleTurnClosesThePreviouslyDeclinedClaim(t *testing.T) {
	// Arrange
	path := wedgedStore(t, "ws1", "ws1", "s_daemon", "vendor-uuid")
	m := openWedgedManager(t, path)

	// Act
	closed, err := m.CloseStaleTurn("ws1", "s_daemon", "interrupted_by_restart", false)

	// Assert
	if err != nil {
		t.Fatalf("CloseStaleTurn: %v", err)
	}
	if !closed {
		t.Fatalf("CloseStaleTurn closed = false, want true; the session's own teardown must be able to spend its own claim")
	}
}

// TestMarkPromptAcceptedSucceedsAfterNormalization: the second half of the
// wedge. Before it, every prompt was refused with "while session <vendor uuid>
// owns the active turn" and the workspace could never be driven again.
func TestMarkPromptAcceptedSucceedsAfterNormalization(t *testing.T) {
	// Arrange
	path := wedgedStore(t, "ws1", "ws1", "s_daemon", "vendor-uuid")
	m := openWedgedManager(t, path)

	// Act
	err := m.MarkPromptAccepted("ws1", "s_daemon", "req-1", func(*frontendv1.WorkspaceState) {})

	// Assert
	if err != nil {
		t.Fatalf("MarkPromptAccepted: %v, want the session's own standing turn to be its own", err)
	}
}

// TestMarkPromptAcceptedStillRefusesAGenuinelyDifferentSession: normalization
// resolves an ALIAS, never a rival. A claim belonging to another session stays
// refused, loudly.
func TestMarkPromptAcceptedStillRefusesAGenuinelyDifferentSession(t *testing.T) {
	// Arrange
	path := wedgedStore(t, "ws1", "ws1", "s_daemon", "vendor-uuid")
	m := openWedgedManager(t, path)

	// Act
	err := m.MarkPromptAccepted("ws1", "s_other", "req-1", func(*frontendv1.WorkspaceState) {})

	// Assert
	if err == nil {
		t.Fatalf("MarkPromptAccepted = nil, want a refusal: the normalized claim belongs to s_daemon, not s_other")
	}
	if !strings.Contains(err.Error(), `session "s_daemon" owns the active turn`) {
		t.Fatalf("MarkPromptAccepted error = %v, want it to name the owning session loudly", err)
	}
}

// openWedgedManager opens a Manager over an existing store, running the
// migration on the way in and wiring the workspace so the axis under test is
// reachable.
func openWedgedManager(t *testing.T, path string) *Manager {
	t.Helper()
	cl := &capLog{}
	m, err := Open(Options{
		DBPath: path, Logf: cl.logf,
		Resolver: vendorAliasResolver{workspace: "ws1", daemonID: "s_daemon", vendorIDs: []string{"vendor-uuid"}},
	})
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(func() { m.Close() })
	if err := m.ApplyWired("ws1", WiringWired, "test arrangement"); err != nil {
		t.Fatalf("ApplyWired: %v", err)
	}
	// A REOPENED store comes up hibernated on the connection axis (nothing is
	// wired to a daemon that has just started), and the connection-truth law
	// holds every workspace closed until something reconnects it. The session
	// identity under test lives on the OTHER axis, so the reconnection is
	// arrangement.
	for _, state := range []SessionConnectivity{SessionConnectivityConnecting, SessionConnectivityOperational} {
		if err := m.ApplySessionConnectivity("ws1", "s_daemon", "gen-1", state, "test arrangement"); err != nil {
			t.Fatalf("ApplySessionConnectivity(%s): %v", state, err)
		}
	}
	return m
}
