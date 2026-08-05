package ssm

import (
	"path/filepath"
	"testing"
)

// newSupersedeTestManager opens a bare Manager: these cases exercise the turn
// ledger's SQL directly, so no resolver or wiring is needed.
func newSupersedeTestManager(t *testing.T) *Manager {
	t.Helper()
	m, err := Open(Options{
		DBPath: filepath.Join(t.TempDir(), "state.db"),
		Logf:   func(string, ...any) {},
	})
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(func() { m.Close() })
	return m
}

// THE CLAIMS OF DISCARDED TURNS MUST CLOSE. They live in the seq space the
// rewind retires, so nothing in the new space will ever deliver their
// TurnEnded; an untouched claim stays active forever and the workspace reads
// as THINKING with no turn behind it.
func TestSupersedeTurnClaimsClosesADiscardedTurn(t *testing.T) {
	// Arrange.
	m := newSupersedeTestManager(t)
	openClaim(t, m, "/ws", "s1", "ka_1")

	// Act.
	closed, _, err := m.SupersedeTurnClaims("/ws", "s1", []string{"ka_1"}, TurnCloseCauseSupersededByRewind)

	// Assert.
	if err != nil {
		t.Fatalf("SupersedeTurnClaims: %v", err)
	}
	if len(closed) != 1 || closed[0] != "ka_1" {
		t.Fatalf("closed = %v, want [ka_1]", closed)
	}
	if active := activeClaimIDs(t, m, "/ws", "s1"); len(active) != 0 {
		t.Fatalf("active claims = %v after supersession, want none", active)
	}
}

// THE CLOSE IS ATTRIBUTED through the ledger's existing end_cause vocabulary,
// so a reader can tell a rewind-superseded claim from one the daemon closed
// because the shim said the turn had vanished.
func TestSupersedeTurnClaimsRecordsTheCause(t *testing.T) {
	// Arrange.
	m := newSupersedeTestManager(t)
	openClaim(t, m, "/ws", "s1", "ka_1")

	// Act.
	if _, _, err := m.SupersedeTurnClaims("/ws", "s1", []string{"ka_1"}, TurnCloseCauseSupersededByRewind); err != nil {
		t.Fatalf("SupersedeTurnClaims: %v", err)
	}

	// Assert.
	var cause string
	if err := m.db.QueryRow(
		`SELECT end_cause FROM turn_lifecycle_claim WHERE workspace=? AND turn_id=?`,
		"/ws", "ka_1").Scan(&cause); err != nil {
		t.Fatalf("read end_cause: %v", err)
	}
	if cause != TurnCloseCauseSupersededByRewind {
		t.Fatalf("end_cause = %q, want %q", cause, TurnCloseCauseSupersededByRewind)
	}
}

// AN UNKNOWN TURN IS A NO-OP. A rewind can name a turn this workspace holds no
// claim for at all, and there is nothing to close or attribute.
func TestSupersedeTurnClaimsIgnoresAnUnknownTurn(t *testing.T) {
	// Arrange.
	m := newSupersedeTestManager(t)

	// Act.
	closed, attributed, err := m.SupersedeTurnClaims("/ws", "s1", []string{"never_opened"}, TurnCloseCauseSupersededByRewind)

	// Assert.
	if err != nil {
		t.Fatalf("SupersedeTurnClaims over an unknown turn = %v, want a benign no-op", err)
	}
	if len(closed) != 0 || len(attributed) != 0 {
		t.Fatalf("closed = %v attributed = %v, want nothing", closed, attributed)
	}
}

// AN UNATTRIBUTED CLOSE IS REFUSED: it would be indistinguishable in the ledger
// from a claim that was simply lost.
func TestSupersedeTurnClaimsRefusesAnEmptyCause(t *testing.T) {
	// Arrange.
	m := newSupersedeTestManager(t)
	openClaim(t, m, "/ws", "s1", "ka_1")

	// Act.
	_, _, err := m.SupersedeTurnClaims("/ws", "s1", []string{"ka_1"}, "")

	// Assert.
	if err == nil {
		t.Fatal("SupersedeTurnClaims accepted an unattributed close")
	}
}

// A turn NOT named by the rewind keeps its claim, so one rewind cannot retire
// a live turn it had nothing to do with.
func TestSupersedeTurnClaimsLeavesUnnamedTurnsAlone(t *testing.T) {
	// Arrange.
	m := newSupersedeTestManager(t)
	openClaim(t, m, "/ws", "s1", "ka_1")
	openClaim(t, m, "/ws", "s1", "real_turn")

	// Act.
	if _, _, err := m.SupersedeTurnClaims("/ws", "s1", []string{"ka_1"}, TurnCloseCauseSupersededByRewind); err != nil {
		t.Fatalf("SupersedeTurnClaims: %v", err)
	}

	// Assert.
	active := activeClaimIDs(t, m, "/ws", "s1")
	if len(active) != 1 || active[0] != "real_turn" {
		t.Fatalf("active claims = %v, want only the turn the rewind did not name", active)
	}
}

// THE ORDINARY CASE: a keep-alive ping ALWAYS ends normally before the rewind
// that discards it runs, so its claim is already closed with an empty cause by
// the time the rewind arrives. It must still be attributed, or the ledger
// cannot answer which closes the daemon's own supersession produced.
func TestSupersedeTurnClaimsAttributesAnAlreadyClosedClaim(t *testing.T) {
	// Arrange.
	m := newSupersedeTestManager(t)
	openClaim(t, m, "/ws", "s1", "ka_1")
	closeClaim(t, m, "/ws", "s1", "ka_1", 77, "")

	// Act.
	closed, attributed, err := m.SupersedeTurnClaims("/ws", "s1", []string{"ka_1"}, TurnCloseCauseSupersededByRewind)

	// Assert.
	if err != nil {
		t.Fatalf("SupersedeTurnClaims: %v", err)
	}
	if len(closed) != 0 {
		t.Fatalf("closed = %v, want nothing closed here: the claim was already closed", closed)
	}
	if len(attributed) != 1 || attributed[0] != "ka_1" {
		t.Fatalf("attributed = %v, want [ka_1]", attributed)
	}
	endSeq, cause := claimEnd(t, m, "/ws", "ka_1")
	if cause != TurnCloseCauseSupersededByRewind {
		t.Fatalf("end_cause = %q, want %q", cause, TurnCloseCauseSupersededByRewind)
	}
	if endSeq != 77 {
		t.Fatalf("end_seq = %d, want the real boundary 77 preserved", endSeq)
	}
}

// AN EXISTING ATTRIBUTION IS NEVER OVERWRITTEN. end_cause already records why
// the daemon ended that claim; a later rewind does not get to rewrite the
// ledger's account of a close it did not cause.
func TestSupersedeTurnClaimsKeepsAnExistingCause(t *testing.T) {
	// Arrange.
	m := newSupersedeTestManager(t)
	openClaim(t, m, "/ws", "s1", "ka_1")
	closeClaim(t, m, "/ws", "s1", "ka_1", 0, TurnCloseRestartInterrupted)

	// Act.
	_, attributed, err := m.SupersedeTurnClaims("/ws", "s1", []string{"ka_1"}, TurnCloseCauseSupersededByRewind)

	// Assert.
	if err != nil {
		t.Fatalf("SupersedeTurnClaims: %v", err)
	}
	if len(attributed) != 0 {
		t.Fatalf("attributed = %v, want nothing: the claim already carried a cause", attributed)
	}
	if _, cause := claimEnd(t, m, "/ws", "ka_1"); cause != TurnCloseRestartInterrupted {
		t.Fatalf("end_cause = %q, want the original %q", cause, TurnCloseRestartInterrupted)
	}
}

// A REPLAYED TurnEnded AFTER SUPERSESSION STAYS IDEMPOTENT. Attribution
// preserves end_seq and end_event_session_id, which is exactly what
// recordTurnEnd matches a replay on, so the boundary replayed off the store
// once the subscription reopens is still recognized rather than rejected as a
// contradiction.
func TestSupersedeTurnClaimsLeavesAReplayedTurnEndIdempotent(t *testing.T) {
	// Arrange.
	m := openTurnClaimManager(t, filepath.Join(t.TempDir(), "state.db"))
	t.Cleanup(func() { m.Close() })
	resolveTurnClaim(t, m, turnClaimEvent(true, 10, "ka_1"))
	end := turnClaimEvent(false, 11, "ka_1")
	resolveTurnClaim(t, m, end)
	if _, _, err := m.SupersedeTurnClaims("ws", "daemon-session", []string{"ka_1"}, TurnCloseCauseSupersededByRewind); err != nil {
		t.Fatalf("SupersedeTurnClaims: %v", err)
	}

	// Act.
	_, _, replayed := resolveTurnClaim(t, m, end)

	// Assert.
	if !replayed {
		t.Fatal("replayed TurnEnded after supersession was not recognized as a replay")
	}
}

// closeClaim ends a claim directly, which is what a TurnEnded (empty cause) or
// a daemon-side close (non-empty cause) leaves behind.
func closeClaim(t *testing.T, m *Manager, workspace, sessionID, turnID string, endSeq int64, cause string) {
	t.Helper()
	if _, err := m.db.Exec(`UPDATE turn_lifecycle_claim
		SET end_seq=?, end_event_session_id=?, end_cause=?
		WHERE workspace=? AND claimant_session_id=? AND turn_id=?`,
		endSeq, sessionID, cause, workspace, sessionID, turnID); err != nil {
		t.Fatalf("close claim %s: %v", turnID, err)
	}
}

// claimEnd reads the recorded boundary and its attribution.
func claimEnd(t *testing.T, m *Manager, workspace, turnID string) (int64, string) {
	t.Helper()
	var endSeq int64
	var cause string
	if err := m.db.QueryRow(
		`SELECT end_seq, end_cause FROM turn_lifecycle_claim WHERE workspace=? AND turn_id=?`,
		workspace, turnID).Scan(&endSeq, &cause); err != nil {
		t.Fatalf("read claim end for %s: %v", turnID, err)
	}
	return endSeq, cause
}

// openClaim inserts one active claim directly, which is what a TurnStarted
// leaves behind.
func openClaim(t *testing.T, m *Manager, workspace, sessionID, turnID string) {
	t.Helper()
	if _, err := m.db.Exec(`INSERT INTO turn_lifecycle_claim
		(workspace, claimant_session_id, turn_id, start_seq, start_event_session_id)
		VALUES (?,?,?,?,?)`, workspace, sessionID, turnID, 1, sessionID); err != nil {
		t.Fatalf("open claim %s: %v", turnID, err)
	}
}

// activeClaimIDs lists the turn ids whose claims are still open.
func activeClaimIDs(t *testing.T, m *Manager, workspace, sessionID string) []string {
	t.Helper()
	rows, err := m.db.Query(`SELECT turn_id FROM turn_lifecycle_claim
		WHERE workspace=? AND claimant_session_id=? AND end_seq IS NULL ORDER BY turn_id`,
		workspace, sessionID)
	if err != nil {
		t.Fatalf("list active claims: %v", err)
	}
	defer rows.Close()
	var out []string
	for rows.Next() {
		var id string
		if err := rows.Scan(&id); err != nil {
			t.Fatalf("scan active claim: %v", err)
		}
		out = append(out, id)
	}
	return out
}
