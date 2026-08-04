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
	superseded, err := m.SupersedeTurnClaims("/ws", "s1", []string{"ka_1"}, TurnCloseCauseSupersededByRewind)

	// Assert.
	if err != nil {
		t.Fatalf("SupersedeTurnClaims: %v", err)
	}
	if len(superseded) != 1 || superseded[0] != "ka_1" {
		t.Fatalf("superseded = %v, want [ka_1]", superseded)
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
	if _, err := m.SupersedeTurnClaims("/ws", "s1", []string{"ka_1"}, TurnCloseCauseSupersededByRewind); err != nil {
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

// AN ALREADY-CLOSED TURN IS A NO-OP. A rewind can legitimately name a ping
// whose claim the turn's own end already retired, which is the ordinary case.
func TestSupersedeTurnClaimsIgnoresAnAlreadyClosedTurn(t *testing.T) {
	// Arrange.
	m := newSupersedeTestManager(t)

	// Act.
	superseded, err := m.SupersedeTurnClaims("/ws", "s1", []string{"never_opened"}, TurnCloseCauseSupersededByRewind)

	// Assert.
	if err != nil {
		t.Fatalf("SupersedeTurnClaims over an unknown turn = %v, want a benign no-op", err)
	}
	if len(superseded) != 0 {
		t.Fatalf("superseded = %v, want nothing", superseded)
	}
}

// AN UNATTRIBUTED CLOSE IS REFUSED: it would be indistinguishable in the ledger
// from a claim that was simply lost.
func TestSupersedeTurnClaimsRefusesAnEmptyCause(t *testing.T) {
	// Arrange.
	m := newSupersedeTestManager(t)
	openClaim(t, m, "/ws", "s1", "ka_1")

	// Act.
	_, err := m.SupersedeTurnClaims("/ws", "s1", []string{"ka_1"}, "")

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
	if _, err := m.SupersedeTurnClaims("/ws", "s1", []string{"ka_1"}, TurnCloseCauseSupersededByRewind); err != nil {
		t.Fatalf("SupersedeTurnClaims: %v", err)
	}

	// Assert.
	active := activeClaimIDs(t, m, "/ws", "s1")
	if len(active) != 1 || active[0] != "real_turn" {
		t.Fatalf("active claims = %v, want only the turn the rewind did not name", active)
	}
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
