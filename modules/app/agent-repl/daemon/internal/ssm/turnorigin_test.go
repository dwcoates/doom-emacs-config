package ssm

import (
	"database/sql"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
)

// openOriginTurn drives one machine-originated turn's START through the ledger,
// leaving an open claim exactly as a merge resolution, a workspace-create prompt
// or a keep-alive ping does.
func openOriginTurn(t *testing.T, m *Manager, seq uint64, id string) {
	t.Helper()
	if _, _, _, err := m.ResolveTurnLifecycle("ws", "daemon-session", "", turnClaimEvent(true, seq, id)); err != nil {
		t.Fatalf("open origin turn %q: %v", id, err)
	}
}

// claimEndCause reads one turn's durable end attribution.
func claimEndCause(t *testing.T, m *Manager, id string) (endSeq sql.NullInt64, cause string) {
	t.Helper()
	if err := m.db.QueryRow(`SELECT end_seq, end_cause FROM turn_lifecycle_claim
		WHERE workspace='ws' AND turn_id=?`, id).Scan(&endSeq, &cause); err != nil {
		t.Fatalf("read claim %q: %v", id, err)
	}
	return endSeq, cause
}

// THE WHOLE POINT: a turn whose origin terminated is closed even though no
// `TurnEnded` ever arrived for it, and the ledger says which origin fact did it.
func TestCloseOriginTurnsEndsAnOpenClaimAndNamesTheCause(t *testing.T) {
	tests := []struct {
		name   string
		turnID string
		cause  string
	}{
		{
			name:   "a merge run that terminated over its own resolution turn",
			turnID: "merge-resume:q_8ddc6f0f",
			cause:  TurnCloseMergeRunTerminal,
		},
		{
			name:   "a workspace-create prompt whose job settled",
			turnID: "workspace-create:workspace_commands_f0395485:0",
			cause:  TurnCloseWorkspaceCreateSettled,
		},
		{
			name:   "a keep-alive ping that outlived any ping duration",
			turnID: "ka_5a9fae9d8881ba6f",
			cause:  TurnCloseKeepAliveOverdue,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			m := openTurnClaimManager(t, filepath.Join(t.TempDir(), "state.db"))
			openOriginTurn(t, m, 4, tc.turnID)

			// Act.
			closed, err := m.CloseOriginTurns("ws", []string{tc.turnID}, tc.cause)

			// Assert.
			if err != nil {
				t.Fatalf("CloseOriginTurns: %v", err)
			}
			if !reflect.DeepEqual(closed, []string{tc.turnID}) {
				t.Fatalf("closed = %v, want [%s]", closed, tc.turnID)
			}
			endSeq, cause := claimEndCause(t, m, tc.turnID)
			if !endSeq.Valid || endSeq.Int64 != 0 || cause != tc.cause {
				t.Fatalf("claim end = (seq=%v valid=%v cause=%q), want the synthesized end_seq=0 attributed to %q",
					endSeq.Int64, endSeq.Valid, cause, tc.cause)
			}
		})
	}
}

// THE DERIVATION IS WHAT CONSUMERS READ, so the close has to move it. A claim
// retired in the table while turn liveness still holds the workspace live would
// leave hibernation refused and every later prompt queued exactly as before.
func TestCloseOriginTurnsLeavesTheWorkspaceIdle(t *testing.T) {
	// Arrange.
	m := openTurnClaimManager(t, filepath.Join(t.TempDir(), "state.db"))
	openOriginTurn(t, m, 4, "ka_overdue")
	before, err := m.TurnLiveness("ws")
	if err != nil {
		t.Fatalf("TurnLiveness before: %v", err)
	}
	if !before.Active() {
		t.Fatalf("liveness before = %s, want a turn in flight for the close to have anything to do", before)
	}

	// Act.
	if _, err := m.CloseOriginTurns("ws", []string{"ka_overdue"}, TurnCloseKeepAliveOverdue); err != nil {
		t.Fatalf("CloseOriginTurns: %v", err)
	}

	// Assert.
	after, err := m.TurnLiveness("ws")
	if err != nil {
		t.Fatalf("TurnLiveness after: %v", err)
	}
	if after.Active() {
		t.Fatalf("liveness after = %s, want no turn in flight; a retired row the derivation still holds open is the same wedge", after)
	}
}

// A REPLAY MUST NOT RESURRECT THE TURN. The claim is closed for its own claimant
// only, and a superseded workspace replays the same `turn_started` under a fresh
// one. The durable end keyed by the START'S STORE COORDINATE is what makes the
// replay reconstruct a matched pair instead of a new open claim.
func TestCloseOriginTurnsRecordsTheDurableEndAReplayConsults(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "state.db")
	m := openTurnClaimManager(t, path)
	openOriginTurn(t, m, 4, "workspace-create:job:0")
	if _, err := m.CloseOriginTurns("ws", []string{"workspace-create:job:0"}, TurnCloseWorkspaceCreateSettled); err != nil {
		t.Fatalf("CloseOriginTurns: %v", err)
	}

	// Act — a replacement claimant replays the very same start off the store.
	if _, _, _, err := m.ResolveTurnLifecycle("ws", "replacement-session", "",
		turnClaimEvent(true, 4, "workspace-create:job:0")); err != nil {
		t.Fatalf("replayed start under a replacement claimant: %v", err)
	}

	// Assert.
	liveness, err := m.TurnLiveness("ws")
	if err != nil {
		t.Fatalf("TurnLiveness: %v", err)
	}
	if liveness.Active() {
		t.Fatalf("liveness after replay = %s, want idle; the replayed start reopened a turn the origin already closed", liveness)
	}
}

// AN ALREADY-CLOSED CLAIM IS LEFT ALONE. The ordinary outcome is that the turn's
// own end got there first, and stamping the origin's cause onto that close would
// claim the daemon ended a turn that ended honestly.
func TestCloseOriginTurnsNeverRewritesAnHonestClose(t *testing.T) {
	// Arrange — the turn starts and genuinely ends.
	m := openTurnClaimManager(t, filepath.Join(t.TempDir(), "state.db"))
	openOriginTurn(t, m, 4, "ka_finished")
	if _, _, _, err := m.ResolveTurnLifecycle("ws", "daemon-session", "", turnClaimEvent(false, 5, "ka_finished")); err != nil {
		t.Fatalf("end origin turn: %v", err)
	}

	// Act.
	closed, err := m.CloseOriginTurns("ws", []string{"ka_finished"}, TurnCloseKeepAliveOverdue)

	// Assert.
	if err != nil {
		t.Fatalf("CloseOriginTurns: %v", err)
	}
	if len(closed) != 0 {
		t.Fatalf("closed = %v, want nothing; the turn's own end had already retired the claim", closed)
	}
	endSeq, cause := claimEndCause(t, m, "ka_finished")
	if endSeq.Int64 != 5 || cause != "" {
		t.Fatalf("claim end = (seq=%d cause=%q), want the real boundary at seq=5 with no origin attribution over it",
			endSeq.Int64, cause)
	}
}

// A turn id this workspace never claimed is nothing to close, and saying so is
// not an error: an origin may legitimately terminate over a turn that was never
// admitted.
func TestCloseOriginTurnsIgnoresAnUnknownTurn(t *testing.T) {
	// Arrange.
	m := openTurnClaimManager(t, filepath.Join(t.TempDir(), "state.db"))

	// Act.
	closed, err := m.CloseOriginTurns("ws", []string{"merge-resume:never-ran"}, TurnCloseMergeRunTerminal)

	// Assert.
	if err != nil || len(closed) != 0 {
		t.Fatalf("CloseOriginTurns = (%v, %v), want (nil, nil) for a turn this workspace holds no claim for", closed, err)
	}
}

// AN IDENTITY-LESS CLAIM IS NOT MATCHABLE BY ID. An empty turn id names every
// legacy claim in the workspace, including turns some other origin is running,
// so it is dropped rather than allowed to close them.
func TestCloseOriginTurnsRefusesToMatchOnAnEmptyIdentity(t *testing.T) {
	// Arrange — a legacy claim, carrying no turn id at all.
	m := openTurnClaimManager(t, filepath.Join(t.TempDir(), "state.db"))
	openOriginTurn(t, m, 4, "")

	// Act.
	closed, err := m.CloseOriginTurns("ws", []string{""}, TurnCloseMergeRunTerminal)

	// Assert.
	if err != nil || len(closed) != 0 {
		t.Fatalf("CloseOriginTurns = (%v, %v), want (nil, nil); an empty identity must match nothing", closed, err)
	}
	liveness, err := m.TurnLiveness("ws")
	if err != nil {
		t.Fatalf("TurnLiveness: %v", err)
	}
	if !liveness.Active() {
		t.Fatal("the legacy claim was closed by an empty identity; it cannot be told apart from any other legacy turn")
	}
}

// An unattributed close is indistinguishable from a lost one, so a caller with
// no lifecycle fact to name is refused rather than allowed to write a blank.
func TestCloseOriginTurnsRefusesAnUnattributedClose(t *testing.T) {
	// Arrange.
	m := openTurnClaimManager(t, filepath.Join(t.TempDir(), "state.db"))
	openOriginTurn(t, m, 4, "ka_overdue")

	// Act.
	_, err := m.CloseOriginTurns("ws", []string{"ka_overdue"}, "")

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "empty cause") {
		t.Fatalf("CloseOriginTurns with no cause = %v, want a refusal naming the missing cause", err)
	}
}

// A close that cannot say which workspace it is about could retire any
// workspace's claims, so the identity is required before anything is read.
func TestCloseOriginTurnsRefusesAnEmptyWorkspace(t *testing.T) {
	// Arrange.
	m := openTurnClaimManager(t, filepath.Join(t.TempDir(), "state.db"))

	// Act.
	_, err := m.CloseOriginTurns("", []string{"ka_overdue"}, TurnCloseKeepAliveOverdue)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "empty workspace") {
		t.Fatalf("CloseOriginTurns with no workspace = %v, want a refusal naming the missing workspace", err)
	}
}
