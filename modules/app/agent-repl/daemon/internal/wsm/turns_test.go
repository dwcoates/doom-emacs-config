package wsm

import (
	"context"
	"errors"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/feedid"
)

func TestPutTurnRoundTripsTheDurableOrigin(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	turn := Turn{ID: NewTurnID(), Workspace: ws.ID, Text: "do it", Origin: "emacs", StartedAt: instant}

	// Act
	if err := s.PutTurn(context.Background(), turn); err != nil {
		t.Fatalf("PutTurn: %v", err)
	}
	got, err := s.OpenTurns(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("OpenTurns: %v", err)
	}
	if len(got) != 1 {
		t.Fatalf("loaded %d open turns, want 1", len(got))
	}
	if got[0].Origin != "emacs" || got[0].Text != "do it" || !got[0].StartedAt.Equal(instant) {
		t.Fatalf("turn = %+v, want %+v", got[0], turn)
	}
}

func TestPutTurnRoundTripsTheOutputAddress(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	lease := NewLeaseID()
	parent := feedid.Ref{WS: ws.ID, Feed: feedid.Feed{Merge: &lease}, Row: feedid.RowKey{Kind: feedid.KindMergeHead, ID: string(lease)}}
	address := OutputAddress{Feed: feedid.Feed{Merge: &lease}, Parent: &parent}

	// Act
	if err := s.PutTurn(context.Background(), Turn{
		ID: NewTurnID(), Workspace: ws.ID, Origin: "merge", Address: &address, StartedAt: instant,
	}); err != nil {
		t.Fatalf("PutTurn: %v", err)
	}
	got, err := s.OpenTurns(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("OpenTurns: %v", err)
	}
	restored := got[0].Address
	if restored == nil || restored.Feed.Merge == nil || *restored.Feed.Merge != lease {
		t.Fatalf("address feed = %+v, want the merge sub-feed", restored)
	}
	if restored.Parent == nil || restored.Parent.Row.Kind != feedid.KindMergeHead {
		t.Fatalf("address parent = %+v, want the merge head row", restored.Parent)
	}
}

func TestPutTurnRoundTripsTheAgentSubFeedAddress(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	address := OutputAddress{Feed: feedid.Feed{Agent: &conversationv1.AgentId{Value: "agent-3"}}}

	// Act
	if err := s.PutTurn(context.Background(), Turn{ID: NewTurnID(), Workspace: ws.ID, Origin: "bubble", Address: &address, StartedAt: instant}); err != nil {
		t.Fatalf("PutTurn: %v", err)
	}
	got, err := s.OpenTurns(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("OpenTurns: %v", err)
	}
	if got[0].Address == nil || got[0].Address.Feed.Agent.GetValue() != "agent-3" {
		t.Fatalf("address = %+v, want the agent sub-feed", got[0].Address)
	}
}

func TestPutTurnRecordsTheDisplacedCapture(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	if err := s.PutTurn(context.Background(), Turn{ID: NewTurnID(), Workspace: ws.ID, Origin: "webapp", Displaced: true, StartedAt: instant}); err != nil {
		t.Fatalf("PutTurn: %v", err)
	}
	got, err := s.OpenTurns(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("OpenTurns: %v", err)
	}
	if !got[0].Displaced {
		t.Fatalf("displaced = false, want the lease holder's capture recorded")
	}
}

func TestPutTurnRefusesAHalfWrittenClose(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s)
	how := CloseCompleted

	// Act
	err := s.PutTurn(context.Background(), Turn{ID: NewTurnID(), Workspace: ws.ID, Origin: "webapp", StartedAt: instant, Close: &how})

	// Assert
	if err == nil {
		t.Fatalf("PutTurn with a close kind and no instant succeeded")
	}
	if !loggedOperation(log, "daemon.wsm.put_turn", "error") {
		t.Fatalf("the refusal was not logged at error: %v", log.Records())
	}
}

func TestPutTurnRefusesAnUndeclaredClose(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	how := TurnClose(99)
	at := instant

	// Act
	err := s.PutTurn(context.Background(), Turn{ID: NewTurnID(), Workspace: ws.ID, Origin: "webapp", StartedAt: instant, ClosedAt: &at, Close: &how})

	// Assert
	if err == nil {
		t.Fatalf("PutTurn with an undeclared close succeeded")
	}
}

func TestCloseTurnStampsTheTerminal(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	turn := openTurn(t, s, ws.ID)
	at := instant.Add(time.Minute)

	// Act
	if err := s.CloseTurn(context.Background(), turn, at, CloseKilled); err != nil {
		t.Fatalf("CloseTurn: %v", err)
	}

	// Assert
	got, err := s.OpenTurns(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("OpenTurns: %v", err)
	}
	if len(got) != 0 {
		t.Fatalf("a closed turn is still open: %+v", got)
	}
	if kind := scalar[int](t, s, `SELECT close_kind FROM turns WHERE id = ?`, turn); kind != int(CloseKilled) {
		t.Fatalf("close kind = %d, want %d", kind, int(CloseKilled))
	}
}

func TestCloseTurnRefusesAnUnknownTurn(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	err := s.CloseTurn(context.Background(), TurnID("absent"), instant, CloseCompleted)

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("CloseTurn = %v, want ErrNotFound", err)
	}
}

func TestCloseTurnRefusesAnUndeclaredClose(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s)
	turn := openTurn(t, s, ws.ID)

	// Act
	err := s.CloseTurn(context.Background(), turn, instant, TurnClose(99))

	// Assert
	if err == nil {
		t.Fatalf("CloseTurn with an undeclared close succeeded")
	}
	if !loggedOperation(log, "daemon.wsm.close_turn", "error") {
		t.Fatalf("the refusal was not logged at error: %v", log.Records())
	}
}

func TestOpenTurnsFailsWholeOnAHalfWrittenClose(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	broken := openTurn(t, s, ws.ID)
	corrupt(t, s, `UPDATE turns SET close_kind = ? WHERE id = ?`, int(CloseCompleted), broken)

	// Act
	got, err := s.OpenTurns(context.Background(), ws.ID)

	// Assert
	var refusal *DecodeError
	if !errors.As(err, &refusal) || refusal.Table != "turns" || refusal.Field != "close" {
		t.Fatalf("OpenTurns = %v, want a *DecodeError naming turns.close", err)
	}
	if got != nil {
		t.Fatalf("loaded %d turns alongside the refusal, want none", len(got))
	}
}

func TestHasTurnsIsFalseForAWorkspaceThatNeverTookATurn(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	has, err := s.HasTurns(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("HasTurns: %v", err)
	}
	if has {
		t.Fatalf("HasTurns = true, want false for a workspace with no turn rows")
	}
}

func TestHasTurnsIsTrueForAnOpenTurn(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.PutTurn(context.Background(), Turn{ID: NewTurnID(), Workspace: ws.ID, Origin: "emacs", StartedAt: instant}); err != nil {
		t.Fatalf("PutTurn: %v", err)
	}

	// Act
	has, err := s.HasTurns(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("HasTurns: %v", err)
	}
	if !has {
		t.Fatalf("HasTurns = false, want true once a turn is recorded")
	}
}

func TestHasTurnsIsTrueForAClosedTurn(t *testing.T) {
	// Arrange: the question is whether the workspace was EVER engaged, so a
	// turn that has since ended answers it exactly as an open one does.
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	turn := Turn{ID: NewTurnID(), Workspace: ws.ID, Origin: "emacs", StartedAt: instant}
	if err := s.PutTurn(context.Background(), turn); err != nil {
		t.Fatalf("PutTurn: %v", err)
	}
	if err := s.CloseTurn(context.Background(), turn.ID, instant.Add(time.Minute), CloseCompleted); err != nil {
		t.Fatalf("CloseTurn: %v", err)
	}

	// Act
	has, err := s.HasTurns(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("HasTurns: %v", err)
	}
	if !has {
		t.Fatalf("HasTurns = false, want true for a workspace whose only turn has closed")
	}
}

func TestHasTurnsIsScopedPerWorkspace(t *testing.T) {
	// Arrange: another workspace's turn is not this workspace's engagement.
	s, _ := testStore(t)
	engaged := testWorkspace(t, s)
	quiet := testWorkspaceNamed(t, s, "quiet")
	if err := s.PutTurn(context.Background(), Turn{ID: NewTurnID(), Workspace: engaged.ID, Origin: "emacs", StartedAt: instant}); err != nil {
		t.Fatalf("PutTurn: %v", err)
	}

	// Act
	has, err := s.HasTurns(context.Background(), quiet.ID)

	// Assert
	if err != nil {
		t.Fatalf("HasTurns: %v", err)
	}
	if has {
		t.Fatalf("HasTurns = true, want false for a workspace whose sibling took the turn")
	}
}

func TestClaimIdempotencyKeyMintsOnFirstClaim(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	turn := NewTurnID()

	// Act
	existing, err := s.ClaimIdempotencyKey(context.Background(), ws.ID, "key-1", turn)

	// Assert
	if err != nil {
		t.Fatalf("ClaimIdempotencyKey: %v", err)
	}
	if existing != nil {
		t.Fatalf("existing = %q on a first claim, want nil", *existing)
	}
}

func TestClaimIdempotencyKeyReturnsTheClaimedTurn(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	first := NewTurnID()
	if _, err := s.ClaimIdempotencyKey(context.Background(), ws.ID, "key-1", first); err != nil {
		t.Fatalf("ClaimIdempotencyKey: %v", err)
	}

	// Act
	existing, err := s.ClaimIdempotencyKey(context.Background(), ws.ID, "key-1", NewTurnID())

	// Assert
	if err != nil {
		t.Fatalf("ClaimIdempotencyKey: %v", err)
	}
	if existing == nil || *existing != first {
		t.Fatalf("existing = %v, want %q", existing, first)
	}
}

func TestClaimIdempotencyKeyIsScopedPerWorkspace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	first := testWorkspace(t, s)
	second := testWorkspace(t, s)
	if _, err := s.ClaimIdempotencyKey(context.Background(), first.ID, "key-1", NewTurnID()); err != nil {
		t.Fatalf("ClaimIdempotencyKey: %v", err)
	}

	// Act
	existing, err := s.ClaimIdempotencyKey(context.Background(), second.ID, "key-1", NewTurnID())

	// Assert
	if err != nil {
		t.Fatalf("ClaimIdempotencyKey: %v", err)
	}
	if existing != nil {
		t.Fatalf("existing = %q across workspaces, want nil", *existing)
	}
}

func TestClaimIdempotencyKeyRefusesAnEmptyKey(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	_, err := s.ClaimIdempotencyKey(context.Background(), ws.ID, "", NewTurnID())

	// Assert
	if err == nil {
		t.Fatalf("ClaimIdempotencyKey with an empty key succeeded")
	}
	if !loggedOperation(log, "daemon.wsm.claim_idempotency_key", "error") {
		t.Fatalf("the refusal was not logged at error: %v", log.Records())
	}
}

func TestCloseOrphansClosesEveryTerminallessTurn(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	first := openTurn(t, s, ws.ID)
	second := openTurn(t, s, ws.ID)

	// Act
	report, err := s.CloseOrphans(context.Background(), ws.ID, instant)

	// Assert
	if err != nil {
		t.Fatalf("CloseOrphans: %v", err)
	}
	if len(report.Turns) != 2 {
		t.Fatalf("closed %d turns, want 2", len(report.Turns))
	}
	for _, turn := range []TurnID{first, second} {
		if kind := scalar[int](t, s, `SELECT close_kind FROM turns WHERE id = ?`, turn); kind != int(CloseOrphaned) {
			t.Fatalf("turn %q close kind = %d, want %d", turn, kind, int(CloseOrphaned))
		}
	}
}

func TestCloseOrphansStampsEngagement(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.PutSession(context.Background(), Session{Workspace: ws.ID, HostSessionID: "host-1", StartedAt: instant, LastEngagementAt: instant}); err != nil {
		t.Fatalf("PutSession: %v", err)
	}
	openTurn(t, s, ws.ID)
	at := instant.Add(time.Hour)

	// Act
	if _, err := s.CloseOrphans(context.Background(), ws.ID, at); err != nil {
		t.Fatalf("CloseOrphans: %v", err)
	}

	// Assert
	session, _, err := s.Session(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("Session: %v", err)
	}
	if !session.LastEngagementAt.Equal(at) {
		t.Fatalf("last engagement = %v, want %v", session.LastEngagementAt, at)
	}
}

func TestCloseOrphansLeavesHoldsStanding(t *testing.T) {
	// Arrange — holds survive a shutdown; the orphan close touches only turns.
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	openTurn(t, s, ws.ID)
	standingHold(t, s, ws.ID)

	// Act
	if _, err := s.CloseOrphans(context.Background(), ws.ID, instant); err != nil {
		t.Fatalf("CloseOrphans: %v", err)
	}

	// Assert
	held, err := s.HeldPrompts(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("HeldPrompts: %v", err)
	}
	if len(held) != 1 {
		t.Fatalf("%d holds survived the orphan close, want 1", len(held))
	}
}

func TestCloseOrphansIsOneTransaction(t *testing.T) {
	// Arrange — a corrupt turn row aborts the read inside the transaction, so a
	// crash mid-teardown can never leave half the bookkeeping done.
	s, log := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.PutSession(context.Background(), Session{Workspace: ws.ID, HostSessionID: "host-1", StartedAt: instant, LastEngagementAt: instant}); err != nil {
		t.Fatalf("PutSession: %v", err)
	}
	sound := openTurn(t, s, ws.ID)
	broken := openTurn(t, s, ws.ID)
	corrupt(t, s, `UPDATE turns SET close_kind = ? WHERE id = ?`, int(CloseCompleted), broken)

	// Act
	report, err := s.CloseOrphans(context.Background(), ws.ID, instant.Add(time.Hour))

	// Assert
	var refusal *DecodeError
	if !errors.As(err, &refusal) {
		t.Fatalf("CloseOrphans = %v, want a *DecodeError", err)
	}
	if len(report.Turns) != 0 {
		t.Fatalf("report closed %d turns alongside the refusal, want none", len(report.Turns))
	}
	if !isNull(t, s, `SELECT closed_at FROM turns WHERE id = ?`, sound) {
		t.Fatalf("the sound turn was closed despite the aborted transaction")
	}
	session, _, err := s.Session(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("Session: %v", err)
	}
	if !session.LastEngagementAt.Equal(instant) {
		t.Fatalf("engagement = %v, want the untouched %v", session.LastEngagementAt, instant)
	}
	if !loggedOperation(log, "daemon.wsm.close_orphans", "error") {
		t.Fatalf("the aborted teardown was not logged at error: %v", log.Records())
	}
}

func TestCloseOrphansReportsNothingWhenEveryTurnIsClosed(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	turn := openTurn(t, s, ws.ID)
	if err := s.CloseTurn(context.Background(), turn, instant, CloseCompleted); err != nil {
		t.Fatalf("CloseTurn: %v", err)
	}

	// Act
	report, err := s.CloseOrphans(context.Background(), ws.ID, instant)

	// Assert
	if err != nil {
		t.Fatalf("CloseOrphans: %v", err)
	}
	if len(report.Turns) != 0 {
		t.Fatalf("closed %d turns, want none", len(report.Turns))
	}
}

// openTurn records one turn with no terminal and returns its id.
func openTurn(t *testing.T, s *store, ws WorkspaceID) TurnID {
	t.Helper()
	turn := NewTurnID()
	if err := s.PutTurn(context.Background(), Turn{ID: turn, Workspace: ws, Text: "running", Origin: "webapp", StartedAt: instant}); err != nil {
		t.Fatalf("PutTurn: %v", err)
	}
	return turn
}

func TestAllDisplacedTurnsFindsAClosedDisplacedTurn(t *testing.T) {
	// Arrange: a displaced turn already closed, which is what a merge's
	// capture leaves behind — it ends the turn it takes.
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	turn := Turn{ID: NewTurnID(), Workspace: ws.ID, Text: "carry on", Origin: "emacs", Displaced: true, StartedAt: instant}
	if err := s.PutTurn(context.Background(), turn); err != nil {
		t.Fatalf("PutTurn: %v", err)
	}
	if err := s.CloseTurn(context.Background(), turn.ID, instant, CloseKilled); err != nil {
		t.Fatalf("CloseTurn: %v", err)
	}

	// Act
	got, err := s.AllDisplacedTurns(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("AllDisplacedTurns: %v", err)
	}
	if len(got) != 1 || got[0].ID != turn.ID || got[0].Text != "carry on" {
		t.Fatalf("displaced turns = %+v, want the one closed displaced record", got)
	}
}

func TestAllDisplacedTurnsSkipsAnUnmarkedTurn(t *testing.T) {
	// Arrange: an ordinary open turn, never displaced.
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.PutTurn(context.Background(), Turn{
		ID: NewTurnID(), Workspace: ws.ID, Text: "do it", Origin: "emacs", StartedAt: instant,
	}); err != nil {
		t.Fatalf("PutTurn: %v", err)
	}

	// Act
	got, err := s.AllDisplacedTurns(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("AllDisplacedTurns: %v", err)
	}
	if len(got) != 0 {
		t.Fatalf("displaced turns = %+v, want none", got)
	}
}

func TestClaimDisplacedTurnClearsTheMark(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	turn := Turn{ID: NewTurnID(), Workspace: ws.ID, Text: "carry on", Origin: "emacs", Displaced: true, StartedAt: instant}
	if err := s.PutTurn(context.Background(), turn); err != nil {
		t.Fatalf("PutTurn: %v", err)
	}

	// Act
	if claimed, err := s.ClaimDisplacedTurn(context.Background(), turn.ID, instant); err != nil || !claimed.Claimed {
		t.Fatalf("ClaimDisplacedTurn = (%v, %v), want (true, nil)", claimed, err)
	}
	got, err := s.AllDisplacedTurns(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("AllDisplacedTurns: %v", err)
	}
	if len(got) != 0 {
		t.Fatalf("displaced turns after the claim = %+v, want none", got)
	}
}

func TestClaimDisplacedTurnClosesAnOpenTurn(t *testing.T) {
	// Arrange: the turn's kill never landed, so its record is still open.
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	turn := Turn{ID: NewTurnID(), Workspace: ws.ID, Text: "carry on", Origin: "emacs", Displaced: true, StartedAt: instant}
	if err := s.PutTurn(context.Background(), turn); err != nil {
		t.Fatalf("PutTurn: %v", err)
	}

	// Act
	if claimed, err := s.ClaimDisplacedTurn(context.Background(), turn.ID, instant); err != nil || !claimed.Claimed {
		t.Fatalf("ClaimDisplacedTurn = (%v, %v), want (true, nil)", claimed, err)
	}
	open, err := s.OpenTurns(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("OpenTurns: %v", err)
	}
	if len(open) != 0 {
		t.Fatalf("open turns after the claim = %+v, want the record closed", open)
	}
}

func TestClaimDisplacedTurnKeepsAnExistingClose(t *testing.T) {
	// Arrange: a turn already closed as KILLED by the capture.
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	turn := Turn{ID: NewTurnID(), Workspace: ws.ID, Text: "carry on", Origin: "emacs", Displaced: true, StartedAt: instant}
	if err := s.PutTurn(context.Background(), turn); err != nil {
		t.Fatalf("PutTurn: %v", err)
	}
	if err := s.CloseTurn(context.Background(), turn.ID, instant, CloseKilled); err != nil {
		t.Fatalf("CloseTurn: %v", err)
	}

	// Act
	if claimed, err := s.ClaimDisplacedTurn(context.Background(), turn.ID, instant.Add(time.Minute)); err != nil || !claimed.Claimed {
		t.Fatalf("ClaimDisplacedTurn = (%v, %v), want (true, nil)", claimed, err)
	}
	var kind int
	if err := s.db().QueryRowContext(context.Background(), `SELECT close_kind FROM turns WHERE id = ?`, turn.ID).Scan(&kind); err != nil {
		t.Fatalf("read the claimed turn's close: %v", err)
	}

	// Assert
	if TurnClose(kind) != CloseKilled {
		t.Fatalf("the claimed turn's close = %v, want the kill it already had", TurnClose(kind))
	}
}

func TestClaimDisplacedTurnAnswersFalseForAnUnknownTurn(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	claimed, err := s.ClaimDisplacedTurn(context.Background(), NewTurnID(), instant)

	// Assert
	if err != nil || claimed.Claimed {
		t.Fatalf("ClaimDisplacedTurn on an unknown turn = (%v, %v), want (false, nil): nobody owns it", claimed, err)
	}
}

func TestASecondClaimOfTheSameDisplacedTurnAnswersFalse(t *testing.T) {
	// Arrange: a claimed record, as the second owner would find it.
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	turn := Turn{ID: NewTurnID(), Workspace: ws.ID, Text: "carry on", Origin: "emacs", Displaced: true, StartedAt: instant}
	if err := s.PutTurn(context.Background(), turn); err != nil {
		t.Fatalf("PutTurn: %v", err)
	}
	if claimed, err := s.ClaimDisplacedTurn(context.Background(), turn.ID, instant); err != nil || !claimed.Claimed {
		t.Fatalf("the first claim = (%v, %v), want (true, nil)", claimed, err)
	}

	// Act
	claimed, err := s.ClaimDisplacedTurn(context.Background(), turn.ID, instant)

	// Assert
	if err != nil || claimed.Claimed {
		t.Fatalf("the second claim = (%v, %v), want (false, nil): one owner per record", claimed, err)
	}
}

// THE WORKSPACE'S LAST-ACTIVITY STAMP. A turn write IS activity, and the turn
// writes advance the workspace's last_activity_at in the same transaction so
// the roster when-column reflects when the workspace last did real work — and
// so the column can never be stamped at compose or select time.

func TestPutTurnStampsTheWorkspacesLastActivity(t *testing.T) {
	// Arrange — a workspace that has never taken a turn.
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act — a turn lands.
	if err := s.PutTurn(context.Background(), Turn{ID: NewTurnID(), Workspace: ws.ID, Text: "go", Origin: "emacs", StartedAt: instant}); err != nil {
		t.Fatalf("PutTurn: %v", err)
	}

	// Assert — last activity is the turn's start.
	got, err := s.Workspace(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if got.LastActivityAt == nil || !got.LastActivityAt.Equal(instant) {
		t.Fatalf("last activity = %v, want the turn's start %v", got.LastActivityAt, instant)
	}
}

func TestCloseTurnStampsTheWorkspacesLastActivity(t *testing.T) {
	// Arrange — a turn open since `instant`.
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	turn := NewTurnID()
	if err := s.PutTurn(context.Background(), Turn{ID: turn, Workspace: ws.ID, Text: "go", Origin: "emacs", StartedAt: instant}); err != nil {
		t.Fatalf("PutTurn: %v", err)
	}
	settled := instant.Add(time.Hour)

	// Act — the response settles an hour later.
	if err := s.CloseTurn(context.Background(), turn, settled, CloseCompleted); err != nil {
		t.Fatalf("CloseTurn: %v", err)
	}

	// Assert — last activity advanced to the close instant.
	got, err := s.Workspace(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if got.LastActivityAt == nil || !got.LastActivityAt.Equal(settled) {
		t.Fatalf("last activity = %v, want the close instant %v", got.LastActivityAt, settled)
	}
}

// TestPutTurnDoesNotPullLastActivityBackward pins the MONOTONIC guard: a fleet
// rollout re-PUTs an in-flight turn to mark it displaced, carrying the turn's
// ORIGINAL (older) start. That re-put must not drag the stamp back to a past
// instant and mis-report a handover as fresh activity.
func TestPutTurnDoesNotPullLastActivityBackward(t *testing.T) {
	// Arrange — a recent turn set the stamp.
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	recent := NewTurnID()
	newer := instant.Add(time.Hour)
	if err := s.PutTurn(context.Background(), Turn{ID: recent, Workspace: ws.ID, Text: "recent", Origin: "emacs", StartedAt: newer}); err != nil {
		t.Fatalf("PutTurn recent: %v", err)
	}

	// Act — an older turn is re-PUT (as a displacement mark would).
	if err := s.PutTurn(context.Background(), Turn{ID: NewTurnID(), Workspace: ws.ID, Text: "old", Origin: "emacs", StartedAt: instant, Displaced: true}); err != nil {
		t.Fatalf("PutTurn old: %v", err)
	}

	// Assert — the stamp stayed at the newer instant.
	got, err := s.Workspace(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if got.LastActivityAt == nil || !got.LastActivityAt.Equal(newer) {
		t.Fatalf("last activity = %v, want it to stay at the newer instant %v", got.LastActivityAt, newer)
	}
}

// THE DOOR'S DURABLE HALF. A turn closes only through the prompt queue's door,
// so PutTurn never writes a close, and the reads a feed replay draws endings
// from answer the recorded close.

func TestPutTurnRefusesARecordCarryingAWholeClose(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s)
	how := CloseCompleted
	at := instant

	// Act
	err := s.PutTurn(context.Background(), Turn{ID: NewTurnID(), Workspace: ws.ID, Origin: "webapp", StartedAt: instant, ClosedAt: &at, Close: &how})

	// Assert
	if err == nil {
		t.Fatalf("PutTurn with a whole close succeeded; a turn closes only through the door")
	}
	if !loggedOperation(log, "daemon.wsm.put_turn", "error") {
		t.Fatalf("the refusal was not logged at error: %v", log.Records())
	}
}

func TestPutTurnKeepsTheCloseOfARecordThatClosedSinceItWasRead(t *testing.T) {
	// Arrange: a record read while open, then closed, then put again (the
	// displaced capture's shape).
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	turn := Turn{ID: NewTurnID(), Workspace: ws.ID, Origin: "emacs", StartedAt: instant}
	if err := s.PutTurn(context.Background(), turn); err != nil {
		t.Fatalf("PutTurn: %v", err)
	}
	if err := s.CloseTurn(context.Background(), turn.ID, instant, CloseKilled); err != nil {
		t.Fatalf("CloseTurn: %v", err)
	}
	turn.Displaced = true

	// Act
	err := s.PutTurn(context.Background(), turn)

	// Assert
	if err != nil {
		t.Fatalf("PutTurn: %v", err)
	}
	if kind := scalar[int](t, s, `SELECT close_kind FROM turns WHERE id = ?`, turn.ID); kind != int(CloseKilled) {
		t.Fatalf("close kind after the re-put = %d, want the kill it had (%d)", kind, int(CloseKilled))
	}
}

func TestCloseTurnAcceptsTheAgentDiedClose(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	turn := openTurn(t, s, ws.ID)

	// Act
	err := s.CloseTurn(context.Background(), turn, instant, CloseAgentDied)

	// Assert
	if err != nil {
		t.Fatalf("CloseTurn(CloseAgentDied): %v", err)
	}
	if kind := scalar[int](t, s, `SELECT close_kind FROM turns WHERE id = ?`, turn); kind != int(CloseAgentDied) {
		t.Fatalf("close kind = %d, want %d", kind, int(CloseAgentDied))
	}
}

func TestTurnCloseFailed(t *testing.T) {
	tests := []struct {
		name string
		how  TurnClose
		want bool
	}{
		{name: "completed", how: CloseCompleted, want: false},
		{name: "failed", how: CloseFailed, want: true},
		{name: "killed", how: CloseKilled, want: false},
		{name: "orphaned", how: CloseOrphaned, want: false},
		{name: "agent died", how: CloseAgentDied, want: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := tc.how.Failed()

			// Assert
			if got != tc.want {
				t.Fatalf("Failed() = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestTurnClosesAnswersAClosedTurnsClose(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	turn := openTurn(t, s, ws.ID)
	at := instant.Add(time.Minute)
	if err := s.CloseTurn(context.Background(), turn, at, CloseOrphaned); err != nil {
		t.Fatalf("CloseTurn: %v", err)
	}

	// Act
	got, err := s.TurnCloses(context.Background(), ws.ID, []TurnID{turn})

	// Assert
	if err != nil {
		t.Fatalf("TurnCloses: %v", err)
	}
	if close, ok := got[turn]; !ok || close.How != CloseOrphaned || !close.At.Equal(at) {
		t.Fatalf("TurnCloses = %+v, want %s closed orphaned at %v", got, turn, at)
	}
}

func TestTurnClosesOmitsAnOpenTurn(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	turn := openTurn(t, s, ws.ID)

	// Act
	got, err := s.TurnCloses(context.Background(), ws.ID, []TurnID{turn})

	// Assert
	if err != nil {
		t.Fatalf("TurnCloses: %v", err)
	}
	if len(got) != 0 {
		t.Fatalf("TurnCloses = %+v, want an open turn absent", got)
	}
}

func TestTurnClosesOmitsAnotherWorkspacesTurn(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	other := testWorkspaceNamed(t, s, "other")
	turn := openTurn(t, s, other.ID)
	if err := s.CloseTurn(context.Background(), turn, instant, CloseCompleted); err != nil {
		t.Fatalf("CloseTurn: %v", err)
	}

	// Act
	got, err := s.TurnCloses(context.Background(), ws.ID, []TurnID{turn})

	// Assert
	if err != nil {
		t.Fatalf("TurnCloses: %v", err)
	}
	if len(got) != 0 {
		t.Fatalf("TurnCloses = %+v, want another workspace's turn absent", got)
	}
}

func TestClaimDisplacedTurnReportsItClosedAnOpenTurnAsOrphaned(t *testing.T) {
	// Arrange: the turn's kill never landed, so its record is still open.
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	turn := Turn{ID: NewTurnID(), Workspace: ws.ID, Text: "carry on", Origin: "emacs", Displaced: true, StartedAt: instant}
	if err := s.PutTurn(context.Background(), turn); err != nil {
		t.Fatalf("PutTurn: %v", err)
	}

	// Act
	claim, err := s.ClaimDisplacedTurn(context.Background(), turn.ID, instant)

	// Assert
	if err != nil || !claim.Claimed || !claim.Closed {
		t.Fatalf("ClaimDisplacedTurn = (%+v, %v), want claimed and closed", claim, err)
	}
	if kind := scalar[int](t, s, `SELECT close_kind FROM turns WHERE id = ?`, turn.ID); kind != int(CloseOrphaned) {
		t.Fatalf("close kind = %d, want orphaned (%d)", kind, int(CloseOrphaned))
	}
}

func TestClaimDisplacedTurnReportsItLeftAnExistingCloseStanding(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	turn := Turn{ID: NewTurnID(), Workspace: ws.ID, Text: "carry on", Origin: "emacs", Displaced: true, StartedAt: instant}
	if err := s.PutTurn(context.Background(), turn); err != nil {
		t.Fatalf("PutTurn: %v", err)
	}
	if err := s.CloseTurn(context.Background(), turn.ID, instant, CloseKilled); err != nil {
		t.Fatalf("CloseTurn: %v", err)
	}

	// Act
	claim, err := s.ClaimDisplacedTurn(context.Background(), turn.ID, instant)

	// Assert
	if err != nil || !claim.Claimed || claim.Closed {
		t.Fatalf("ClaimDisplacedTurn = (%+v, %v), want claimed and not closed by the claim", claim, err)
	}
}
