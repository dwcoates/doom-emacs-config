package wsm

import (
	"context"
	"errors"
	"testing"

	"agent-repld/internal/promptorigin"
)

// agentSent is PROMPT_ORIGIN_AGENT_SENT as the origin columns spell it.
const agentSent = "PROMPT_ORIGIN_AGENT_SENT"

func TestPutTurnRoundTripsTheSender(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	sender := testWorkspace(t, s)
	turn := NewTurnID()

	// Act
	if err := s.PutTurn(context.Background(), Turn{ID: turn, Workspace: ws.ID, Text: "x", Origin: agentSent, SentBy: &sender.ID, StartedAt: instant}); err != nil {
		t.Fatalf("PutTurn: %v", err)
	}
	got, err := s.Turn(context.Background(), ws.ID, turn)

	// Assert
	if err != nil || got.SentBy == nil || *got.SentBy != sender.ID {
		t.Fatalf("turn = (%+v, %v), want sender %s", got, err, sender.ID)
	}
}

func TestPutTurnRefusesASenderThatDisagreesWithTheOrigin(t *testing.T) {
	// Arrange
	s, logs := testStore(t)
	ws := testWorkspace(t, s)
	turn := NewTurnID()

	// Act
	err := s.PutTurn(context.Background(), Turn{ID: turn, Workspace: ws.ID, Text: "x", Origin: agentSent, StartedAt: instant})
	recorded, _ := s.RecordedTurns(context.Background(), ws.ID, []TurnID{turn})

	// Assert
	if !errors.Is(err, promptorigin.ErrSenderMismatch) {
		t.Fatalf("PutTurn = %v, want promptorigin.ErrSenderMismatch", err)
	}
	if recorded[turn] {
		t.Fatal("the refused turn was recorded")
	}
	if !loggedAt(logs, "daemon.wsm.put_turn") {
		t.Fatal("the refusal was not recorded under the operation")
	}
}

func TestATurnStoredWithASenderThatDisagreesWithItsOriginFailsTheRead(t *testing.T) {
	// Arrange: a row no write of this build produces.
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if _, err := s.db().ExecContext(context.Background(),
		`INSERT INTO turns (id, workspace_id, text, origin, displaced, started_at, sent_by) VALUES ('t1', ?, 'x', 'PROMPT_ORIGIN_USER_SENT', 0, 1, 'ws-sender')`, ws.ID); err != nil {
		t.Fatalf("insert: %v", err)
	}

	// Act
	_, err := s.Turn(context.Background(), ws.ID, "t1")

	// Assert
	var decode *DecodeError
	if !errors.As(err, &decode) || decode.Field != "sent_by" || !errors.Is(err, promptorigin.ErrSenderMismatch) {
		t.Fatalf("Turn = %v, want a sent_by decode error", err)
	}
}

func TestPutHeldPromptRoundTripsTheSender(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	sender := testWorkspace(t, s)
	turn := NewTurnID()

	// Act
	if err := s.PutHeldPrompt(context.Background(), HeldPrompt{
		Workspace: ws.ID, Turn: turn, Said: said("hello"), Origin: agentSent, SentBy: &sender.ID, QueuedAt: instant,
	}); err != nil {
		t.Fatalf("PutHeldPrompt: %v", err)
	}
	got, ok, err := s.HeldPromptByTurn(context.Background(), turn)

	// Assert
	if err != nil || !ok || got.SentBy == nil || *got.SentBy != sender.ID {
		t.Fatalf("hold = (%+v, %v, %v), want sender %s", got, ok, err, sender.ID)
	}
}

func TestPutHeldPromptRefusesASenderThatDisagreesWithTheOrigin(t *testing.T) {
	// Arrange
	s, logs := testStore(t)
	ws := testWorkspace(t, s)
	sender := WorkspaceID("ws-sender")
	turn := NewTurnID()

	// Act
	err := s.PutHeldPrompt(context.Background(), HeldPrompt{
		Workspace: ws.ID, Turn: turn, Said: said("hello"), Origin: "webapp", SentBy: &sender, QueuedAt: instant,
	})
	_, found, _ := s.HeldPromptByTurn(context.Background(), turn)

	// Assert
	if !errors.Is(err, promptorigin.ErrSenderMismatch) {
		t.Fatalf("PutHeldPrompt = %v, want promptorigin.ErrSenderMismatch", err)
	}
	if found {
		t.Fatal("the refused hold was recorded")
	}
	if !loggedAt(logs, "daemon.wsm.put_held_prompt") {
		t.Fatal("the refusal was not recorded under the operation")
	}
}

func TestTurnSendersAnswersOnlyTheTurnsAWorkspaceSent(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	sender := testWorkspace(t, s)
	sent, said := NewTurnID(), NewTurnID()
	for _, turn := range []Turn{
		{ID: sent, Workspace: ws.ID, Text: "x", Origin: agentSent, SentBy: &sender.ID, StartedAt: instant},
		{ID: said, Workspace: ws.ID, Text: "y", Origin: "PROMPT_ORIGIN_USER_SENT", StartedAt: instant},
	} {
		if err := s.PutTurn(context.Background(), turn); err != nil {
			t.Fatalf("PutTurn: %v", err)
		}
	}

	// Act
	got, err := s.TurnSenders(context.Background(), ws.ID, []TurnID{sent, said, "never-recorded"})

	// Assert
	if err != nil || len(got) != 1 || got[sent] != sender.ID {
		t.Fatalf("TurnSenders = (%v, %v), want only %s sent by %s", got, err, sent, sender.ID)
	}
}

func TestTurnSendersSurfacesAFailedRead(t *testing.T) {
	// Arrange
	s, logs := testStore(t)
	ws := testWorkspace(t, s)
	turn := NewTurnID()
	if err := s.PutTurn(context.Background(), Turn{ID: turn, Workspace: ws.ID, Text: "x", Origin: "PROMPT_ORIGIN_USER_SENT", StartedAt: instant}); err != nil {
		t.Fatalf("PutTurn: %v", err)
	}
	if _, err := s.db().ExecContext(context.Background(), `UPDATE turns SET sent_by = 'ws-sender' WHERE id = ?`, turn); err != nil {
		t.Fatalf("corrupt the row: %v", err)
	}

	// Act
	got, err := s.TurnSenders(context.Background(), ws.ID, []TurnID{turn})

	// Assert
	if !errors.Is(err, promptorigin.ErrSenderMismatch) || got != nil {
		t.Fatalf("TurnSenders = (%v, %v), want the decode failure and no answer", got, err)
	}
	if !loggedAt(logs, "daemon.wsm.turn_senders") {
		t.Fatal("the failed read was not recorded under the operation")
	}
}
