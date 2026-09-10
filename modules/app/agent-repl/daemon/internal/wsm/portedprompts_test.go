package wsm

import (
	"context"
	"testing"
)

func TestPutPortedPromptsRoundTripsTheConversation(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	rows := []PortedPrompt{{Workspace: ws.ID, Turn: NewTurnID(), Ordinal: 0, Text: "what is 2+2", Origin: "webapp", StartedAt: instant}}

	// Act
	if err := s.PutPortedPrompts(context.Background(), ws.ID, rows); err != nil {
		t.Fatalf("PutPortedPrompts: %v", err)
	}
	got, err := s.PortedPrompts(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("PortedPrompts: %v", err)
	}
	if len(got) != 1 || got[0].Text != "what is 2+2" {
		t.Fatalf("PortedPrompts() = %+v, want the one ported row", got)
	}
}

func TestPortedPromptsAnswersOldestFirst(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	first, second := NewTurnID(), NewTurnID()
	rows := []PortedPrompt{
		{Workspace: ws.ID, Turn: second, Ordinal: 1, Text: "second", Origin: "webapp", StartedAt: instant},
		{Workspace: ws.ID, Turn: first, Ordinal: 0, Text: "first", Origin: "webapp", StartedAt: instant},
	}

	// Act
	if err := s.PutPortedPrompts(context.Background(), ws.ID, rows); err != nil {
		t.Fatalf("PutPortedPrompts: %v", err)
	}
	got, err := s.PortedPrompts(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("PortedPrompts: %v", err)
	}
	if len(got) != 2 || got[0].Text != "first" || got[1].Text != "second" {
		t.Fatalf("PortedPrompts() = %+v, want oldest first", got)
	}
}

func TestPutPortedPromptsRefusesARowWithNoTurnID(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	rows := []PortedPrompt{{Workspace: ws.ID, Ordinal: 0, Text: "orphan", Origin: "webapp", StartedAt: instant}}

	// Act
	err := s.PutPortedPrompts(context.Background(), ws.ID, rows)

	// Assert
	if err == nil {
		t.Fatalf("PutPortedPrompts() = nil, want a refusal for a row with no turn id")
	}
}

func TestConversationPromptsCarriesTheWorkspacesOwnTurns(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.PutTurn(context.Background(), Turn{ID: NewTurnID(), Workspace: ws.ID, Text: "mine", Origin: "webapp", StartedAt: instant}); err != nil {
		t.Fatalf("PutTurn: %v", err)
	}

	// Act
	got, err := s.ConversationPrompts(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("ConversationPrompts: %v", err)
	}
	if len(got) != 1 || got[0].Text != "mine" {
		t.Fatalf("ConversationPrompts() = %+v, want the workspace's own turn", got)
	}
}

func TestConversationPromptsPutsTheInheritedRowsBeforeItsOwn(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.PutPortedPrompts(context.Background(), ws.ID, []PortedPrompt{
		{Workspace: ws.ID, Turn: NewTurnID(), Ordinal: 0, Text: "grandparent", Origin: "webapp", StartedAt: instant},
	}); err != nil {
		t.Fatalf("PutPortedPrompts: %v", err)
	}
	if err := s.PutTurn(context.Background(), Turn{ID: NewTurnID(), Workspace: ws.ID, Text: "mine", Origin: "webapp", StartedAt: instant}); err != nil {
		t.Fatalf("PutTurn: %v", err)
	}

	// Act
	got, err := s.ConversationPrompts(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("ConversationPrompts: %v", err)
	}
	if len(got) != 2 || got[0].Text != "grandparent" || got[1].Text != "mine" {
		t.Fatalf("ConversationPrompts() = %+v, want the inherited row before the workspace's own", got)
	}
}

func TestConversationPromptsNumbersTheOrdinalsContiguouslyFromZero(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.PutPortedPrompts(context.Background(), ws.ID, []PortedPrompt{
		{Workspace: ws.ID, Turn: NewTurnID(), Ordinal: 41, Text: "grandparent", Origin: "webapp", StartedAt: instant},
	}); err != nil {
		t.Fatalf("PutPortedPrompts: %v", err)
	}
	if err := s.PutTurn(context.Background(), Turn{ID: NewTurnID(), Workspace: ws.ID, Text: "mine", Origin: "webapp", StartedAt: instant}); err != nil {
		t.Fatalf("PutTurn: %v", err)
	}

	// Act
	got, err := s.ConversationPrompts(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("ConversationPrompts: %v", err)
	}
	if len(got) != 2 || got[0].Ordinal != 0 || got[1].Ordinal != 1 {
		t.Fatalf("ConversationPrompts() ordinals = %d and %d, want 0 and 1", got[0].Ordinal, got[1].Ordinal)
	}
}

func TestRemintPortedPromptsCarriesEveryTurnIDThroughTheMapping(t *testing.T) {
	// Arrange
	parent := NewTurnID()
	rows := []PortedPrompt{{Workspace: "parent-ws", Turn: parent, Ordinal: 7, Text: "ask", Origin: "webapp", StartedAt: instant}}

	// Act
	got, err := RemintPortedPrompts("child-ws", rows, func(old string) string { return "minted-" + old })

	// Assert
	if err != nil {
		t.Fatalf("RemintPortedPrompts: %v", err)
	}
	if len(got) != 1 || string(got[0].Turn) != "minted-"+string(parent) {
		t.Fatalf("RemintPortedPrompts() turn = %q, want the mapped id", got[0].Turn)
	}
}

func TestRemintPortedPromptsFilesTheCopyUnderTheChild(t *testing.T) {
	// Arrange
	rows := []PortedPrompt{{Workspace: "parent-ws", Turn: NewTurnID(), Ordinal: 7, Text: "ask", Origin: "webapp", StartedAt: instant}}

	// Act
	got, err := RemintPortedPrompts("child-ws", rows, func(old string) string { return old })

	// Assert
	if err != nil {
		t.Fatalf("RemintPortedPrompts: %v", err)
	}
	if got[0].Workspace != "child-ws" || got[0].Ordinal != 0 {
		t.Fatalf("RemintPortedPrompts() = %+v, want the child's workspace and a re-numbered ordinal", got[0])
	}
}

func TestRemintPortedPromptsRefusesAMappingThatAnswersNothing(t *testing.T) {
	// Arrange
	rows := []PortedPrompt{{Workspace: "parent-ws", Turn: NewTurnID(), Text: "ask", Origin: "webapp", StartedAt: instant}}

	// Act
	_, err := RemintPortedPrompts("child-ws", rows, func(string) string { return "" })

	// Assert
	if err == nil {
		t.Fatalf("RemintPortedPrompts() = nil, want a refusal when the mapping answers no id")
	}
}

func TestRemintPortedPromptsRefusesAMissingMapping(t *testing.T) {
	// Arrange
	rows := []PortedPrompt{{Workspace: "parent-ws", Turn: NewTurnID(), Text: "ask", Origin: "webapp", StartedAt: instant}}

	// Act
	_, err := RemintPortedPrompts("child-ws", rows, nil)

	// Assert
	if err == nil {
		t.Fatalf("RemintPortedPrompts() = nil, want a refusal when no mapping was supplied")
	}
}
