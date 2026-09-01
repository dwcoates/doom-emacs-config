package server

import (
	"context"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/topbar"
)

// panelTopbar answers a context panel on demand.
type panelTopbar struct {
	topbar.Resolver
	panel *frontendv1.ContextPanelView
}

func (p *panelTopbar) ContextPanel(ids.WorkspaceID) (*frontendv1.ContextPanelView, bool) {
	return p.panel, p.panel != nil
}

// TestPanelsAnswersTheContextPanel pins the one panel that has a producer.
func TestPanelsAnswersTheContextPanel(t *testing.T) {
	// Arrange.
	source := Panels(&panelTopbar{panel: &frontendv1.ContextPanelView{}}, fakeLogger{})

	// Act.
	panel, err := source(context.Background(), testWorkspaceID,
		conversationv1.SessionCommand_SESSION_COMMAND_CONTEXT)

	// Assert.
	if err != nil || panel.GetContext() == nil {
		t.Fatalf("panel = %v, err = %v; want the context panel", panel, err)
	}
}

// TestPanelsRefusesAPanelWithNoProducer pins that a command whose panel nothing
// assembles fails LOUDLY rather than drawing an empty card.
func TestPanelsRefusesAPanelWithNoProducer(t *testing.T) {
	// Arrange.
	source := Panels(&panelTopbar{panel: &frontendv1.ContextPanelView{}}, fakeLogger{})

	// Act.
	_, err := source(context.Background(), testWorkspaceID,
		conversationv1.SessionCommand_SESSION_COMMAND_STATUS)

	// Assert.
	if err == nil {
		t.Fatal("a panel with no producer was answered; it must fail loudly")
	}
}

// TestPanelsRefusesWhenNoContextPanelStands pins that an absent panel is a
// failure rather than an empty one.
func TestPanelsRefusesWhenNoContextPanelStands(t *testing.T) {
	// Arrange.
	source := Panels(&panelTopbar{}, fakeLogger{})

	// Act.
	_, err := source(context.Background(), testWorkspaceID,
		conversationv1.SessionCommand_SESSION_COMMAND_CONTEXT)

	// Assert.
	if err == nil {
		t.Fatal("an absent context panel was answered as a panel")
	}
}
