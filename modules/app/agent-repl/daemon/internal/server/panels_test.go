package server

import (
	"context"
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/topbar"
)

// panelTopbar answers a context panel and the status facts on demand.
type panelTopbar struct {
	topbar.Resolver
	panel     *frontendv1.ContextPanelView
	facts     topbar.StatusFacts
	factsHeld bool
	mcp       *frontendv1.McpPanelView
}

func (p *panelTopbar) McpPanel(ids.WorkspaceID) *frontendv1.McpPanelView {
	if p.mcp == nil {
		return &frontendv1.McpPanelView{}
	}
	return p.mcp
}

func (p *panelTopbar) ContextPanel(ids.WorkspaceID) (*frontendv1.ContextPanelView, bool) {
	return p.panel, p.panel != nil
}

func (p *panelTopbar) StatusFacts(ids.WorkspaceID) (topbar.StatusFacts, bool) {
	return p.facts, p.factsHeld
}

// stamp is a build stamp that reads.
func stamp(sha string) VersionFunc { return func() (string, error) { return sha, nil } }

// TestPanelsAnswersTheContextPanel pins the context-tree producer.
func TestPanelsAnswersTheContextPanel(t *testing.T) {
	// Arrange.
	source := Panels(&panelTopbar{panel: &frontendv1.ContextPanelView{}}, stamp("abc123"), fakeLogger{})

	// Act.
	panel, err := source(context.Background(), testWorkspaceID,
		conversationv1.SessionCommand_SESSION_COMMAND_CONTEXT)

	// Assert.
	if err != nil || panel.GetContext() == nil {
		t.Fatalf("panel = %v, err = %v; want the context panel", panel, err)
	}
}

// TestPanelsAnswersTheStatusPanel pins the settled thin panel: the version row
// plus the three spliced session facts, and nothing else.
func TestPanelsAnswersTheStatusPanel(t *testing.T) {
	// Arrange.
	source := Panels(&panelTopbar{
		factsHeld: true,
		facts: topbar.StatusFacts{
			Account: "someone@example.com", Model: "opus", PermissionMode: "default",
		},
	}, stamp("abc123"), fakeLogger{})

	// Act.
	panel, err := source(context.Background(), testWorkspaceID,
		conversationv1.SessionCommand_SESSION_COMMAND_STATUS)

	// Assert.
	if err != nil {
		t.Fatalf("status panel: %v", err)
	}
	want := [][2]string{
		{"Version", "abc123"},
		{"Account", "someone@example.com"},
		{"Model", "opus"},
		{"Permission mode", "default"},
	}
	rows := panel.GetStatus().GetRows()
	if len(rows) != len(want) {
		t.Fatalf("rows = %v, want %v", rows, want)
	}
	for i, row := range rows {
		if row.GetLabel() != want[i][0] || row.GetValue() != want[i][1] {
			t.Fatalf("row %d = %q/%q, want %q/%q", i, row.GetLabel(), row.GetValue(), want[i][0], want[i][1])
		}
	}
}

// TestStatusPanelOmitsARowTheSessionHasNotStated pins that a blank value is
// omitted rather than drawn: StatusPanelRow.value is never empty.
func TestStatusPanelOmitsARowTheSessionHasNotStated(t *testing.T) {
	// Arrange: a logged-out config root states no account.
	source := Panels(&panelTopbar{
		factsHeld: true,
		facts:     topbar.StatusFacts{Model: "opus", PermissionMode: "plan"},
	}, stamp("abc123"), fakeLogger{})

	// Act.
	panel, err := source(context.Background(), testWorkspaceID,
		conversationv1.SessionCommand_SESSION_COMMAND_STATUS)

	// Assert.
	if err != nil {
		t.Fatalf("status panel: %v", err)
	}
	for _, row := range panel.GetStatus().GetRows() {
		if row.GetLabel() == "Account" {
			t.Fatalf("rows = %v, want no Account row", panel.GetStatus().GetRows())
		}
	}
}

// TestStatusPanelRefusesWhenNoSessionFactsStand pins that a workspace whose
// session never opened fails loudly rather than drawing a version-only card.
func TestStatusPanelRefusesWhenNoSessionFactsStand(t *testing.T) {
	// Arrange.
	source := Panels(&panelTopbar{}, stamp("abc123"), fakeLogger{})

	// Act.
	_, err := source(context.Background(), testWorkspaceID,
		conversationv1.SessionCommand_SESSION_COMMAND_STATUS)

	// Assert.
	if err == nil {
		t.Fatal("a status panel was answered for a workspace with no session facts")
	}
}

// TestStatusPanelRefusesAnUnreadableBuildStamp pins that an unknown version is
// a failure, never a drawn placeholder.
func TestStatusPanelRefusesAnUnreadableBuildStamp(t *testing.T) {
	// Arrange.
	source := Panels(&panelTopbar{factsHeld: true}, func() (string, error) {
		return "", errors.New("the deploy stamp is empty")
	}, fakeLogger{})

	// Act.
	_, err := source(context.Background(), testWorkspaceID,
		conversationv1.SessionCommand_SESSION_COMMAND_STATUS)

	// Assert.
	if err == nil {
		t.Fatal("a status panel was answered without a version the daemon knows")
	}
}

// TestPanelsRefusesAPanelWithNoProducer pins that a command whose panel nothing
// assembles fails LOUDLY rather than drawing an empty card.
func TestPanelsRefusesAPanelWithNoProducer(t *testing.T) {
	// Arrange.
	source := Panels(&panelTopbar{panel: &frontendv1.ContextPanelView{}}, stamp("abc123"), fakeLogger{})

	// Act.
	_, err := source(context.Background(), testWorkspaceID,
		conversationv1.SessionCommand_SESSION_COMMAND_TODOS)

	// Assert.
	if err == nil {
		t.Fatal("a panel with no producer was answered; it must fail loudly")
	}
}

// TestPanelsRefusesWhenNoContextPanelStands pins that an absent panel is a
// failure rather than an empty one.
func TestPanelsRefusesWhenNoContextPanelStands(t *testing.T) {
	// Arrange.
	source := Panels(&panelTopbar{}, stamp("abc123"), fakeLogger{})

	// Act.
	_, err := source(context.Background(), testWorkspaceID,
		conversationv1.SessionCommand_SESSION_COMMAND_CONTEXT)

	// Assert.
	if err == nil {
		t.Fatal("an absent context panel was answered as a panel")
	}
}

// TestStatusPanelOmitsTheVersionRowWhenNoStampWasWritten pins that the Version
// row obeys the same omission rule as the rest: a checkout the deploy chain
// never stamped has no version to state, and an empty-valued row would state
// one anyway.
func TestStatusPanelOmitsTheVersionRowWhenNoStampWasWritten(t *testing.T) {
	// Arrange.
	source := Panels(&panelTopbar{
		factsHeld: true,
		facts:     topbar.StatusFacts{Model: "opus", PermissionMode: "plan"},
	}, stamp(""), fakeLogger{})

	// Act.
	panel, err := source(context.Background(), testWorkspaceID,
		conversationv1.SessionCommand_SESSION_COMMAND_STATUS)

	// Assert.
	if err != nil {
		t.Fatalf("status panel: %v", err)
	}
	for _, row := range panel.GetStatus().GetRows() {
		if row.GetLabel() == "Version" {
			t.Fatalf("rows = %v, want no Version row when the deploy chain wrote no stamp", panel.GetStatus().GetRows())
		}
	}
}

// TestPanelsAnswersTheMcpPanel pins the /mcp producer: the retained server
// healths are drawn as the panel's rows.
func TestPanelsAnswersTheMcpPanel(t *testing.T) {
	// Arrange.
	source := Panels(&panelTopbar{mcp: &frontendv1.McpPanelView{
		Rows: []*frontendv1.McpPanelRow{{Name: "github"}},
	}}, stamp("abc123"), fakeLogger{})

	// Act.
	panel, err := source(context.Background(), testWorkspaceID,
		conversationv1.SessionCommand_SESSION_COMMAND_MCP)

	// Assert.
	if err != nil {
		t.Fatalf("mcp panel: %v", err)
	}
	rows := panel.GetMcp().GetRows()
	if len(rows) != 1 || rows[0].GetName() != "github" {
		t.Fatalf("rows = %v, want one github row", rows)
	}
}

// TestMcpPanelAnswersAnEmptyCatalogAsAnEmptyPanel pins that a workspace with no
// MCP server stated is an empty panel, never a refusal.
func TestMcpPanelAnswersAnEmptyCatalogAsAnEmptyPanel(t *testing.T) {
	// Arrange.
	source := Panels(&panelTopbar{}, stamp("abc123"), fakeLogger{})

	// Act.
	panel, err := source(context.Background(), testWorkspaceID,
		conversationv1.SessionCommand_SESSION_COMMAND_MCP)

	// Assert.
	if err != nil {
		t.Fatalf("mcp panel: %v", err)
	}
	if panel.GetMcp() == nil || len(panel.GetMcp().GetRows()) != 0 {
		t.Fatalf("panel = %v, want an empty mcp panel", panel)
	}
}
