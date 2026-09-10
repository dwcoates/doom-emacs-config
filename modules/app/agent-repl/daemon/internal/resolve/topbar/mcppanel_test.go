package topbar

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

// mcpServer is one mcp_server session update, at the given health.
func mcpServer(name string, health any) *conversationv1.SessionUpdate {
	server := &conversationv1.SessionMcpServer{Name: name}
	switch h := health.(type) {
	case nil:
	case *conversationv1.SessionMcpServerConnected:
		server.Health = &conversationv1.SessionMcpServer_Connected{Connected: h}
	case *conversationv1.SessionMcpServerFailed:
		server.Health = &conversationv1.SessionMcpServer_Failed{Failed: h}
	case *conversationv1.SessionMcpServerNeedsAuth:
		server.Health = &conversationv1.SessionMcpServer_NeedsAuth{NeedsAuth: h}
	case *conversationv1.SessionMcpServerPending:
		server.Health = &conversationv1.SessionMcpServer_Pending{Pending: h}
	case *conversationv1.SessionMcpServerDisabled:
		server.Health = &conversationv1.SessionMcpServer_Disabled{Disabled: h}
	}
	return &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_McpServer{McpServer: server},
	}
}

// TestTheMcpPanelRetainsAServerHealth pins that a stated health becomes a row.
func TestTheMcpPanelRetainsAServerHealth(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ready(t)

	// Act.
	h.r.OnSessionUpdate(testWS, mcpServer("github", &conversationv1.SessionMcpServerConnected{}))

	// Assert.
	rows := h.r.McpPanel(testWS).GetRows()
	if len(rows) != 1 || rows[0].GetName() != "github" || rows[0].GetConnected() == nil {
		t.Fatalf("rows = %v, want one connected github row", rows)
	}
}

// TestALaterHealthReplacesTheSameServersRow pins that a server's second update
// updates its row rather than appending a second one.
func TestALaterHealthReplacesTheSameServersRow(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ready(t)
	h.r.OnSessionUpdate(testWS, mcpServer("github", &conversationv1.SessionMcpServerPending{}))

	// Act.
	h.r.OnSessionUpdate(testWS, mcpServer("github",
		&conversationv1.SessionMcpServerFailed{Error: "connection refused"}))

	// Assert.
	rows := h.r.McpPanel(testWS).GetRows()
	if len(rows) != 1 {
		t.Fatalf("rows = %v, want one row", rows)
	}
	if got := rows[0].GetFailed().GetDetail().GetText(); got != "connection refused" {
		t.Fatalf("detail = %q, want %q", got, "connection refused")
	}
}

// TestAHealthlessUpdateRemovesTheServersRow pins that a server the session
// states no health for is dropped rather than drawn with an invented badge.
func TestAHealthlessUpdateRemovesTheServersRow(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ready(t)
	h.r.OnSessionUpdate(testWS, mcpServer("github", &conversationv1.SessionMcpServerConnected{}))

	// Act.
	h.r.OnSessionUpdate(testWS, mcpServer("github", nil))

	// Assert.
	if rows := h.r.McpPanel(testWS).GetRows(); len(rows) != 0 {
		t.Fatalf("rows = %v, want the row dropped", rows)
	}
}

// TestTheMcpPanelKeepsFirstNamedOrderAcrossServers pins that N servers draw N
// rows, in the order they were first named.
func TestTheMcpPanelKeepsFirstNamedOrderAcrossServers(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ready(t)
	h.r.OnSessionUpdate(testWS, mcpServer("github", &conversationv1.SessionMcpServerConnected{}))
	h.r.OnSessionUpdate(testWS, mcpServer("linear", &conversationv1.SessionMcpServerNeedsAuth{}))
	h.r.OnSessionUpdate(testWS, mcpServer("sentry", &conversationv1.SessionMcpServerDisabled{}))

	// Act: an update for the FIRST server must not move it behind the others.
	h.r.OnSessionUpdate(testWS, mcpServer("github", &conversationv1.SessionMcpServerPending{}))

	// Assert.
	rows := h.r.McpPanel(testWS).GetRows()
	want := []string{"github", "linear", "sentry"}
	if len(rows) != len(want) {
		t.Fatalf("rows = %v, want %v", rows, want)
	}
	for i, name := range want {
		if rows[i].GetName() != name {
			t.Fatalf("row %d = %q, want %q", i, rows[i].GetName(), name)
		}
	}
}

// TestAnEmptyCatalogDrawsAnEmptyMcpPanel pins that a workspace no server has
// been stated for answers an empty panel, not a missing one.
func TestAnEmptyCatalogDrawsAnEmptyMcpPanel(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ready(t)

	// Act.
	panel := h.r.McpPanel(testWS)

	// Assert.
	if panel == nil || len(panel.GetRows()) != 0 {
		t.Fatalf("panel = %v, want an empty panel", panel)
	}
}

// TestAFailedHealthWithNoErrorStatesNoDetail pins that a vendor that gave no
// failure text leaves the detail UNSET rather than drawing an empty one.
func TestAFailedHealthWithNoErrorStatesNoDetail(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ready(t)

	// Act.
	h.r.OnSessionUpdate(testWS, mcpServer("github", &conversationv1.SessionMcpServerFailed{}))

	// Assert.
	rows := h.r.McpPanel(testWS).GetRows()
	if len(rows) != 1 || rows[0].GetFailed() == nil {
		t.Fatalf("rows = %v, want one failed row", rows)
	}
	if rows[0].GetFailed().Detail != nil {
		t.Fatalf("detail = %v, want unset", rows[0].GetFailed().GetDetail())
	}
}
