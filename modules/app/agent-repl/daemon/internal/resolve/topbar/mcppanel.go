package topbar

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

// putMcpServer retains ONE server's health, keyed by the server's name.
//
// The shim states a whole health per server, never a delta, so a later update
// REPLACES its predecessor in place rather than appending a second row for the
// same server. Row order is first-named order and stays put across health
// churn: a panel that reordered itself whenever a server flapped would move the
// row the reader is looking at.
//
// A health whose arm is UNSET says the server no longer stands — the session
// can state no health for a server it no longer has — and the row is dropped
// rather than drawn with a badge the producer never chose.
func (s *wsState) putMcpServer(server *conversationv1.SessionMcpServer) {
	if server == nil {
		return
	}
	name := server.GetName()
	for i, existing := range s.mcpServers {
		if existing.GetName() != name {
			continue
		}
		if server.GetHealth() == nil {
			s.mcpServers = append(s.mcpServers[:i], s.mcpServers[i+1:]...)
			return
		}
		s.mcpServers[i] = server
		return
	}
	if server.GetHealth() == nil {
		return
	}
	s.mcpServers = append(s.mcpServers, server)
}

// mcpPanel assembles the /mcp panel from the retained healths. An EMPTY
// catalog is a legitimate answer — a session with no MCP server configured has
// no rows to draw — so the panel is empty rather than absent.
func mcpPanel(servers []*conversationv1.SessionMcpServer) *frontendv1.McpPanelView {
	out := &frontendv1.McpPanelView{}
	for _, server := range servers {
		out.Rows = append(out.Rows, mcpRow(server))
	}
	return out
}

// mcpRow draws one server's line. The arm IS the badge, so each session arm
// maps to exactly the panel arm that states the same health, and no other.
func mcpRow(server *conversationv1.SessionMcpServer) *frontendv1.McpPanelRow {
	row := &frontendv1.McpPanelRow{Name: server.GetName()}
	switch health := server.GetHealth().(type) {
	case *conversationv1.SessionMcpServer_Connected:
		row.Status = &frontendv1.McpPanelRow_Connected{Connected: &frontendv1.McpPanelConnected{}}
	case *conversationv1.SessionMcpServer_Failed:
		failed := &frontendv1.McpPanelFailed{}
		if text := health.Failed.GetError(); text != "" {
			failed.Detail = &frontendv1.McpPanelFailedDetail{Text: truncate(text, DefaultLineWidth)}
		}
		row.Status = &frontendv1.McpPanelRow_Failed{Failed: failed}
	case *conversationv1.SessionMcpServer_NeedsAuth:
		row.Status = &frontendv1.McpPanelRow_NeedsAuth{NeedsAuth: &frontendv1.McpPanelNeedsAuth{}}
	case *conversationv1.SessionMcpServer_Pending:
		row.Status = &frontendv1.McpPanelRow_Pending{Pending: &frontendv1.McpPanelPending{}}
	case *conversationv1.SessionMcpServer_Disabled:
		row.Status = &frontendv1.McpPanelRow_Disabled{Disabled: &frontendv1.McpPanelDisabled{}}
	}
	return row
}
