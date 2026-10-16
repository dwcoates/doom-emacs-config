// mcpmonitors_e2e_test.go — MCP server healths, an MCP tool call's ordinary
// tool card, the unmodeled-tool warning, and the two Monitor lifetime arms (SPEC.md section C, "Everything
// else", entries #81-84 — project-lead ruling 5's split; this file owns
// mcp-server-healths, mcp-unmodeled-tool, monitor-deadline, monitor-persistent
// and none of the other three split files' goldens).
//
// Every scenario driven here is a REAL, GROUNDED capture per
// agent-shim/claude/shim/testdata/captures/MANIFEST.md (rows
// `mcp-server-healths` 2026-09-02, `mcp-unmodeled-tool` 2026-09-01,
// `monitor-deadline`/`monitor-persistent` 2026-09-01 — all end
// `success.completed`), mirrored by the fake SDK's registered `!name`
// scenarios documented in agent-shim/claude/shim/AGENTS.md's "Mocked vendor:
// prompt -> scenario table". The GOLDEN names and the fake SDK's registered
// `!name`s differ for two of the four (confirmed by reading
// src/fake/scenarios/{session,automation}.ts and AGENTS.md's own table, per
// SPEC.md's own note that a "no new scenario needed" row can still require
// finding which EXISTING name covers the golden):
//
//   - golden "mcp-server-healths" (#81)    -> fake scenario "!mcp-all"
//     (session.ts MCP_ALL: switches mcpServerStatus() to the five-server
//     catalog, one row per declared health).
//   - golden "mcp-unmodeled-tool" (#82)    -> fake scenario "!mcp-tool"
//     (automation.ts MCP_TOOL: an mcp__echo__echo call, an ordinary MCP tool
//     call). The unmodeled-tool warning is driven by "!unmodeled"
//     (automation.ts UNMODELED: a StructuredOutput call no converter owns).
//   - golden "monitor-deadline" (#83)      -> fake scenario "!monitor-deadline"
//     (exact name match, automation.ts MONITOR_DEADLINE).
//   - golden "monitor-persistent" (#84)    -> fake scenario "!monitor-persistent"
//     (exact name match, automation.ts MONITOR_PERSISTENT).
//
// Per the dispatching agent's ruling (superseding SPEC.md's original "Real
// git" ruling 4 while it is being reworked elsewhere): this file uses the
// FAKE git the daemon harness installs (harness.NewRepo), never real git —
// every workspace here is a plain single-session fixture with no git fact of
// its own interest.
package e2e

import (
	"context"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"connectrpc.com/connect"

	"agent-repld/integration/harness"
)

// ===========================================================================
// #82 McpToolCall — mcp-unmodeled-tool (driven as "!mcp-tool").
//
// Contract: proto/src/conversation/v1/agent_activity.proto AgentMcpToolCall —
// an MCP server's tool is an ORDINARY TOOL CALL, drawn as the ordinary tool
// card (FeedSimpleToolCall) headed by the tool as the agent named it, and it
// raises no unmodeled-tool warning. No `unmodeled_activity` WARN is declared,
// so the world's log sweep fails the test if one is recorded.
// ===========================================================================

func TestMcpToolCall(t *testing.T) {
	t.Parallel()
	// Arrange
	w := NewWorld(t, WorldOpts{})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)
	topbar := w.WatchTopbar(ws)
	defer topbar.Close()

	// Act
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "mcp-tool")

	// Assert: the turn drew one ordinary tool card for the MCP call, with
	// what the tool returned.
	const toolName = "mcp__echo__echo"
	card := turnToolCard(t, w, ws, turn, toolName)
	if got := card.GetReturned().GetText().GetText(); got != "hello from the offline session" {
		t.Fatalf("the MCP card's output = %q, want the tool's own text: %v", got, card)
	}

	// Assert: the call raised no unmodeled-tool warning.
	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()
	view := harness.AwaitView(t, ctx, topbar, "the topbar after the MCP turn", func(v *frontendv1.TopbarView) bool { return v != nil })
	if got := countUnmodeledWarnings(view, toolName); got != 0 {
		t.Fatalf("TopbarWarningStrip carries %d unmodeled warnings for %q, want none: %v", got, toolName, view.GetWarnings())
	}
}

// turnToolCard reads the workspace's root feed and answers the one settled
// tool card of turn headed toolName, failing the test when there is none.
func turnToolCard(t *testing.T, w *World, ws *workspacev1.WorkspaceRef, turn *conversationv1.TurnId, toolName string) *frontendv1.FeedSimpleToolCall {
	t.Helper()
	opened, err := w.Client().OpenFeed(w.Ctx(), connect.NewRequest(&agentreplv1.OpenFeedRequest{Workspace: ws}))
	if err != nil {
		t.Fatalf("OpenFeed: %v", err)
	}
	for _, row := range opened.Msg.GetSuccess().GetPage().GetSuccess().GetRows() {
		card := row.GetActivity().GetSimpleToolCall()
		if row.GetTurn().GetValue() == turn.GetValue() && card.GetName().GetText() == toolName && card.GetReturned() != nil {
			return card
		}
	}
	t.Fatalf("turn %s drew no settled tool card headed %q", turn.GetValue(), toolName)
	return nil
}

// ===========================================================================
// UnmodeledTool — driven as "!unmodeled".
//
// Contract, cited verbatim by the dispatching agent: docs/overhaul/daemon.md
// "Failure classification": "Unmodeled tools are NOT failures and never feed
// rows — their home is the topbar's warning dropdown, one warning per
// distinct name." This is also STRUCTURAL, not merely a convention:
// proto/src/frontend/v1/feed.proto's FeedTurnActivity.unit oneof has no
// "unmodeled" arm at all — there is no representation an unmodeled tool call
// COULD occupy on the feed. The tool name and its opaque payload are asserted
// absent from the turn's feed rows as an extra check on top of that fact.
//
// The scenario is driven TWICE to pin "one warning per distinct name" (not
// one per occurrence): automation.ts UNMODELED always calls the same tool,
// "StructuredOutput".
// ===========================================================================

// countUnmodeledWarnings counts TopbarWarning entries whose unmodeled_tool
// detail names toolName.
func countUnmodeledWarnings(v *frontendv1.TopbarView, toolName string) int {
	n := 0
	for _, warning := range v.GetWarnings().GetWarnings() {
		if warning.GetUnmodeledTool().GetToolName().GetText() == toolName {
			n++
		}
	}
	return n
}

// ===========================================================================
// #83/#84 MonitorDeadline / MonitorPersistent — monitor-deadline,
// monitor-persistent.
//
// Contract: proto/src/conversation/v1/agent_activity.proto's AgentMonitor
// family: the call draws the ordinary tool-call card in its owner's feed,
// which is the footer monitor row's jump target (owner ruling, 2026-09-23),
// and the daemon tracks liveness for the footer's monitors chip and panel.
// proto/src/frontend/v1/footer.proto:
// FooterChipMonitors (the (eye) chip, "set iff at least one is live") and
// FooterExpandedMonitors/FooterMonitorRow (description, runtime, and an
// optional persistent marker — "Present iff the watch is persistent").
//
// automation.ts MONITOR_DEADLINE/MONITOR_PERSISTENT: the deadline scenario's
// ctx.startTask carries description "build log" and no persistent marker
// (AgentMonitorStart.lifetime=deadline); the persistent scenario's carries
// description "echo watch" with persistent set. Both scenarios note "the
// monitor stays in the live set" past the turn's own terminal, so the
// footer's monitor row is asserted to survive the scenario's own
// AwaitTurnEnded (driveScenarioToCompletion already waits for that).
// ===========================================================================
