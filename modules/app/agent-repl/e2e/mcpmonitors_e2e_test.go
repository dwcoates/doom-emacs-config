// mcpmonitors_e2e_test.go — MCP server healths, the unmodeled-MCP-tool
// warning, and the two Monitor lifetime arms (SPEC.md section C, "Everything
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
//   - golden "mcp-unmodeled-tool" (#82)    -> fake scenario "!unmodeled"
//     (automation.ts UNMODELED_MCP: an mcp__echo__echo call no converter
//     owns).
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
	"fmt"
	"strings"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/encoding/prototext"

	"claude-repld/integration/harness"
)

// ===========================================================================
// #81 McpServerHealths — mcp-server-healths (driven as "!mcp-all").
//
// Contract: proto/src/frontend/v1/mcp_panel.proto's McpPanelView ("THE /mcp
// PANEL: each MCP server and where it stands, from the vendor's structured
// status answer") and proto/src/agentrepl/v1/endpoint_submit_prompt.proto's
// SubmitPromptCommandPanel.mcp ("The /mcp panel: each MCP server and where
// it stands") — /mcp is a daemon-recognized, programmatically-handled
// command (confirmed against the daemon's own prompthandler test naming:
// SubmitPrompt("/mcp") resolves synchronously, it does not mint a turn).
// docs/overhaul/shim.md: "mcp servers -> query.mcpServerStatus()" — a
// CONTROL ANSWER, not a stream message, discovered "at the shim's own
// cadence". The fake's five-server catalog
// (agent-shim/claude/shim/src/fake/catalogs.ts FAKE_MCP_SERVERS) declares
// one server per McpPanelRow.status arm: echo=connected,
// broken=failed("spawn ENOENT"), needs-login=needs_auth, slow=pending,
// switched-off=disabled.
// ===========================================================================

func TestMcpServerHealths(t *testing.T) {
	t.Parallel()
	// Arrange
	w := NewWorld(t, WorldOpts{})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)

	// Act: drive "!mcp-all", which switches mcpServerStatus() to the
	// five-server catalog, then resolve the /mcp panel. The panel has no
	// watch stream of its own (it answers a command, like /status and
	// /agents), so this polls the real, synchronous SubmitPrompt round trip
	// on the same bounded-poll discipline world_test.go's
	// awaitCursorAdvance uses for the store's cursor read — never a sleep.
	driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "mcp-all")
	panel := awaitMcpPanel(t, w, ws, func(v *frontendv1.McpPanelView) bool {
		return v != nil && len(v.GetRows()) == 5
	})

	// Assert
	wantHealth := map[string]string{
		"echo":         "connected",
		"broken":       "failed",
		"needs-login":  "needs_auth",
		"slow":         "pending",
		"switched-off": "disabled",
	}
	seen := map[string]bool{}
	for _, row := range panel.GetRows() {
		name := row.GetName()
		seen[name] = true
		want, ok := wantHealth[name]
		if !ok {
			t.Errorf("McpPanelView row for unexpected server %q: %v", name, row)
			continue
		}
		if got := mcpRowHealth(row); got != want {
			t.Errorf("McpPanelView row %q health = %q, want %q (catalogs.ts FAKE_MCP_SERVERS): %v", name, got, want, row)
		}
	}
	for name := range wantHealth {
		if !seen[name] {
			t.Errorf("McpPanelView is missing a row for server %q, want one per declared health", name)
		}
	}
	// The failed server's own error is carried verbatim ("spawn ENOENT").
	for _, row := range panel.GetRows() {
		if row.GetName() != "broken" {
			continue
		}
		if got := row.GetFailed().GetDetail().GetText(); got != "spawn ENOENT" {
			t.Errorf("McpPanelRow(broken).failed.detail = %q, want %q", got, "spawn ENOENT")
		}
	}
}

// mcpRowHealth answers the drawn arm name of one McpPanelRow's status oneof.
func mcpRowHealth(row *frontendv1.McpPanelRow) string {
	switch {
	case row.GetConnected() != nil:
		return "connected"
	case row.GetFailed() != nil:
		return "failed"
	case row.GetNeedsAuth() != nil:
		return "needs_auth"
	case row.GetPending() != nil:
		return "pending"
	case row.GetDisabled() != nil:
		return "disabled"
	default:
		return "unset"
	}
}

// awaitMcpPanel polls /mcp until pred is satisfied or DefaultTimeout elapses.
// /mcp is a real command recognized by the daemon's own prompt handler
// (confirmed unreachable-when-absent: daemon/internal/prompthandler's own
// test asserts an absent panel is surfaced as an ERROR, never an empty
// success), so a transient error while the scenario's control answer has
// not yet been discovered is expected and retried, not fatal.
func awaitMcpPanel(t *testing.T, w *World, ws *workspacev1.WorkspaceRef, pred func(*frontendv1.McpPanelView) bool) *frontendv1.McpPanelView {
	t.Helper()
	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()
	ticker := time.NewTicker(pollInterval)
	defer ticker.Stop()
	var lastErr error
	var lastPanel *frontendv1.McpPanelView
	for {
		resp, err := w.Client().SubmitPrompt(w.Ctx(), connect.NewRequest(&agentreplv1.SubmitPromptRequest{
			Workspace:      ws,
			Said:           slashCommandSaid("/mcp"),
			IdempotencyKey: newIdempotencyKey(t),
			Origin:         e2ePromptOrigin,
		}))
		switch {
		case err != nil:
			lastErr = err
		case resp.Msg.GetError() != nil:
			lastErr = fmt.Errorf("refused: %v", resp.Msg.GetError())
		default:
			lastPanel = resp.Msg.GetSuccess().GetCommandPanel().GetMcp()
			lastErr = nil
			if pred(lastPanel) {
				return lastPanel
			}
		}
		select {
		case <-ticker.C:
		case <-ctx.Done():
			t.Fatalf("e2e: waiting for /mcp to reflect the scenario's catalog: last error %v, last panel %v", lastErr, lastPanel)
			return nil
		}
	}
}

// slashCommandSaid wraps a slash command as the UserSaid a real SubmitPrompt
// carries — the same one-text-block shape SubmitPrompt (world_test.go) uses
// for an ordinary prompt.
func slashCommandSaid(text string) *conversationv1.UserSaid {
	return &conversationv1.UserSaid{Content: &conversationv1.UserContent{Blocks: []*conversationv1.UserContentBlock{
		{Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: text}}},
	}}}
}

// ===========================================================================
// #82 McpUnmodeledTool — mcp-unmodeled-tool (driven as "!unmodeled").
//
// Contract, cited verbatim by the dispatching agent: docs/overhaul/daemon.md
// "Failure classification": "Unmodeled tools are NOT failures and never feed
// rows — their home is the topbar's warning dropdown, one warning per
// distinct name." This is also STRUCTURAL, not merely a convention:
// proto/src/frontend/v1/feed.proto's FeedTurnActivity.unit oneof (response,
// simple_tool_call, skill, merge, subagent, hook, artifact, plan, findings)
// has no "unmodeled" arm at all — there is no representation an unmodeled
// tool call COULD occupy on the feed. The tool name and its opaque payload
// are asserted absent from the turn's feed rows as an extra check on top of
// that structural fact (the row content should never leak the raw call even
// incidentally, e.g. folded into a response's own text).
//
// The scenario is driven TWICE to pin "one warning per distinct name" (not
// one per occurrence): agent-shim/claude/shim/src/fake/scenarios/automation.ts
// UNMODELED_MCP always calls the same tool_name, "mcp__echo__echo".
// ===========================================================================

func TestMcpUnmodeledTool(t *testing.T) {
	t.Parallel()
	// Arrange
	w := NewWorld(t, WorldOpts{})
	// THE UNMODELED ACTIVITY IS WHAT THIS TEST DRIVES. `AgentActivity.unmodeled`
	// is a modeled arm, the watcher routes it to the topbar's warning dropdown
	// (asserted below) and records it on the way past; the scenario runs twice
	// and both planes carry each frame, so the record is repeated while the
	// dropdown still holds exactly one warning per distinct name.
	w.ExpectWarnings("daemon.sessionwatcher.unmodeled_activity")
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)
	topbar := w.WatchTopbar(ws)
	defer topbar.Close()

	// Act
	firstTurn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "unmodeled")
	secondTurn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "unmodeled")

	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()
	const toolName = "mcp__echo__echo"
	view := harness.AwaitView(t, ctx, topbar, "the mcp__echo__echo unmodeled-tool warning", func(v *frontendv1.TopbarView) bool {
		return countUnmodeledWarnings(v, toolName) > 0
	})

	// Assert: one warning per distinct name, even though the tool ran twice.
	if got := countUnmodeledWarnings(view, toolName); got != 1 {
		t.Fatalf("TopbarWarningStrip carries %d warnings for %q, want exactly 1 (one per distinct name): %v",
			got, toolName, view.GetWarnings())
	}

	// Assert: neither turn's feed content mentions the tool or its opaque
	// payload — its only home is the warning dropdown just asserted above.
	assertFeedRowsMentionNoneOf(t, w, ws, firstTurn, toolName, "hello from the offline session")
	assertFeedRowsMentionNoneOf(t, w, ws, secondTurn, toolName, "hello from the offline session")
}

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

// assertFeedRowsMentionNoneOf reads the workspace's root feed page and fails
// the test if any row belonging to turn contains any of the forbidden
// substrings, anywhere in the row's own fields.
func assertFeedRowsMentionNoneOf(t *testing.T, w *World, ws *workspacev1.WorkspaceRef, turn *conversationv1.TurnId, forbidden ...string) {
	t.Helper()
	opened, err := w.Client().OpenFeed(w.Ctx(), connect.NewRequest(&agentreplv1.OpenFeedRequest{Workspace: ws}))
	if err != nil {
		t.Fatalf("OpenFeed: %v", err)
	}
	rows := opened.Msg.GetSuccess().GetPage().GetSuccess().GetRows()
	found := false
	for _, row := range rows {
		if row.GetTurn().GetValue() != turn.GetValue() {
			continue
		}
		found = true
		text := prototext.Format(row)
		for _, needle := range forbidden {
			if strings.Contains(text, needle) {
				t.Errorf("turn %s feed row mentions %q, which must be confined to the topbar warning dropdown: %s",
					turn.GetValue(), needle, text)
			}
		}
	}
	if !found {
		t.Errorf("no feed rows found for turn %s", turn.GetValue())
	}
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

func TestMonitorDeadline(t *testing.T) {
	t.Parallel()
	// Arrange
	w := NewWorld(t, WorldOpts{})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)
	footer := w.WatchFooter(ws)
	defer footer.Close()

	// Act
	driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "monitor-deadline")
	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()
	view := harness.AwaitView(t, ctx, footer.Stream, "the deadline monitor's footer row", func(v *frontendv1.FooterView) bool {
		return len(v.GetExpanded().GetMonitors().GetRows()) > 0
	})

	// Assert
	assertOneMonitorRow(t, view, "build log", false /* persistent */)
}

func TestMonitorPersistent(t *testing.T) {
	t.Parallel()
	// Arrange
	w := NewWorld(t, WorldOpts{})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)
	footer := w.WatchFooter(ws)
	defer footer.Close()

	// Act
	driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "monitor-persistent")
	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()
	view := harness.AwaitView(t, ctx, footer.Stream, "the persistent monitor's footer row", func(v *frontendv1.FooterView) bool {
		return len(v.GetExpanded().GetMonitors().GetRows()) > 0
	})

	// Assert
	assertOneMonitorRow(t, view, "echo watch", true /* persistent */)
}

// A MONITOR ROW JUMPS TO ITS CALL'S CARD: the Monitor call draws the ordinary
// tool-call card on the root feed, and the footer's row names exactly that
// FeedId, so a click centers the card.
func TestMonitorRowJumpsToTheMonitorsToolCallCard(t *testing.T) {
	t.Parallel()
	// Arrange
	w := NewWorld(t, WorldOpts{})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)
	footer := w.WatchFooter(ws)
	defer footer.Close()

	// Act
	driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "monitor-persistent")

	// Assert
	card := awaitFeedRow(t, w, ws, "the monitor's tool-call card", func(r *frontendv1.FeedRow) bool {
		return r.GetActivity().GetSimpleToolCall().GetName().GetText() == "Monitor"
	})
	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()
	harness.AwaitView(t, ctx, footer.Stream, "the monitor row naming the card", func(v *frontendv1.FooterView) bool {
		rows := v.GetExpanded().GetMonitors().GetRows()
		return len(rows) == 1 && rows[0].GetJump().GetEntry().GetValue() == card.GetId().GetValue()
	})
}

// assertOneMonitorRow asserts the footer carries exactly one live monitor,
// with the given drawn description and persistent-marker presence.
func assertOneMonitorRow(t *testing.T, view *frontendv1.FooterView, wantDescription string, wantPersistent bool) {
	t.Helper()
	if got := view.GetStrip().GetLiveWork().GetMonitors().GetCount(); got != 1 {
		t.Errorf("FooterChipMonitors.count = %d, want 1: %v", got, view.GetStrip().GetLiveWork())
	}
	rows := view.GetExpanded().GetMonitors().GetRows()
	if len(rows) != 1 {
		t.Fatalf("FooterExpandedMonitors.rows has %d rows, want 1: %v", len(rows), rows)
	}
	row := rows[0]
	if got := row.GetDescription().GetText(); got != wantDescription {
		t.Errorf("FooterMonitorRow.description = %q, want %q", got, wantDescription)
	}
	if gotPersistent := row.GetPersistent() != nil; gotPersistent != wantPersistent {
		t.Errorf("FooterMonitorRow.persistent set = %v, want %v", gotPersistent, wantPersistent)
	}
	if row.GetRuntime().GetStartedAtMs() == 0 {
		t.Errorf("FooterMonitorRow.runtime.started_at_ms is 0, want the watch's real arm instant")
	}
}
