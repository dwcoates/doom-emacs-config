//go:build integration

package integration

import (
	"os"
	"path/filepath"
	"slices"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/integration/harness"
	"agent-repld/internal/scenariogolden"
)

// scenarioGoldensDir is the scenario goldens, from this package's directory.
const scenarioGoldensDir = "../../testdata/scenario-goldens"

// endToEndScenarios are the scenarios driven through a real daemon. Every
// scenario's projection is proved in process (internal/scenariogolden,
// daemon/AGENTS.md "Projection goldens run in process"); these prove the seam
// that projection cannot: frames served by a shim process over its socket,
// routed by the daemon's own watcher into its own feed and read back over its
// API. `subagent` opens a subagent's watch and carries session updates;
// `compact-failed` draws a golden whose warnings are read from the daemon's
// own log.
var endToEndScenarios = []string{"subagent", "compact-failed"}

// TestScenarioGoldensRouteThroughARealDaemon drives each end-to-end scenario
// through a real daemon and its fake shim process with the same drive loop as
// the in-process projection (scenariogolden.Drive), and compares the root feed
// it draws, and the warnings it records, against the scenario's feed.json.
func TestScenarioGoldensRouteThroughARealDaemon(t *testing.T) {
	t.Parallel()
	for _, name := range endToEndScenarios {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			path := filepath.Join(scenarioGoldensDir, name, "feed.json")
			want := scenariogolden.ReadFeedGolden(t, path)
			// THE SCENARIO'S WARNINGS ARE READ FROM EVERYTHING ITS DAEMON
			// RECORDED, so each scenario keeps a daemon of its own.
			f := newOpened(t, harness.Opts{})
			if len(want.Warnings) > 0 {
				f.d.RequireWarnings(want.Warnings...)
			}
			got := scenariogolden.Drive(t, &daemonDriver{f: f}, scenarioGoldensDir, name)
			if string(got) != string(want.Rows) {
				t.Fatalf("the daemon's feed for %s differs from its golden %s (rewrite with %s=1 in internal/scenariogolden if the change is intended):\ngot:\n%s\nwant:\n%s",
					name, path, scenariogolden.WriteEnv, got, want.Rows)
			}
		})
	}
}

func TestEveryEndToEndScenarioHasAGolden(t *testing.T) {
	// Arrange
	names := scenariogolden.Names(t, scenarioGoldensDir)

	// Act and Assert
	for _, name := range endToEndScenarios {
		if !slices.Contains(names, name) {
			t.Errorf("end-to-end scenario %q names no golden under %s", name, scenarioGoldensDir)
		}
		if _, err := os.Stat(filepath.Join(scenarioGoldensDir, name, "feed.json")); err != nil {
			t.Errorf("end-to-end scenario %q has no feed.json: %v", name, err)
		}
	}
}

// daemonDriver serves a scenario through a real daemon: its fake shim process
// pushes each entry, and the daemon's workspace log holds the routing records
// the drive awaits.
type daemonDriver struct{ f *fixture }

// OpenTurn implements scenariogolden.Driver: a SubmitPrompt, which the daemon
// opens on its shim.
func (d *daemonDriver) OpenTurn(t *testing.T, prompt, key string) string {
	t.Helper()
	turn := d.f.submit(prompt, key, conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT).GetSuccess().GetTurn().GetTurn().GetValue()
	d.f.shim.ExpectStartTurn()
	return turn
}

// MainAgent implements scenariogolden.Driver.
func (d *daemonDriver) MainAgent() string { return mainAgent }

// Workspace implements scenariogolden.Driver.
func (d *daemonDriver) Workspace() string { return d.f.ws.GetId() }

// Records implements scenariogolden.Driver: the workspace's daemon log.
func (d *daemonDriver) Records(t *testing.T) []scenariogolden.Record {
	t.Helper()
	var out []scenariogolden.Record
	for _, r := range d.f.d.WorkspaceLog(d.f.repo.Dir, "daemon") {
		out = append(out, scenariogolden.Record{Operation: r.Operation, Context: r.Context})
	}
	return out
}

// AwaitRecords implements scenariogolden.Driver.
func (d *daemonDriver) AwaitRecords(t *testing.T, what string, match func(scenariogolden.Record) bool, n int) {
	t.Helper()
	d.f.d.AwaitWorkspaceLogRecordCount(d.f.repo.Dir, what, n, func(r harness.LogRecord) bool {
		return match(scenariogolden.Record{Operation: r.Operation, Context: r.Context})
	})
}

// stamp is STAMP as the fake shim's push takes it.
func stamp(s scenariogolden.Stamp) harness.EntryStamp {
	return harness.EntryStamp{Pointer: s.Pointer, Turn: s.Turn, PlaceMs: s.PlaceMs}
}

// PushPrompt implements scenariogolden.Driver.
func (d *daemonDriver) PushPrompt(t *testing.T, agent string, s scenariogolden.Stamp, prompt *conversationv1.AgentPrompt) {
	t.Helper()
	d.f.shim.PushUserPromptStamped(agent, stamp(s), prompt)
}

// PushFrame implements scenariogolden.Driver.
func (d *daemonDriver) PushFrame(t *testing.T, agent string, s scenariogolden.Stamp, frame *conversationv1.AgentFrame) {
	t.Helper()
	d.f.shim.PushAgentFrameStamped(agent, stamp(s), frame)
}

// PushSessionUpdate implements scenariogolden.Driver.
func (d *daemonDriver) PushSessionUpdate(t *testing.T, u *conversationv1.SessionUpdate) {
	t.Helper()
	d.f.shim.PushSessionUpdate(u)
}

// AwaitRow implements scenariogolden.Driver.
func (d *daemonDriver) AwaitRow(t *testing.T, what string, match func(*frontendv1.FeedRow) bool) {
	t.Helper()
	d.f.awaitRowInFeed(nil, what, match)
}

// RootRows implements scenariogolden.Driver.
func (d *daemonDriver) RootRows(t *testing.T) []*frontendv1.FeedRow {
	t.Helper()
	page, _ := d.f.openFeed(nil)
	return page.GetSuccess().GetRows()
}
