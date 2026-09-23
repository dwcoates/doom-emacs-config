// replay_e2e_test.go — the owner's replay rule (2026-09-23), across the real
// daemon, the real (--fake) shim, store and sidecar: history is replayed only
// when a workspace is OPENED or a transcript is SELECTED, and only the first
// page. A turn opening never replays it.
//
// The evidence is the systems' own records, matched on operation and
// structured context, never on prose:
//
//   - the daemon's `daemon.sessionwatcher.turn_opened` states how many entries
//     StartTurn's opening page carried, and R15 makes a turn's own page the
//     prompt row alone;
//   - the daemon's `daemon.workspace.bring_up` "opened the session's watches"
//     record states each watcher's opening and whether it replays history;
//   - the feed's history effects (`daemon.feed.clear_confirmed`,
//     `daemon.feed.delivery_bound_moved`) must not fire for a conversation
//     that never cleared or compacted.
package e2e

import (
	"testing"

	"claude-repld/integration/harness"
)

// rpTurnPageEntries answers the entry count StartTurn's opening page carried
// for one turn, waiting for the daemon's own record of the hand-over.
func rpTurnPageEntries(t *testing.T, w *World, workspaceDir, turn string) float64 {
	t.Helper()
	rec := w.Daemon.AwaitLogRecord(harness.WorkspaceLogPath(workspaceDir, "daemon"),
		"the turn_opened record of turn "+turn,
		func(r harness.LogRecord) bool {
			return r.Operation == "daemon.sessionwatcher.turn_opened" && r.Context["turn_id"] == turn
		})
	entries, ok := rec.Context["entries"].(float64)
	if !ok {
		t.Fatalf("turn_opened for %s carries no entry count: %v", turn, rec.Context)
	}
	return entries
}

// TestATurnOpeningNeverReplaysHistory drives two turns on one workspace and
// asserts that neither turn's opening page carried anything but its own
// prompt row: the second turn, whose conversation already holds the first,
// is the case that used to replay the newest 200 entries.
func TestATurnOpeningNeverReplaysHistory(t *testing.T) {
	t.Parallel()
	// Arrange.
	w := NewWorld(t, WorldOpts{})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)
	first := SubmitPrompt(t, w, ws, "say something")
	AwaitTurnEnded(t, w, ws, first)

	// Act.
	second := SubmitPrompt(t, w, ws, "say something else")
	AwaitTurnEnded(t, w, ws, second)

	// Assert.
	for _, turn := range []string{first.GetValue(), second.GetValue()} {
		if got := rpTurnPageEntries(t, w, repo.Dir, turn); got != 1 {
			t.Errorf("turn %s opened on a page of %v entries, want 1: its own prompt row", turn, got)
		}
	}
	for _, rec := range w.Daemon.WorkspaceLog(repo.Dir, "daemon") {
		switch rec.Operation {
		case "daemon.feed.clear_confirmed", "daemon.feed.delivery_bound_moved":
			t.Errorf("a history effect fired in a conversation that never cleared: %s %v", rec.Operation, rec.Context)
		}
	}
}

// TestAWorkspaceOpenReplaysItsFirstPageOnce covers the other half of the
// rule: the workspace's opening is a replay, it is the only one, and the turns
// that follow start no watcher of their own.
func TestAWorkspaceOpenReplaysItsFirstPageOnce(t *testing.T) {
	t.Parallel()
	// Arrange.
	w := NewWorld(t, WorldOpts{})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)

	// Act.
	first := SubmitPrompt(t, w, ws, "say something")
	AwaitTurnEnded(t, w, ws, first)
	second := SubmitPrompt(t, w, ws, "say something else")
	AwaitTurnEnded(t, w, ws, second)

	// Assert.
	var openings []any
	for _, rec := range w.Daemon.WorkspaceLog(repo.Dir, "daemon") {
		if opening, ok := rec.Context["opening"]; ok && rec.Operation == "daemon.workspace.bring_up" {
			openings = append(openings, opening)
		}
	}
	if len(openings) != 1 || openings[0] != "workspace_opened" {
		t.Fatalf("watcher openings = %v, want exactly one workspace_opened", openings)
	}
}
