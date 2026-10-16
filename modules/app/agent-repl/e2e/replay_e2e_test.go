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

	"agent-repld/integration/harness"
)

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
