// rosterarm_e2e_test.go — the roster arm's ORDER, against the real world.
//
// THE DEFECT, MEASURED. Emacs observed the walk
// :none -> :ready -> :init -> :submitting -> :thinking on a cold workspace,
// with the `ready` frame arriving ~340ms AFTER SubmitPrompt had already
// answered with a turn id. Both halves contradict
// proto/src/frontend/v1/sidebar.proto:226, which defines ready as "Live,
// proven usable, and idle": a workspace whose session record exists while no
// link state has been observed has proven nothing, and one whose prompt the
// daemon has accepted is not idle. `ready` also cannot precede the `init` it
// is supposed to follow.
//
// The resolver's own ordering is pinned in
// daemon/internal/resolve/sidebar/status_test.go; this file pins it where the
// frames actually reach a client, over the real store, sidecar, daemon and
// Node shim.
package e2e

import (
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"connectrpc.com/connect"

	"claude-repld/integration/harness"
)

// TestRosterNeverPublishesReadyBeforeInit walks a workspace's cold start with
// the roster stream open from before the session exists, and asserts the ORDER
// of the arms that reach the wire.
func TestRosterNeverPublishesReadyBeforeInit(t *testing.T) {
	t.Parallel()
	// Arrange: a registered workspace with no session, watched from now.
	w := NewWorld(t, WorldOpts{})
	repoFixture := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repoFixture.Dir)
	roster := w.WatchRoster()
	defer roster.Close()

	// Act: the cold start.
	raOpenWorkspace(t, w, ws)

	// Assert: the walk up to the first idle arm carries an `init` before it.
	walk := raWalkUntil(t, w, roster, ws, func(arm string) bool { return arm == "ready" })
	sawInit := false
	for _, arm := range walk {
		switch arm {
		case "init":
			sawInit = true
		case "ready":
			if !sawInit {
				t.Fatalf("the roster published ready before any init: %v — sidebar.proto:226 defines ready as \"live, PROVEN usable, and idle\", and nothing had proven this route", walk)
			}
		}
	}
	if !sawInit {
		t.Fatalf("the cold start published no init at all: %v", walk)
	}
}

// TestRosterNeverPublishesReadyAfterAnAcceptedPrompt is the accept side: from
// the instant SubmitPrompt answers with a turn id until that turn ends, the
// row is not idle and must never read `ready`.
func TestRosterNeverPublishesReadyAfterAnAcceptedPrompt(t *testing.T) {
	t.Parallel()
	// Arrange: a live, idle session, watched from the accept onward.
	w := NewWorld(t, WorldOpts{})
	repoFixture := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repoFixture.Dir)
	raOpenWorkspace(t, w, ws)
	roster := w.WatchRoster()
	defer roster.Close()
	harness.AwaitView(t, w.Ctx(), roster, "the workspace's row to settle idle", func(r *frontendv1.WorkspaceRoster) bool {
		return raArm(r, ws.GetId()) == "ready"
	})

	// Act. Plain prose, deliberately with NO `!` prefix: this test wants the
	// ordinary default turn, and a `!`-prefixed prompt that names no
	// registered scenario reaches the same prose scenario while reading like
	// a scenario selection that has gone stale.
	turn := SubmitPrompt(t, w, ws, "hello")

	// Assert: every arm from the ack to the turn's end is a busy one.
	walk := raWalkUntil(t, w, roster, ws, func(arm string) bool { return arm == "done" || arm == "interrupted" })
	for _, arm := range walk[:len(walk)-1] {
		if arm == "ready" {
			t.Fatalf("the roster published ready between the SubmitPrompt ack and the turn's end: %v", walk)
		}
	}
	AwaitTurnEnded(t, w, ws, turn)
}

// raOpenWorkspace sends OpenWorkspace, which is what spawns the real shim.
func raOpenWorkspace(t *testing.T, w *World, ws *workspacev1.WorkspaceRef) {
	t.Helper()
	resp, err := w.Client().OpenWorkspace(w.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: ws}))
	if err != nil {
		t.Fatalf("OpenWorkspace: %v", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("OpenWorkspace = %v, want a success", resp.Msg)
	}
}

// raWalkUntil reads roster frames, recording the workspace's arm from each,
// until one satisfies stop — and answers the whole walk including it. The wait
// is harness.AwaitView's, so it is bounded by the daemon's own context rather
// than by a sleep.
func raWalkUntil(t *testing.T, w *World, roster *harness.Stream[*frontendv1.WorkspaceRoster], ws *workspacev1.WorkspaceRef, stop func(arm string) bool) []string {
	t.Helper()
	var walk []string
	harness.AwaitView(t, w.Ctx(), roster, "the workspace's arm to reach its terminal", func(r *frontendv1.WorkspaceRoster) bool {
		arm := raArm(r, ws.GetId())
		if arm == "" {
			return false
		}
		if len(walk) == 0 || walk[len(walk)-1] != arm {
			walk = append(walk, arm)
		}
		return stop(arm)
	})
	return walk
}

// raArm names one workspace's roster status arm, empty when the roster carries
// no row for it.
func raArm(r *frontendv1.WorkspaceRoster, id string) string {
	arm := ""
	raWalkRows(r, func(row *frontendv1.RosterRow) {
		if row.GetWorkspace().GetWorkspace().GetId() != id {
			return
		}
		switch {
		case row.GetNone() != nil:
			arm = "none"
		case row.GetInit() != nil:
			arm = "init"
		case row.GetReady() != nil:
			arm = "ready"
		case row.GetSubmitting() != nil:
			arm = "submitting"
		case row.GetThinking() != nil:
			arm = "thinking"
		case row.GetPermission() != nil:
			arm = "permission"
		case row.GetDone() != nil:
			arm = "done"
		case row.GetInterrupted() != nil:
			arm = "interrupted"
		case row.GetTurnFailed() != nil:
			arm = "turn_failed"
		case row.GetTurnDied() != nil:
			arm = "turn_died"
		case row.GetVendorBlocked() != nil:
			arm = "vendor_blocked"
		case row.GetIdleAsync() != nil:
			arm = "idle_async"
		case row.GetSevered() != nil:
			arm = "severed"
		case row.GetDegraded() != nil:
			arm = "degraded"
		case row.GetDead() != nil:
			arm = "dead"
		case row.GetStartFailed() != nil:
			arm = "start_failed"
		default:
			arm = "other"
		}
	})
	return arm
}

// raWalkRows visits every row of every grouping, children included. The
// roster nests worktree rows under their parent, so a flat scan of the
// top-level rows would miss exactly the workspaces this suite creates.
func raWalkRows(r *frontendv1.WorkspaceRoster, visit func(*frontendv1.RosterRow)) {
	var walk func(rows []*frontendv1.RosterRow)
	walk = func(rows []*frontendv1.RosterRow) {
		for _, row := range rows {
			visit(row)
			walk(row.GetChildren())
		}
	}
	for _, s := range r.GetRepository().GetSections() {
		walk(s.GetRows().GetRows())
	}
	for _, s := range r.GetTask().GetSections() {
		walk(s.GetRows().GetRows())
	}
	walk(r.GetRecentlyMerged().GetRows().GetRows())
}
