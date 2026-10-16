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
	frontendv1 "agentrepl/proto/frontend/v1"
	"agentrepl/protohelpers/rosterwalk"
)

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
		case row.GetQuestion() != nil:
			arm = "question"
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
	for _, row := range rosterwalk.AllRows(r) {
		visit(row)
	}
}
