package workspace

import (
	"context"
	"fmt"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

// opServing is the operation every serving claim the fleet makes is recorded
// under.
const opServing = "daemon.workspace.serving"

// claimServing records THIS daemon instance as the workspace's serving owner.
// It is called from the two places the fleet's session map gains a client --
// `hold` (every bring-up: the boot's, a lazy revival's, a re-open's, a
// resume's) and `Install` (the boot's adoption of a survivor, the handover
// successor's adoption, a relaunch's rotation) -- and from nowhere else,
// because those two writes ARE the fleet beginning to serve a shim.
//
// WHY IT IS HERE, not at each caller. The serving row is what a handover
// hands over (rollout's `served`): a workspace nobody claimed is transferred
// by nobody. The claim used to be made only at RegisterWorkspace and at the
// handover's adoption, so a daemon that was COLD-STARTED and brought existing
// workspaces up without Emacs re-registering them served three live sessions
// under a dead instance's row, and its handover skipped all three: the
// successor adopted nothing and the shims were orphaned, alive, holding their
// locks, served by no daemon (2026-09-24, daemon pid 43501, instance
// 86dbf754ae5e4ebb; the rows named ce34b5e9cd834d09). A claim at the map
// write is one no future bring-up path can skip.
//
// THE CLAIM IS UNCONDITIONAL, as wsm.ClaimServing states: the kernel lock is
// the arbitration, and a daemon that holds a live client of the shim holding
// it is the one serving the workspace.
//
// A FAILED CLAIM FAILS THE BRING-UP, loudly. The caller has already recorded
// the client in the session map, so the shim is never left running with
// nothing holding it: this daemon can still stop it, and its handover still
// sees the live session (and says so at ERROR, since the row disagrees).
func (f *Fleet) claimServing(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID) error {
	if err := f.deps.DB.ClaimServing(ctx, ws, f.deps.Instance); err != nil {
		log.Error(opServing, "could not claim the workspace's serving ownership for a session this daemon now holds", dlog.Context{
			"workspace": string(ws), "instance": string(f.deps.Instance), "cause": err.Error(),
		})
		return fmt.Errorf("workspace: claim serving for %q: %w", ws, err)
	}
	log.Debug(opServing, "this daemon serves the workspace", dlog.Context{
		"workspace": string(ws), "instance": string(f.deps.Instance),
	})
	return nil
}
