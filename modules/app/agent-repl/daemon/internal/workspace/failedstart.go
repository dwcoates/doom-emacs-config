package workspace

import (
	"context"
	"errors"
	"fmt"

	"claude-repld/internal/bringup"
	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

// ErrAdoptOwnSpawn is the refusal of an adoption whose target is a shim THIS
// daemon's own supervisor spawned and still holds.
//
// It is a BUG in this daemon, not a state of the world: one process with two
// clients is the shape that made a stand-down report a death for a shim it had
// itself ordered away, and the two clients disagree about everything after
// that -- the exit, the redial, the lock. Nothing recovers it, so the bring-up
// refuses rather than proceeding, and the error names the case so a reader is
// not left deducing it from a pid that appears twice.
var ErrAdoptOwnSpawn = errors.New("workspace: the shim to adopt is one this daemon's own supervisor spawned")

// refuseAdoptingOurOwnSpawn refuses an adoption whose target is a shim this
// daemon spawned and still supervises.
//
// THE FLEET HOLDS EVERY SHIM IT SPAWNS from the spawn on (reuseOrBringUp), and
// a start reuses a held shim before it ever probes, so a bring-up should never
// meet one of its own. This is the GUARD that keeps that true whatever path
// reaches the probe: the two kernel facts the bring-up branches on (a free
// lock, a live socket) are exactly what an inert shim of our own looks like,
// and the supervisor's registry is what tells "a survivor of some previous
// daemon" from "the process we are already holding" (the one-shim-two-clients
// regression, 2026-09-13, shim pid 48170).
func (f *Fleet) refuseAdoptingOurOwnSpawn(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, socketPath, why string) error {
	pid, ours := f.deps.Supervisor.SpawnedFor(ws)
	if !ours {
		return nil
	}
	err := fmt.Errorf("%w: pid %d is listening at %q for workspace %q (%s)", ErrAdoptOwnSpawn, pid, socketPath, ws, why)
	log.Error(opBringUp, "refused to adopt a shim this daemon's own supervisor spawned", dlog.Context{
		"shim_pid": pid, "socket": socketPath, "reason": why,
	})
	f.noteStartFailed(ctx, log, ws, err)
	return err
}

// ErrShimTaken is a start whose shim was taken from it while it ran: a
// handover's transfer detached it for the successor to adopt, or a kill
// stopped it. The start serves nothing and claims nothing. It is
// bringup.ErrNotServed, so every bring-up counts it as stood down rather than
// failed.
var ErrShimTaken = fmt.Errorf("workspace: the shim this start held was taken from it (handed to a successor, or stopped): %w", bringup.ErrNotServed)
