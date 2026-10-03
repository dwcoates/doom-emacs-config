package workspace

import (
	"context"
	"fmt"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/startingshim"
)

// recordSpawnedShimPID makes a spawn DURABLE at the instant of the fork.
//
// IT IS THE WHOLE OF THE INVARIANT: a spawned shim's pid is in the registry
// from the instant it is spawned, so a daemon that finds the workspace lock
// free and the socket absent can tell "no shim" from "a shim the predecessor
// spawned that has not announced itself yet". Everything else here reads what
// this writes.
//
// A FAILED WRITE IS RECORDED AND DOES NOT FAIL THE SPAWN. The process is
// already running: refusing the bring-up now would leave it standing with
// nothing holding it, which is strictly worse than the window this record
// closes. What the record buys is that the next daemon's undetermined
// adoption, if it comes to that, has an explanation on disk.
func (f *Fleet) recordSpawnedShimPID(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, pid int) {
	if pid <= 0 {
		log.Error(opBringUp, "the supervisor answered a spawn with no pid; the successor cannot tell this shim from none",
			dlog.Context{"shim_pid": pid})
		return
	}
	// THE WRITE OUTLIVES THE CALLER'S CANCELLATION. The commonest reason a
	// start is abandoned is its context ending, and the pid of a process that
	// is nonetheless running is exactly what the next daemon needs.
	if err := f.deps.DB.SetSpawnedShimPID(context.WithoutCancel(ctx), ws, &pid); err != nil {
		log.Error(opBringUp, "could not record the spawned shim's pid", dlog.Context{
			"shim_pid": pid, "cause": err.Error(),
		})
		return
	}
	log.Debug(opBringUp, "recorded the spawned shim's pid", dlog.Context{"shim_pid": pid})
}

// awaitStartingSurvivor answers whether a shim a PREVIOUS daemon spawned is
// merely still starting, waiting out the adoption bound for it to bind its
// socket. It reports whether one announced itself and the path it announced on.
//
// It is called on exactly one state: the workspace lock reads FREE and no
// generation of the socket is live. That is what a client-less workspace looks
// like, and it is also what a shim spawned tens of milliseconds ago looks like
// — see startingshim for the measured timeline. The registry's recorded spawn
// pid is what separates them.
//
// AN EXPIRED BOUND IS AN ERROR AND NEVER A SPAWN. A live process that may bind
// the path at any instant is precisely what a second shim must not race: the
// shim itself refuses the bind and dies, and the daemon is left having lost
// the turn to a process nobody adopted.
func (f *Fleet) awaitStartingSurvivor(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, udsPath string) (bool, string, error) {
	record, err := f.deps.DB.Workspace(ctx, ws)
	if err != nil {
		return false, "", fmt.Errorf("start session for %q: read the recorded spawn: %w", ws, err)
	}
	if record.SpawnedShimPID == nil {
		return false, "", nil
	}
	pid := *record.SpawnedShimPID
	if !f.shimAlive(pid) {
		log.Debug(opBringUp, "the recorded spawn's process is gone; nothing survives to wait for", dlog.Context{
			"shim_pid": pid, "socket": udsPath,
		})
		return false, "", nil
	}
	log.Info(opBringUp, "a shim spawned for this workspace is still starting; waiting for it to announce itself rather than spawning a second one",
		dlog.Context{"shim_pid": pid, "socket": udsPath, "bound_ms": f.adoptBound.Milliseconds()})
	path, outcome := f.starting.Await(ctx, &pid, udsPath, f.adoptBound)
	switch outcome {
	case startingshim.OutcomeAnnounced:
		log.Info(opBringUp, "the starting shim announced itself; adopting it as the inert survivor it is", dlog.Context{
			"shim_pid": pid, "socket": path,
		})
		return true, path, nil
	case startingshim.OutcomeSpawnDead:
		log.Info(opBringUp, "the starting shim died before it announced itself; this bring-up spawns", dlog.Context{
			"shim_pid": pid, "socket": udsPath,
		})
		return false, "", nil
	default:
		err := fmt.Errorf(
			"start session for %q: the shim recorded as spawned (pid %d) is alive but did not bind %q within %s: the workspace is left undetermined rather than spawned onto",
			ws, pid, udsPath, f.adoptBound)
		log.Error(opBringUp, "a shim recorded as spawned is alive but never announced itself within the adoption bound; the workspace is left undetermined",
			dlog.Context{"shim_pid": pid, "socket": udsPath, "bound_ms": f.adoptBound.Milliseconds()})
		return false, "", err
	}
}

// shimAlive answers the injected liveness probe, or the kernel's.
func (f *Fleet) shimAlive(pid int) bool {
	if f.deps.ShimAlive != nil {
		return f.deps.ShimAlive(pid)
	}
	return startingshim.Alive(pid)
}
