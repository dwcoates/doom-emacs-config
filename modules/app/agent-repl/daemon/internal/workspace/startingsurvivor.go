package workspace

import (
	"context"
	"fmt"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/startingshim"
)

// spawnRecorded spawns SPEC's shim with its spawn DURABLE from the fork until
// the shim announces itself.
//
// IT IS THE WHOLE OF THE INVARIANT: a spawned shim's identity is in the
// registry from the instant it is spawned until it has bound its socket and
// answered, so a daemon that finds the workspace lock free and the socket
// absent can tell "no shim" from "a shim the predecessor spawned that has not
// announced itself yet". Everything else here reads what this writes.
//
// THE RECORD IS RETIRED AT THE ANNOUNCEMENT. From then on the shim's live
// socket is its trace, and a record left standing would outlive the shim and
// name whatever process the kernel gives its pid to next: a record left from
// 2026-11-20 19:33 named a root daemon the next morning, and the workspace
// waited on it every 10s (2026-11-21). A spawn that fails leaves its record
// for the stand-down that sweeps it; the stamp already tells its pid's next
// holder from it.
func (f *Fleet) spawnRecorded(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, spec shimclient.Spec) (shimclient.Client, error) {
	var spawned *ids.ProcessIdentity
	spec.Spawned = func(spawn ids.ProcessIdentity) {
		if f.recordSpawnedShim(ctx, log, ws, spawn) {
			spawned = &spawn
		}
	}
	client, err := f.deps.Supervisor.Spawn(ctx, spec)
	if err == nil && spawned != nil {
		f.retireSpawnedShim(ctx, log, ws, *spawned)
	}
	return client, err
}

// recordSpawnedShim makes SPAWN durable at the instant of the fork, answering
// whether it was recorded.
//
// A FAILED WRITE IS RECORDED AND DOES NOT FAIL THE SPAWN. The process is
// already running: refusing the bring-up now would leave it standing with
// nothing holding it, which is strictly worse than the window this record
// closes. What the record buys is that the next daemon's undetermined
// adoption, if it comes to that, has an explanation on disk.
func (f *Fleet) recordSpawnedShim(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, spawn ids.ProcessIdentity) bool {
	fields := dlog.Context{"shim_pid": spawn.PID, "shim_start_unix_micros": spawn.StartUnixMicros}
	if spawn.PID <= 0 {
		log.Error(opBringUp, "the supervisor answered a spawn with no pid; the successor cannot tell this shim from none", fields)
		return false
	}
	// THE WRITE OUTLIVES THE CALLER'S CANCELLATION. The commonest reason a
	// start is abandoned is its context ending, and the identity of a process
	// that is nonetheless running is exactly what the next daemon needs.
	if err := f.deps.DB.SetSpawnedShim(context.WithoutCancel(ctx), ws, &spawn); err != nil {
		log.Error(opBringUp, "could not record the spawned shim", dlog.WithCause(fields, err))
		return false
	}
	log.Debug(opBringUp, "recorded the spawned shim", fields)
	return true
}

// retireSpawnedShim retires SPAWN's record once its shim announced itself. A
// record another spawn wrote since is left as it stands. A failed write is
// recorded: the stamp still keeps the stale record from naming any other
// process, and the stand-down sweeps it.
func (f *Fleet) retireSpawnedShim(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, spawn ids.ProcessIdentity) {
	fields := dlog.Context{"shim_pid": spawn.PID, "shim_start_unix_micros": spawn.StartUnixMicros}
	retired, err := f.deps.DB.RetireSpawnedShim(context.WithoutCancel(ctx), ws, spawn)
	if err != nil {
		log.Error(opBringUp, "could not retire the record of a spawned shim that announced itself", dlog.WithCause(fields, err))
		return
	}
	if !retired {
		log.Debug(opBringUp, "the spawned shim announced itself, and its record was already replaced or cleared", fields)
		return
	}
	log.Debug(opBringUp, "the spawned shim announced itself; its spawn record is retired", fields)
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
	if record.SpawnedShim == nil {
		return false, "", nil
	}
	spawn := *record.SpawnedShim
	pid := spawn.PID
	running, err := f.starting.Running(spawn)
	if err != nil {
		log.Error(opBringUp, "could not read whether the recorded spawn's process still runs; the workspace is left undetermined", dlog.Context{
			"shim_pid": pid, "shim_start_unix_micros": spawn.StartUnixMicros, "socket": udsPath, "cause": err.Error(),
		})
		return false, "", fmt.Errorf("start session for %q: read whether the recorded spawn (pid %d) runs: %w", ws, pid, err)
	}
	if !running {
		log.Debug(opBringUp, "the recorded spawn's process is gone; nothing survives to wait for", dlog.Context{
			"shim_pid": pid, "shim_start_unix_micros": spawn.StartUnixMicros, "socket": udsPath,
		})
		return false, "", nil
	}
	log.Info(opBringUp, "a shim spawned for this workspace is still starting; waiting for it to announce itself rather than spawning a second one",
		dlog.Context{"shim_pid": pid, "socket": udsPath, "bound_ms": f.adoptBound.Milliseconds()})
	path, outcome := f.starting.Await(ctx, &spawn, udsPath, f.adoptBound)
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
