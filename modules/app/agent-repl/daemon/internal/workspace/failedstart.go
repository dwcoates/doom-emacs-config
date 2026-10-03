package workspace

import (
	"context"
	"errors"
	"fmt"
	"time"

	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/bringup"
	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/shimsocket"
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

// socketGonePoll is how often the stop of a failed start's shim re-probes the
// workspace socket. It is the floor under a fact that lives in the KERNEL and
// cannot announce itself; the whole wait is bounded by
// shimclient.GracefulKillBound, which is 1.5s, so this samples it ~75 times.
const socketGonePoll = 20 * time.Millisecond

// refuseAdoptingOurOwnSpawn refuses an adoption whose target is a shim this
// daemon spawned and still supervises.
//
// THE SUPERVISOR'S REGISTRY IS THE ONLY WITNESS. A spawn reaches the fleet's
// session map only after StartSession answers, so for the whole window before
// that the process is known to the supervisor and to nothing else -- and the
// two kernel facts the bring-up branches on (a free lock, a live socket) are
// exactly what an inert shim of our own looks like. The registry is what tells
// "a survivor of some previous daemon" from "the process we are already
// holding".
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

// stopFailedStart stops the shim a FAILED start spawned, through the ordered
// path, and waits for its socket to go.
//
// THE INVARIANT IS THAT A START THAT FAILS LEAVES NO SHIM OF ITS OWN SERVING.
// `Fleet.Start` returns a refused StartSession before it remembers the client,
// so the fleet holds nothing; the shim, meanwhile, rolled its conversation
// locks back inside StartSession and kept serving the workspace socket. The
// next bring-up then read "lock free, socket live", adopted that very process,
// and the daemon had one shim with two clients -- measured 2026-09-13, shim
// pid 48170 spawned at 18:17:56 and adopted at 18:18:41, with the adopted
// client reporting a death at stand-down.
//
// IT IS THE ORDERED PATH, in the order `verbs.kill` uses. The stand-down latch
// is armed FIRST, so the departure this daemon is about to cause is read as
// one it ordered by the exit watcher, the redialer and the adopted-death
// witness; then the session ask, whose refusal is evidence rather than a
// reason to stop; then the unconditional process stop.
//
// EVERY FAILURE HERE IS RECORDED AND NONE OF THEM CHANGES THE CALLER'S ERROR.
// The caller is already returning the start's own refusal, which is the
// sentence the user needs; a leaked shim is a SECOND fault, and its record is
// the only thing that will ever say so.
func (f *Fleet) stopFailedStart(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, client shimclient.Client, udsPath string, cause error) {
	pid := client.PID()
	fields := dlog.Context{"shim_pid": pid, "socket": udsPath, "cause": cause.Error()}

	// THE LATCH IS ARMED BEFORE ANYTHING IS ENDED. See Shim.StandDown: a
	// teardown this daemon orders must be readable as one from the first
	// moment, not from whenever the kill happens to land.
	ordered := client.StandDown()

	// THE STOP GETS ITS OWN BUDGET, DETACHED FROM THE START'S. The commonest
	// reason a start fails is that its context ended, and a kill handed that
	// same dead context would SIGKILL a shim the grace would have let leave
	// cleanly.
	stop, cancel := context.WithTimeout(context.WithoutCancel(ctx), shimclient.GracefulKillBound)
	defer cancel()

	if err := askShimToEnd(stop, client); err != nil {
		// A shim that will not answer is not a reason to leave the process
		// standing: the stop below is unconditional and the refusal is
		// evidence. It is INFO when this daemon ordered the stand-down -- and
		// a start that just failed has no session to end, so `no_session` is
		// the ORDINARY answer here rather than a defect.
		evidence := dlog.Context{"shim_pid": pid, "cause": err.Error()}
		if ordered {
			log.Info(opBringUp, "the failed start's KillSession did not answer", evidence)
		} else {
			log.Warn(opBringUp, "the failed start's KillSession did not answer", evidence)
		}
	}

	if err := client.Kill(stop, shimclient.KillAttribution{
		Actor:  "workspace.failed_start",
		Reason: "the start that spawned this shim failed: " + cause.Error(),
		Force:  false,
	}); err != nil && !errors.Is(err, shimclient.ErrDetached) {
		log.Error(opBringUp, "the failed start's shim could not be stopped", dlog.Context{
			"shim_pid": pid, "cause": err.Error(),
		})
	}

	// THE SOCKET IS THE FACT THE NEXT BRING-UP READS, so it is the fact this
	// waits on. A process that is gone but whose socket is still bound would
	// be adopted by the very next probe, which is the state this whole
	// function exists to prevent.
	settle, endSettle := context.WithTimeout(context.WithoutCancel(ctx), f.socketGoneBound)
	defer endSettle()
	state, gone := f.awaitSocketGone(settle, udsPath)
	fields["socket_state"] = state.String()
	if !gone {
		log.Error(opBringUp, "the failed start's shim is still reachable on the workspace socket after the stop", fields)
		return
	}
	// THE RECORDED SPAWN GOES WITH THE PROCESS. The pid was made durable at
	// the fork so a successor would wait for this shim rather than spawn over
	// it; left behind, that successor would wait out its whole adoption bound
	// for a process this daemon has just stopped.
	f.clearSpawnedShimPID(ctx, log, ws)
	log.Info(opBringUp, "stopped the shim of a failed start", fields)
}

// askShimToEnd sends the forced KillSession and reads its typed refusal, so a
// shim that refused is told from a shim that never answered only by the text.
func askShimToEnd(ctx context.Context, client shimclient.Client) error {
	response, err := client.KillSession(ctx, &shimv1.KillSessionRequest{Force: true})
	if err != nil {
		return err
	}
	if failure := response.GetFailure(); failure != nil {
		return &ShimRefusal{Verb: "KillSession", Arm: killSessionArm(failure), Detail: failure.GetDetail()}
	}
	return nil
}

// awaitSocketGone polls the workspace socket until no listener is there,
// answering the last state it saw and whether it concluded the socket is gone.
//
// AN UNDETERMINED PROBE IS NEVER READ AS GONE, for the same reason an
// unreadable lock is never read as free: the whole point of the wait is that
// the NEXT bring-up must not find a listener, and "could not tell" does not
// say it will not.
func (f *Fleet) awaitSocketGone(ctx context.Context, udsPath string) (shimsocket.State, bool) {
	ticker := time.NewTicker(socketGonePoll)
	defer ticker.Stop()
	for {
		_, state, _ := shimsocket.NewestLive(f.socketProbe, udsPath)
		if state == shimsocket.StateAbsent || state == shimsocket.StateStale {
			return state, true
		}
		select {
		case <-ticker.C:
		case <-ctx.Done():
			return state, false
		}
	}
}

// ErrHandedOver is a start that finished after a handover took its workspace
// from this daemon (Fleet.HandOver): its shim was left running for the
// successor, and this daemon serves nothing. It is bringup.ErrNotServed, so
// every bring-up counts it as stood down rather than failed.
var ErrHandedOver = fmt.Errorf("workspace: the workspace was handed to a successor while its start ran: %w", bringup.ErrNotServed)
