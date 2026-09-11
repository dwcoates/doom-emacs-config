package boot

import (
	"context"
	"errors"
	"fmt"
	"sync"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/rollout"
	"claude-repld/internal/sessionlock"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/shimsocket"
	"claude-repld/internal/wsm"
)

// sequence is the boot sequence.
type sequence struct {
	deps        Deps
	probe       ProbeFunc
	socketProbe SocketProbeFunc
	now         func() time.Time
	// adoptBound bounds ONE surviving shim's adoption. See DefaultAdoptBound:
	// the daemon's listener is already bound and advertised while this step
	// runs, so an unbounded adoption is a daemon that listens and never
	// accepts.
	adoptBound time.Duration
}

// Joining reports whether this daemon was spawned as a successor. The
// distinguishing fact is the explicit joining argument, never a race: an
// unflagged second daemon lost the address claim before it ever got here.
func (s *sequence) Joining() bool { return s.deps.JoiningAddress != "" }

// Run performs the whole reconciliation in the settled order: adopt the shims
// that outlived the last daemon, reconcile the intent manifest, restore the
// holds all-or-nothing, close the orphaned turns of the workspaces nothing is
// serving, recover the in-flight merges, and — for a successor — run the join
// half of the handover.
//
// EVERY STEP FAILS THE BOOT. There is no partial boot: a reconciliation the
// daemon could not finish is a daemon that does not know what it owns.
func (s *sequence) Run(ctx context.Context) (Report, error) {
	log := s.deps.Log.Global()
	log.Debug("daemon.boot.run", "reconciling the durable state", dlog.Context{
		"joining": s.Joining(),
		"run_dir": s.deps.RunDir,
	})

	report := Report{}
	workspaces, err := s.deps.DB.ListWorkspaces(ctx)
	if err != nil {
		log.Error("daemon.boot.run", "the workspace registry could not be read", dlog.Context{
			"error": err.Error(),
		})
		return Report{}, fmt.Errorf("boot: read the workspace registry: %w", err)
	}

	// A JOINING DAEMON RECONCILES NOTHING. It owns no workspace yet -- the
	// incumbent still does, and is still writing -- so its state handle is
	// READ-ONLY and every reconciliation step here is a write it must not
	// make: adopting a shim the incumbent is serving, closing turns the
	// incumbent's sessions are still running, recovering a merge the incumbent
	// is still driving. Each workspace's state is reconciled as it is
	// TRANSFERRED, which is the rendezvous the join arms below.
	if !s.Joining() {
		clientless, err := s.adopt(ctx, log, workspaces, &report)
		if err != nil {
			return Report{}, err
		}
		if err := s.reconcileManifest(ctx, log, &report); err != nil {
			return Report{}, err
		}
		if err := s.restoreHolds(ctx, log, &report); err != nil {
			return Report{}, err
		}
		if err := s.closeOrphans(ctx, log, clientless, &report); err != nil {
			return Report{}, err
		}
		if err := s.recoverMerges(ctx, log, workspaces, &report); err != nil {
			return Report{}, err
		}
	}
	if err := s.join(ctx, log); err != nil {
		return Report{}, err
	}

	log.Debug("daemon.boot.run", "the boot reconciliation is complete", dlog.Context{
		"adopted":          len(report.Adopted),
		"undetermined":     len(report.Undetermined),
		"orphans_closed":   len(report.Orphaned),
		"holds_restored":   report.HoldsRestored,
		"merges_recovered": len(report.MergesRecovered),
		"dispositions":     len(report.Dispositions),
	})
	return report, nil
}

// survivor is one workspace whose lock reads HELD: a shim this boot must dial
// before it can say what the workspace is. The dial's outcome is filled in by
// the concurrent pass and read by the sequential one that follows it.
type survivor struct {
	ws         wsm.Workspace
	socketPath string
	client     shimclient.Client
	err        error
	overran    bool
}

// adopt probes every open workspace's shim-held kernel lock and ADOPTS the
// shims that are still holding one — never kill-and-restart, because the
// process on the other side of that lock is the only thing that may touch the
// conversation's transcript. It answers the workspaces whose lock read FREE,
// which are the client-less ones whose turns are orphans.
//
// THE PROBING IS SEQUENTIAL AND THE DIALLING IS NOT. Each survivor's dial is
// bounded by adoptBound, and a boot that paid those bounds one after another
// paid N of them: the whole point of the bound is that the daemon reaches its
// accept loop, and N silent survivors put it N bounds away from serving. The
// dials therefore run CONCURRENTLY and the bound is paid ONCE for all of them.
// Everything that touches the report or the state root stays on this
// goroutine and in workspace order, so the boot's outcome does not depend on
// which dial answered first.
func (s *sequence) adopt(ctx context.Context, log dlog.Logger, workspaces []wsm.Workspace, report *Report) ([]wsm.Workspace, error) {
	var clientless []wsm.Workspace
	var survivors []*survivor
	for _, ws := range workspaces {
		if ws.Closed {
			log.Debug("daemon.boot.adopt", "a closed workspace has no shim to adopt", dlog.Context{
				"workspace_id": string(ws.ID),
			})
			continue
		}
		state, err := s.probe(s.deps.RunDir, ws.Dir)
		// THE SOCKET PATH IS THE SHIM'S CURRENT GENERATION, not the layout's
		// base name. A relaunch moved the workspace's shim onto
		// `<base>.nN.sock` and the counter that minted N lived in the last
		// daemon's memory, so a boot that dials the base path dials a path the
		// survivor has not held since; its lock still reads HELD, so the
		// redial ladder never stops. See shimsocket.NewestLive.
		socketPath, socket, socketErr := shimsocket.NewestLive(s.socketProbe, s.deps.Layout.ShimSocket(string(ws.ID)))
		// THE SOCKET IS THE SECOND KERNEL FACT. A lock that reads FREE says
		// no shim CLAIMS this conversation; it does not say no shim is
		// LISTENING for it. A survivor still bound to the path is reachable,
		// and the daemon that spawned over it put a newcomer onto a path it
		// could not bind — the newcomer died, this daemon dialed the path and
		// reached the SURVIVOR, which refused StartSession `already_started`
		// and the turn was lost to a shim nobody adopted. So a live listener
		// is adopted whatever the lock says, and the disagreement is recorded
		// rather than resolved silently.
		if state == sessionlock.StateFree && socket == shimsocket.StateLive {
			log.Warn("daemon.boot.adopt", "the workspace lock reads free but a shim is listening; adopting the survivor",
				dlog.Context{
					"workspace_id": string(ws.ID),
					"socket_path":  socketPath,
					"lock_state":   state.String(),
				})
			state = sessionlock.StateHeld
		}
		// A SOCKET THAT COULD NOT BE PROBED IS NEVER SPAWNED OVER, for the
		// same reason an unreadable lock is never read as free: the answer
		// this daemon needs is "is anybody there", and "could not tell" is
		// not it.
		if state == sessionlock.StateFree && socket == shimsocket.StateUndetermined {
			context := dlog.Context{"workspace_id": string(ws.ID), "socket_path": socketPath}
			if socketErr != nil {
				context["error"] = socketErr.Error()
			}
			log.Warn("daemon.boot.adopt", "the shim socket probe could not tell; never read as free", context)
			report.Undetermined = append(report.Undetermined, ws.ID)
			continue
		}
		switch {
		case state == sessionlock.StateHeld:
			survivors = append(survivors, &survivor{ws: ws, socketPath: socketPath})
		case state == sessionlock.StateFree:
			// NOTHING HOLDS AND NOTHING LISTENS, so a socket FILE left here is
			// a dead shim's leavings — an AF_UNIX path is not reclaimed on
			// process death the way a flock is — and the next spawn would
			// fail to bind it for a reason that no longer exists. Clearing it
			// is part of declaring the workspace client-less; a clear that
			// fails fails the BOOT rather than leaving a path the next spawn
			// will trip over.
			if clearErr := shimsocket.ClearStale(log, socketPath); clearErr != nil {
				log.Error("daemon.boot.adopt", "the dead shim's socket path could not be cleared", dlog.Context{
					"workspace_id": string(ws.ID),
					"socket_path":  socketPath,
					"error":        clearErr.Error(),
				})
				return nil, fmt.Errorf("boot: clear the stale shim socket of %s: %w", ws.ID, clearErr)
			}
			log.Debug("daemon.boot.adopt", "no shim survives for this workspace", dlog.Context{
				"workspace_id": string(ws.ID),
			})
			clientless = append(clientless, ws)
		default:
			context := dlog.Context{"workspace_id": string(ws.ID), "state": state.String()}
			if err != nil {
				context["error"] = err.Error()
			}
			log.Warn("daemon.boot.adopt", "the workspace lock probe could not tell; never read as free", context)
			report.Undetermined = append(report.Undetermined, ws.ID)
		}
	}

	s.dialSurvivors(ctx, survivors)

	for _, sv := range survivors {
		// THE ADOPTION IS BOUNDED, AND ITS OVERRUN IS NOT A BOOT FAILURE.
		// `shimclient.bringUp` redials a shim whose lock reads held
		// FOREVER — correctly, because a held lock is a living process — so a
		// survivor whose socket path is gone (a rolled generation, an
		// unlinked path) is a condition that never resolves. This step runs
		// BEFORE the daemon serves, so waiting it out is a listener nobody
		// accepts on: pid 31984 sat there for ten hours with its accept queue
		// full while Emacs and curl timed out on connect.
		//
		// An overrun is therefore reported at ERROR and the workspace is
		// UNDETERMINED: the lock says the conversation is owned, so its turns
		// are NOT orphan-closed and no second shim is spawned onto it,
		// exactly as for a probe that could not tell. The boot goes on and
		// the daemon serves.
		if sv.overran {
			log.Error("daemon.boot.adopt", "a surviving shim did not answer within the adoption bound; the workspace is left undetermined and the daemon serves", dlog.Context{
				"workspace_id": string(sv.ws.ID),
				"socket_path":  sv.socketPath,
				"bound_ms":     s.adoptBound.Milliseconds(),
				"error":        sv.err.Error(),
			})
			report.Undetermined = append(report.Undetermined, sv.ws.ID)
			continue
		}
		if sv.err != nil {
			log.Error("daemon.boot.adopt", "a surviving shim could not be adopted", dlog.Context{
				"workspace_id": string(sv.ws.ID),
				"error":        sv.err.Error(),
			})
			return nil, fmt.Errorf("boot: adopt the surviving shim of %s: %w", sv.ws.ID, sv.err)
		}
		if installErr := s.deps.Adopted(ctx, sv.ws.ID, sv.client); installErr != nil {
			log.Error("daemon.boot.adopt", "an adopted shim could not be installed", dlog.Context{
				"workspace_id": string(sv.ws.ID),
				"error":        installErr.Error(),
			})
			return nil, fmt.Errorf("boot: install the adopted shim of %s: %w", sv.ws.ID, installErr)
		}
		log.Debug("daemon.boot.adopt", "a surviving shim was adopted", dlog.Context{
			"workspace_id": string(sv.ws.ID),
		})
		report.Adopted = append(report.Adopted, sv.ws.ID)
	}
	return clientless, nil
}

// dialSurvivors dials every survivor AT ONCE, each under its own adoption
// bound, and fills each one's outcome in. Nothing here touches the report or
// the state root: the caller reads the outcomes back in workspace order.
func (s *sequence) dialSurvivors(ctx context.Context, survivors []*survivor) {
	var wg sync.WaitGroup
	for _, sv := range survivors {
		wg.Add(1)
		go func(sv *survivor) {
			defer wg.Done()
			adoptCtx, cancelAdopt := context.WithTimeout(ctx, s.adoptBound)
			defer cancelAdopt()
			sv.client, sv.err = s.deps.Supervisor.Adopt(adoptCtx, sv.ws.ID, sv.ws.Dir, sv.socketPath)
			sv.overran = sv.err != nil && errors.Is(adoptCtx.Err(), context.DeadlineExceeded) && ctx.Err() == nil
		}(sv)
	}
	wg.Wait()
}

// reconcileManifest reads the outgoing daemon's stand-down intent against the
// kernel locks actually held. The rollout controller persists all four
// dispositions as faults; PRESERVED, ROLLED, DIED and UNKNOWN are never
// collapsed, because WHICH sessions silently died is the whole point.
func (s *sequence) reconcileManifest(ctx context.Context, log dlog.Logger, report *Report) error {
	dispositions, err := s.deps.Rollout.Reconcile(ctx, report.Adopted)
	if err != nil {
		log.Error("daemon.boot.reconcile", "the intent manifest could not be reconciled", dlog.Context{
			"error": err.Error(),
		})
		return fmt.Errorf("boot: reconcile the intent manifest: %w", err)
	}
	for _, d := range dispositions {
		log.Debug("daemon.boot.reconcile", "a session's bounce disposition", dlog.Context{
			"workspace_id": string(d.Workspace),
			"intent":       string(d.Intent),
			"lock":         d.Lock.String(),
			"disposition":  string(d.Kind),
		})
	}
	report.Dispositions = dispositions
	return nil
}

// restoreHolds reloads every standing hold. It is ALL-OR-NOTHING: a corrupt
// record restores nothing and fails the boot, because a partial set silently
// loses what users typed.
func (s *sequence) restoreHolds(ctx context.Context, log dlog.Logger, report *Report) error {
	if err := s.deps.Queue.RestoreHolds(ctx); err != nil {
		log.Error("daemon.boot.restore_holds", "the held prompts could not be restored; nothing was loaded", dlog.Context{
			"error": err.Error(),
		})
		return fmt.Errorf("boot: restore the held prompts: %w", err)
	}
	held, err := s.deps.DB.AllHeldPrompts(ctx)
	if err != nil {
		log.Error("daemon.boot.restore_holds", "the restored holds could not be counted", dlog.Context{
			"error": err.Error(),
		})
		return fmt.Errorf("boot: count the restored holds: %w", err)
	}
	log.Debug("daemon.boot.restore_holds", "the held prompts were restored whole", dlog.Context{
		"holds": len(held),
	})
	report.HoldsRestored = len(held)
	return nil
}

// closeOrphans closes, in ONE TRANSACTION per workspace, every turn that never
// got a terminal in a workspace nothing is serving. An ADOPTED workspace is
// deliberately absent: its turns are still running and the sessionwatcher the
// adoption installed re-opens them.
func (s *sequence) closeOrphans(ctx context.Context, log dlog.Logger, clientless []wsm.Workspace, report *Report) error {
	at := s.now()
	for _, ws := range clientless {
		orphans, err := s.deps.DB.CloseOrphans(ctx, ws.ID, at)
		if err != nil {
			log.Error("daemon.boot.close_orphans", "a client-less workspace's orphaned turns could not be closed", dlog.Context{
				"workspace_id": string(ws.ID),
				"error":        err.Error(),
			})
			return fmt.Errorf("boot: close the orphaned turns of %s: %w", ws.ID, err)
		}
		log.Debug("daemon.boot.close_orphans", "a client-less workspace's orphaned turns were closed", dlog.Context{
			"workspace_id": string(ws.ID),
			"turns":        len(orphans.Turns),
		})
		report.Orphaned = append(report.Orphaned, orphans.Turns...)
	}
	return nil
}

// recoverMerges resumes the merges that were in flight or waiting. The
// orchestrator re-queues an in-flight merge at the FRONT of its repository's
// queue and re-runs it from the queue tab (the recorded override): the git
// state after a crash is only trustworthy from a clean re-run.
func (s *sequence) recoverMerges(ctx context.Context, log dlog.Logger, workspaces []wsm.Workspace, report *Report) error {
	inFlight, err := s.mergeLeases(ctx, log, workspaces)
	if err != nil {
		return err
	}
	if err := s.deps.Merge.Recover(ctx); err != nil {
		log.Error("daemon.boot.recover_merges", "the in-flight merges could not be recovered", dlog.Context{
			"error": err.Error(),
		})
		return fmt.Errorf("boot: recover the in-flight merges: %w", err)
	}
	log.Debug("daemon.boot.recover_merges", "the in-flight merges were recovered", dlog.Context{
		"merges": len(inFlight),
	})
	report.MergesRecovered = inFlight
	return nil
}

// mergeLeases names the workspaces whose occupancy lease is held by the merge
// orchestrator, which is what a merge in flight across a crash looks like.
// They are read BEFORE the recovery runs, because the recovery is what
// releases them.
func (s *sequence) mergeLeases(ctx context.Context, log dlog.Logger, workspaces []wsm.Workspace) ([]ids.WorkspaceID, error) {
	var inFlight []ids.WorkspaceID
	for _, ws := range workspaces {
		lease, ok, err := s.deps.DB.Lease(ctx, ws.ID)
		if err != nil {
			log.Error("daemon.boot.recover_merges", "a workspace's lease could not be read", dlog.Context{
				"workspace_id": string(ws.ID),
				"error":        err.Error(),
			})
			return nil, fmt.Errorf("boot: read the lease of %s: %w", ws.ID, err)
		}
		if ok && lease.Holder == wsm.HolderMerge {
			inFlight = append(inFlight, ws.ID)
		}
	}
	return inFlight, nil
}

// join runs the successor's half of a handover: read the intent manifest, arm
// the rendezvous, and start each headless adoption without a participant call.
// Adoption itself remains behind the incumbent's serving release at freeness.
// A daemon that is not joining owns its workspaces already and does nothing
// here.
func (s *sequence) join(ctx context.Context, log dlog.Logger) error {
	if !s.Joining() {
		log.Debug("daemon.boot.join", "this daemon is the incumbent; nothing to join", nil)
		return nil
	}
	if err := s.deps.Rollout.Join(ctx); err != nil {
		log.Error("daemon.boot.join", "the joining daemon could not take over", dlog.Context{
			"incumbent": s.deps.JoiningAddress,
			"error":     err.Error(),
		})
		return fmt.Errorf("boot: join the incumbent at %s: %w", s.deps.JoiningAddress, err)
	}
	log.Debug("daemon.boot.join", "the joining daemon armed its rendezvous", dlog.Context{
		"incumbent": s.deps.JoiningAddress,
	})
	return nil
}

// compile-time assertion that a rollout disposition's four kinds stay
// distinguishable in the report: the report carries the values themselves
// rather than a count, so nothing downstream can collapse them.
var _ = []rollout.DispositionKind{
	rollout.DispositionPreserved,
	rollout.DispositionRolled,
	rollout.DispositionDied,
	rollout.DispositionUnknown,
}
