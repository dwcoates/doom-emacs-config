package boot

import (
	"context"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"sync"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/health"
	"claude-repld/internal/ids"
	"claude-repld/internal/rollout"
	"claude-repld/internal/sessionlock"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/shimsocket"
	"claude-repld/internal/startingshim"
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
	// starting waits for a PREDECESSOR'S still-starting shim to announce
	// itself before this boot concludes no shim survives. See startingshim.
	starting startingshim.Waiter
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
	// THE VIEWS ARE BOUND BEFORE ANY STEP CAN PUBLISH. The manifest's
	// dispositions, the orphan closes and the missing-directory closes all
	// reach the footer, and until this ran they reached it for every
	// workspace before PublishRegistry had bound a single directory: one
	// invariant-violation record per workspace on every boot.
	if err := s.deps.BindViews(ctx); err != nil {
		log.Error("daemon.boot.run", "the workspaces' views could not be bound", dlog.Context{
			"error": err.Error(),
		})
		return Report{}, fmt.Errorf("boot: bind the workspaces' views: %w", err)
	}
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
		// THE MISSING DIRECTORIES ARE CLOSED FIRST, because every step below
		// asks something of a workspace that is still open, and a workspace
		// whose worktree is gone can answer none of it.
		if err := s.closeMissingDirs(ctx, log, workspaces, &report); err != nil {
			return Report{}, err
		}
		if err := s.releaseClosedServing(ctx, log, workspaces, &report); err != nil {
			return Report{}, err
		}
		clientless, err := s.adopt(ctx, log, workspaces, &report)
		if err != nil {
			return Report{}, err
		}
		if err := s.reconcileManifest(ctx, log, &report); err != nil {
			return Report{}, err
		}
		s.adoptedAttachedHealthy(ctx, log, &report)
		if err := s.restoreHolds(ctx, log, &report); err != nil {
			return Report{}, err
		}
		if err := s.closeOrphans(ctx, log, clientless, &report); err != nil {
			return Report{}, err
		}
		if err := s.recoverMerges(ctx, log, workspaces, &report); err != nil {
			return Report{}, err
		}
		// THE ORPHANED LEASES GO LAST, after the merge recovery has resolved
		// the merge leases it owns the reading of.
		if err := s.releaseOrphanLeases(ctx, log, &report); err != nil {
			return Report{}, err
		}
		// THE BRING-UP IS ONLY NAMED HERE, and it is named LAST for the same
		// reason it used to RUN last: the sessions it starts would race every
		// reconciliation above it — the orphaned turns are closed, the holds
		// are restored, the merges are recovered — and a session spawned
		// before those would answer for state this boot had not finished
		// reading. It does not run here because it must not gate the
		// listener; see BringUp.
		report.PendingBringUp = clientless
	}
	if err := s.join(ctx, log); err != nil {
		return Report{}, err
	}

	log.Debug("daemon.boot.run", "the boot reconciliation is complete", dlog.Context{
		"adopted":            len(report.Adopted),
		"pending_bring_up":   len(report.PendingBringUp),
		"orphan_leases":      len(report.OrphanLeases),
		"undetermined":       len(report.Undetermined),
		"orphans_closed":     len(report.Orphaned),
		"missing_dir_closed": len(report.MissingDirClosed),
		"holds_restored":     report.HoldsRestored,
		"merges_recovered":   len(report.MergesRecovered),
		"dispositions":       len(report.Dispositions),
	})
	return report, nil
}

// releaseClosedServing heals every CLOSED workspace whose row still names a
// serving instance or a spawned shim pid. A closed workspace is served by no
// daemon, and every close releases both in the same write (wsm.SetClosed), so
// such a row is an invariant violation a leaked close path left behind — it is
// repaired through that same helper and stated at ERROR. Left alone, every
// later handover transferred it (2026-09-27, 498b3b658c074bf4).
func (s *sequence) releaseClosedServing(ctx context.Context, log dlog.Logger, workspaces []wsm.Workspace, report *Report) error {
	const op = "daemon.boot.release_closed_serving"
	for _, ws := range workspaces {
		if !ws.Closed {
			continue
		}
		owner, err := s.deps.DB.Serving(ctx, ws.ID)
		if err != nil {
			log.Error(op, "could not read a closed workspace's serving ownership", dlog.Context{
				dlog.KeyWorkspaceID: string(ws.ID), "error": err.Error(),
			})
			return fmt.Errorf("boot: read the serving ownership of closed workspace %s: %w", ws.ID, err)
		}
		if owner == nil && ws.SpawnedShimPID == nil {
			continue
		}
		fields := dlog.Context{dlog.KeyWorkspaceID: string(ws.ID), "owner": ""}
		if owner != nil {
			fields["owner"] = string(*owner)
		}
		if ws.SpawnedShimPID != nil {
			fields["spawned_shim_pid"] = *ws.SpawnedShimPID
		}
		log.Error(op, "a closed workspace was still recorded as served; its serving ownership is released", fields)
		if err := s.deps.DB.SetClosed(ctx, ws.ID, true); err != nil {
			fields["error"] = err.Error()
			log.Error(op, "could not release a closed workspace's serving ownership", fields)
			return fmt.Errorf("boot: release the serving ownership of closed workspace %s: %w", ws.ID, err)
		}
		report.ClosedServingReleased = append(report.ClosedServingReleased, ws.ID)
	}
	return nil
}

// closeMissingDirs closes every open workspace whose directory is GONE.
//
// A workspace is a worktree plus the editor state over it, so a registry row
// whose directory no longer exists names nothing a user can work in: Emacs
// read the roster, opened a tab for it, and every per-workspace verb on that
// tab then failed on a path that is not there. Closing the row is what takes
// it out of the roster's live half (internal/resolve/sidebar draws a closed
// row receded and `inactive`), and it is the same durable flag an explicit
// CloseWorkspace sets.
//
// IT DOES NOT GO THROUGH THE CLOSE VERB'S QUIET GATE. That gate exists so a
// USER's close cannot discard undelivered intent; a directory that does not
// exist can neither receive intent nor be worked in, and refusing the close
// would leave exactly the unopenable tab this step is here to prevent. It is
// also not a re-close: a merged or nuked workspace keeps its registry row with
// its directory removed and is ALREADY closed, so this step passes over it.
//
// A STAT THAT DOES NOT SAY "NOT EXIST" IS NEVER READ AS GONE, for the same
// reason an unreadable lock is never read as free: "could not tell" is not the
// answer this step needs, and closing a workspace on it would tear down the
// editor state of a workspace that is merely unreachable this instant.
func (s *sequence) closeMissingDirs(ctx context.Context, log dlog.Logger, workspaces []wsm.Workspace, report *Report) error {
	for i := range workspaces {
		ws := &workspaces[i]
		if ws.Closed {
			continue
		}
		_, statErr := os.Stat(ws.Dir)
		if statErr == nil {
			continue
		}
		// THE IDENTIFIERS GO IN THEIR OWN KEYS, which dlog promotes to
		// top-level record fields (internal/dlog/record.go reservedKeys), so a
		// reader joins on them rather than digging in context.
		context := dlog.Context{
			dlog.KeyWorkspaceID:  string(ws.ID),
			dlog.KeyWorkspaceDir: ws.Dir,
			"error":              statErr.Error(),
		}
		if !errors.Is(statErr, fs.ErrNotExist) {
			log.Warn("daemon.boot.close_missing_dir", "the workspace directory could not be stat-ed; never read as gone", context)
			continue
		}
		if err := s.deps.DB.SetClosed(ctx, ws.ID, true); err != nil {
			context["error"] = err.Error()
			log.Error("daemon.boot.close_missing_dir", "a workspace whose directory is gone could not be closed", context)
			return fmt.Errorf("boot: close the missing-directory workspace %s: %w", ws.ID, err)
		}
		// The LOCAL row is closed with the durable one, so every step below
		// this reads the workspace as the closed row it now is.
		ws.Closed = true
		// THE OWNER RULED THIS AUTOMATIC (2026-09-11): a workspace whose
		// directory no longer exists is closed, no question asked. A ruled
		// automatic action that succeeded states what it did; the two arms
		// above it -- a stat that failed for any other reason, and a close
		// that could not be written -- keep their WARN and their ERROR.
		log.Info("daemon.boot.close_missing_dir", "the workspace directory is gone; the workspace is closed", context)
		report.MissingDirClosed = append(report.MissingDirClosed, ws.ID)
	}
	return nil
}

// survivor is one workspace whose shim this boot must dial before it can say
// what the workspace is. The dial's outcome is filled in by the concurrent
// pass and read by the sequential one that follows it.
type survivor struct {
	ws         wsm.Workspace
	socketPath string
	// inert marks a survivor reached through its LISTENING SOCKET while its
	// workspace lock read FREE. The shim takes that lock at StartSession and
	// not at process start (agent-shim/claude/shim/src/engine/session.ts: "it
	// lands HERE rather than at process start because an inert shim owns no
	// conversation and must not exclude the live one it will replace"), so a
	// free lock behind a live listener is the shim's own statement that NO
	// SESSION HAS BEEN STARTED ON IT. The process is adopted either way —
	// spawning over a listener is what loses turns — but it carries no session,
	// so the bounce accounting has nothing to judge for it.
	inert   bool
	client  shimclient.Client
	err     error
	overran bool
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
		//
		// IT IS NOT AN ANOMALY, AND IT IS NOT A WARNING. The lock is taken at
		// StartSession, so free-and-listening is precisely what an INERT shim
		// looks like: one that was spawned or prelaunched and never had a
		// session started on it. It is adopted as the process it is and
		// carried as INERT, so that neither the watches nor the bounce
		// accounting treats it as a session that survived.
		inert := false
		if state == sessionlock.StateFree && socket == shimsocket.StateLive {
			log.Info("daemon.boot.adopt", "a shim is listening with no session of its own; adopting the inert survivor",
				dlog.Context{
					"workspace_id": string(ws.ID),
					"socket_path":  socketPath,
					"lock_state":   state.String(),
				})
			state = sessionlock.StateHeld
			inert = true
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
		// A PREDECESSOR'S SHIM MAY BE STILL STARTING. A free lock and a socket
		// nothing is listening on are also what a shim spawned moments ago
		// looks like -- it takes its lock inside StartSession and Node binds
		// its socket tens of milliseconds after the fork -- and the registry's
		// recorded spawn pid is the only witness that tells the two apart. It
		// is consulted BEFORE the free branch clears the socket path and
		// declares the workspace client-less. See startingshim.
		if state == sessionlock.StateFree {
			announced, path, undetermined := s.awaitStartingSurvivor(ctx, log, ws, socketPath)
			switch {
			case undetermined:
				report.Undetermined = append(report.Undetermined, ws.ID)
				continue
			case announced:
				socketPath, state, inert = path, sessionlock.StateHeld, true
			}
		}
		switch {
		case state == sessionlock.StateHeld:
			survivors = append(survivors, &survivor{ws: ws, socketPath: socketPath, inert: inert})
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
			report.UnadoptedSessions = append(report.UnadoptedSessions, rollout.UnadoptedSession{Workspace: ws.ID, Lock: state})
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
			// AN INERT SURVIVOR CARRIES NO SESSION (its lock read free), so
			// only a lock-held one is a session nobody adopted.
			if !sv.inert {
				report.UnadoptedSessions = append(report.UnadoptedSessions, rollout.UnadoptedSession{Workspace: sv.ws.ID, Lock: sessionlock.StateHeld})
			}
			continue
		}
		if sv.err != nil {
			// A CANCELLED ADOPTION IS THE BOOT BEING ABANDONED, not a shim
			// that refused: the process is going away under the reconciliation
			// and there is nothing to remediate about a survivor nobody will
			// serve. The boot still fails -- the caller decides what an
			// abandoned boot means -- but it does not report a defect.
			if errors.Is(sv.err, context.Canceled) || errors.Is(sv.err, context.DeadlineExceeded) {
				log.Info("daemon.boot.adopt", "the adoption ended when the boot's context was cancelled", dlog.Context{
					"workspace_id": string(sv.ws.ID),
					"error":        sv.err.Error(),
				})
				return nil, fmt.Errorf("boot: adopt the surviving shim of %s: %w", sv.ws.ID, sv.err)
			}
			log.Error("daemon.boot.adopt", "a surviving shim could not be adopted", dlog.Context{
				"workspace_id": string(sv.ws.ID),
				"error":        sv.err.Error(),
			})
			return nil, fmt.Errorf("boot: adopt the surviving shim of %s: %w", sv.ws.ID, sv.err)
		}
		// A NIL CLIENT WITH A NIL ERROR IS A SUPERVISOR CONTRACT VIOLATION,
		// and it is caught here rather than dereferenced: the pid read below
		// is the bounce accounting's only statement of WHICH process survived,
		// and a boot that panicked on it would take the daemon down.
		if sv.client == nil {
			log.Error("daemon.boot.adopt", "the supervisor answered an adoption with no client and no error", dlog.Context{
				"workspace_id": string(sv.ws.ID),
				"socket_path":  sv.socketPath,
			})
			return nil, fmt.Errorf("boot: adopt the surviving shim of %s: the supervisor answered no client", sv.ws.ID)
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
			"inert":        sv.inert,
		})
		report.Adopted = append(report.Adopted, sv.ws.ID)
		if sv.inert {
			report.AdoptedInert = append(report.AdoptedInert, sv.ws.ID)
			continue
		}
		report.AdoptedSessions = append(report.AdoptedSessions, rollout.AdoptedSession{
			Workspace: sv.ws.ID,
			ShimPID:   sv.client.PID(),
		})
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
	dispositions, err := s.deps.Rollout.Reconcile(ctx, rollout.Survivors{
		Adopted:   report.AdoptedSessions,
		Unadopted: report.UnadoptedSessions,
	})
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

// adoptedAttachedHealthy fires the healthy-attach recovery edge for every
// workspace this boot adopted. An adoption IS a healthy attach. The reconcile
// never records a bounce_unknown for an adopted workspace (its adoption
// accounts for it), so what this closes is one an EARLIER boot left standing
// -- the survivor it could not adopt, adopted now -- which would otherwise
// carry a `bounce_unknown` line until its next bring-up (one stood for over
// 30 minutes on 2026-09-27). It runs after the reconcile so that nothing the
// reconcile writes can outlive it. A fault the edge cannot close is ERROR in
// health.CloseOnEdge and stands; the boot goes on, because a standing fault
// is a line on a strip, not a workspace that cannot be served.
func (s *sequence) adoptedAttachedHealthy(ctx context.Context, log dlog.Logger, report *Report) {
	for _, ws := range report.Adopted {
		workspace := ws
		health.CloseOnEdge(ctx, s.deps.DB, log.With(dlog.Context{"workspace_id": string(ws)}),
			health.EdgeHealthyAttach, health.EdgeScope{Workspace: &workspace}, s.now())
	}
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
		orphans, err := s.deps.Queue.CloseOrphans(ctx, ws.ID, at)
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

// releaseOrphanLeases releases every lease a previous process left behind.
//
// NO LEASE OUTLIVES ITS OWNER. A lease is owned by the process whose state
// handle took it, and that handle's orderly close releases it (wsm Close).
// This boot is an INCUMBENT'S -- it holds the boot claim, which the kernel
// releases only when the previous incumbent's process ends, and a joining
// successor never reaches this step -- so a lease its own handle did not
// take has no living owner. Left standing it is read as a live hold for
// ever: after the 2026-09-27 handover whose successor died at boot, five
// workspaces kept the handover's quiesce leases through a later fresh boot,
// the composer drew `restarting` from them, and Emacs refused every prompt.
//
// IT RUNS AFTER THE MERGE RECOVERY, which reads and releases the merge
// leases a crash left (and re-admits each merge under a lease of its own,
// which this handle owns and so is not foreign). Whatever is still foreign
// after it is an orphan of any holder.
//
// EACH RELEASE IS AN INVARIANT VIOLATION REPAIRED, stated once at ERROR with
// the lease, its holder and its workspace, and the prompt queue is told so the
// intake the lease held drains. A release that fails fails the boot: a daemon
// that cannot clear a hold nobody owns would serve a workspace that refuses
// every prompt.
func (s *sequence) releaseOrphanLeases(ctx context.Context, log dlog.Logger, report *Report) error {
	const op = "daemon.boot.orphan_leases"
	foreign, err := s.deps.DB.ForeignLeases(ctx)
	if err != nil {
		log.Error(op, "the leases a previous process may have left could not be read", dlog.Context{
			"error": err.Error(),
		})
		return fmt.Errorf("boot: read the leases a previous process left: %w", err)
	}
	for _, lease := range foreign {
		fields := dlog.Context{
			"workspace_id": string(lease.Workspace),
			"lease":        string(lease.ID),
			"holder":       lease.Holder.String(),
			"policy":       lease.Policy.String(),
			"acquired_at":  lease.AcquiredAt,
		}
		if err := s.deps.DB.ReleaseLease(ctx, lease.ID); err != nil {
			fields["error"] = err.Error()
			log.Error(op, "a lease whose owning process is gone could not be released", fields)
			return fmt.Errorf("boot: release the orphaned lease %s of %s: %w", lease.ID, lease.Workspace, err)
		}
		log.Error(op, "released a lease that outlived the process that took it; that process died without releasing it", fields)
		s.deps.Queue.OnLeaseChanged(lease.Workspace)
		report.OrphanLeases = append(report.OrphanLeases, lease)
	}
	log.Debug(op, "no lease is left without a living owner", dlog.Context{"released": len(foreign)})
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

// BringUp starts the session of every open, client-less workspace the
// reconciliation named, EXCEPT the hibernated ones.
//
// IT RUNS AFTER THE DAEMON ANSWERS, NOT INSIDE THE RECONCILIATION. The boot
// claim and daemon.addr are published before the reconciliation and nothing is
// accepted until after it, so a step that spawns N shims here is N spawns in
// front of the first `DaemonHealth` — the unary Emacs recognizes a replacement
// daemon by, under a bound of 3s that it does not lengthen. On 2026-09-13 the
// deploy reported `replacement identity was not observed` against a daemon
// that had come up and started both its sessions. So the caller runs this on
// its own goroutine once the listener is accepting, and readiness never waits
// on a shim spawn.
//
// AN OPEN WORKSPACE IS NEVER SESSION-LESS (owner ruling, 2026-09-13). A
// daemon that outlived its shims came back with rows the user had left open
// and nothing behind them: the tab was there, the topbar was blank, and the
// session only appeared once something submitted a prompt. The start goes
// through the same path OpenWorkspace takes, so a boot-started session and a
// user-started one are the same session in every respect, faults included.
//
// THE SET IS THE CLIENT-LESS ONE, WHICH IS NARROWER THAN "NOT ADOPTED".
// `adopt` answers three sets, not two: adopted, client-less, and
// UNDETERMINED — the workspaces whose lock or socket probe could not tell.
// "Could not tell" is never read as free anywhere in this sequence, and
// spawning a second shim onto a conversation a survivor may still own is the
// exact loss that discipline exists to prevent. So an undetermined workspace
// is left alone here too, as it is by the orphan close beside it.
//
// A HIBERNATED WORKSPACE IS LEFT ASLEEP. Hibernation is the memory knob: the
// sweep spent a stand-down to reclaim ~500MB from a workspace nobody had
// touched for six hours, and a boot that woke it would spend it straight back
// for no one. Its topbar says so (the hibernated view), and opening or
// switching to it revives it.
//
// EVERY FAILURE IS PER WORKSPACE. One workspace's start failing does not stop
// the next, and it does not fail the boot: `Fleet.Start` already raises the
// workspace's own start-failed fault and states the dead link on every
// surface, which is the same evidence a failed open leaves. The boot's own
// summary counts it.
//
// THE STARTS RUN CONCURRENTLY. Each one is a shim spawn plus a vendor child
// that has to prove itself live, and nothing one workspace's start does waits
// on another's: `Fleet.Start` serializes per workspace (its start gate), not
// across them. Run one after another, N workspaces paid N starts end to end
// before the last one's feed could draw. MEASURED: the boot of 2026-09-27
// 20:52:08 (pid 2495) took 64.9s to bring four sessions up, 11.1s to 22.4s
// each, back to back. The report still lists every workspace in the order it
// was pending, so its slices do not depend on which start finished first.
func (s *sequence) BringUp(ctx context.Context, pending []wsm.Workspace) BringUpReport {
	log := s.deps.Log.Global()
	outcomes := make([]bringUpOutcome, len(pending))
	var wg sync.WaitGroup
	for i, ws := range pending {
		wg.Add(1)
		go func() {
			defer wg.Done()
			outcomes[i] = s.bringUpOne(ctx, ws)
		}()
	}
	wg.Wait()
	report := BringUpReport{}
	for i, ws := range pending {
		switch outcomes[i] {
		case bringUpStarted:
			report.BroughtUp = append(report.BroughtUp, ws.ID)
		case bringUpHibernated:
			report.HibernatedLeft = append(report.HibernatedLeft, ws.ID)
		case bringUpStoodDown:
			report.StoodDown = append(report.StoodDown, ws.ID)
		case bringUpFailed:
			report.BringUpFailed = append(report.BringUpFailed, ws.ID)
		case bringUpNotBegun:
		}
	}
	// ONE SUMMARY, AT INFO. The per-workspace records are DEBUG because they
	// are a loop body; what a person asks about after a bounce is how many
	// sessions this daemon brought back, so that count is stated once at the
	// level a person reads.
	log.Info("daemon.boot.bring_up", "the boot brought the open workspaces' sessions up", dlog.Context{
		"pending":         len(pending),
		"started":         len(report.BroughtUp),
		"hibernated_left": len(report.HibernatedLeft),
		"stood_down":      len(report.StoodDown),
		"failed":          len(report.BringUpFailed),
	})
	return report
}

// bringUpOutcome is what one workspace's bring-up came to.
type bringUpOutcome int

const (
	// bringUpNotBegun is a workspace whose start was never begun because
	// the daemon was already leaving.
	bringUpNotBegun bringUpOutcome = iota
	bringUpStarted
	bringUpHibernated
	bringUpStoodDown
	bringUpFailed
)

// bringUpOne brings one pending workspace's session up and says what came of
// it, logging the per-workspace record BringUp's summary counts.
func (s *sequence) bringUpOne(ctx context.Context, ws wsm.Workspace) bringUpOutcome {
	log := s.deps.Log.Global()
	// A START THAT HAS BEGUN IS FINISHED, NEVER ABANDONED MID-WRITE, and a
	// start not yet begun is simply not begun once the daemon is leaving. The
	// step runs beside the accept loop, so an exit CAN land in the middle of
	// it, and a start cancelled halfway is not a session that failed: it is a
	// half-written session record, a shim stopped between spawn and attach,
	// and a fault the daemon then could not record because the same
	// cancellation refused its transaction. The exit joins this goroutine
	// (cmd/claude-repld/run.go, loopJoinBound) rather than cutting it.
	if err := ctx.Err(); err != nil {
		log.Info("daemon.boot.bring_up", "the daemon is leaving; this workspace is not started", dlog.Context{
			dlog.KeyWorkspaceID: string(ws.ID),
			"error":             err.Error(),
		})
		return bringUpNotBegun
	}
	startCtx := context.WithoutCancel(ctx)
	fields := dlog.Context{
		dlog.KeyWorkspaceID:  string(ws.ID),
		dlog.KeyWorkspaceDir: ws.Dir,
	}
	session, exists, err := s.deps.DB.Session(startCtx, ws.ID)
	if err != nil {
		fields["error"] = err.Error()
		log.Error("daemon.boot.bring_up", "a workspace's session record could not be read; it is not brought up", fields)
		return bringUpFailed
	}
	if exists && session.Hibernated() {
		log.Debug("daemon.boot.bring_up", "a hibernated workspace is left asleep", fields)
		return bringUpHibernated
	}
	if err := s.deps.StartSession(startCtx, ws.ID); err != nil {
		fields["error"] = err.Error()
		// A START THIS DAEMON STOOD THE SHIM DOWN UNDER IS NOT A FAILED
		// BRING-UP. The exit's drain force-stops every workspace session,
		// and a start still in flight when it does comes back
		// `unavailable: unexpected EOF` from a shim the same process just
		// killed. It is the ordinary shape of an exit landing inside the
		// bring-up, and it is counted apart from the workspaces that genuinely
		// would not start, so the summary's `failed` still means what it
		// says. MEASURED: realtest run 2026-09-13T16:20:34 recorded it as
		// an ERROR on three consecutive daemon generations.
		if errors.Is(err, shimclient.ErrStandDownOrdered) {
			log.Debug("daemon.boot.bring_up", "an open workspace's start ended in a stand-down this daemon ordered", fields)
			return bringUpStoodDown
		}
		log.Error("daemon.boot.bring_up", "an open workspace's session did not come up; the boot goes on", fields)
		return bringUpFailed
	}
	log.Debug("daemon.boot.bring_up", "an open workspace's session was started", fields)
	return bringUpStarted
}

// awaitStartingSurvivor answers whether a shim a PREVIOUS daemon spawned for
// this workspace is merely still starting, waiting out the adoption bound for
// it to bind its socket. It reports whether one announced itself, the path it
// announced on, and whether the workspace must be left UNDETERMINED.
//
// It is called on exactly one state: the workspace lock reads FREE and no
// generation of the socket is live. That is what a client-less workspace looks
// like, and it is also what a shim spawned tens of milliseconds ago looks
// like — startingshim carries the measured timeline. The registry's recorded
// spawn pid, written at the instant of the fork, is what separates them.
//
// AN EXPIRED BOUND IS UNDETERMINED, exactly as an unreadable lock is: a live
// process that may bind the path at any instant is what a second shim must not
// race. Such a workspace is neither adopted nor orphan-closed, no shim is
// spawned onto it, the boot goes on and the daemon serves.
func (s *sequence) awaitStartingSurvivor(ctx context.Context, log dlog.Logger, ws wsm.Workspace, socketPath string) (bool, string, bool) {
	if ws.SpawnedShimPID == nil {
		return false, "", false
	}
	pid := *ws.SpawnedShimPID
	if !s.shimAlive(pid) {
		log.Debug("daemon.boot.adopt", "the recorded spawn's process is gone; nothing survives to wait for", dlog.Context{
			"workspace_id": string(ws.ID), "shim_pid": pid,
		})
		return false, "", false
	}
	log.Info("daemon.boot.adopt", "a shim spawned for this workspace is still starting; waiting for it to announce itself rather than declaring the workspace client-less",
		dlog.Context{
			"workspace_id": string(ws.ID), "shim_pid": pid,
			"socket_path": socketPath, "bound_ms": s.adoptBound.Milliseconds(),
		})
	path, outcome := s.starting.Await(ctx, &pid, socketPath, s.adoptBound)
	switch outcome {
	case startingshim.OutcomeAnnounced:
		log.Info("daemon.boot.adopt", "the starting shim announced itself; adopting it as the inert survivor it is", dlog.Context{
			"workspace_id": string(ws.ID), "shim_pid": pid, "socket_path": path,
		})
		return true, path, false
	case startingshim.OutcomeSpawnDead:
		log.Info("daemon.boot.adopt", "the starting shim died before it announced itself; the workspace is client-less", dlog.Context{
			"workspace_id": string(ws.ID), "shim_pid": pid, "socket_path": socketPath,
		})
		return false, "", false
	default:
		log.Error("daemon.boot.adopt", "a shim recorded as spawned is alive but never announced itself within the adoption bound; the workspace is left undetermined and the daemon serves",
			dlog.Context{
				"workspace_id": string(ws.ID), "shim_pid": pid,
				"socket_path": socketPath, "bound_ms": s.adoptBound.Milliseconds(),
			})
		return false, "", true
	}
}

// shimAlive answers the injected liveness probe, or the kernel's.
func (s *sequence) shimAlive(pid int) bool {
	if s.deps.ShimAlive != nil {
		return s.deps.ShimAlive(pid)
	}
	return startingshim.Alive(pid)
}
