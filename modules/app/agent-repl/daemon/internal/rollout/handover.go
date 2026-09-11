package rollout

import (
	"context"
	"fmt"
	"os"
	"sync"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// FaultAdoptionExpired is the fault kind an expired adoption window records.
const FaultAdoptionExpired = "adoption_window_expired"

// adoptionPoll is how often the outgoing daemon looks for serving ownership
// having moved. It only ever shortens the adoption window.
const adoptionPoll = 25 * time.Millisecond

// Handover is the outgoing daemon's whole blue-green flow.
//
// SPAWN, then ANNOUNCE, then a PER-WORKSPACE TRANSFER at freeness, then EXIT.
// The order is the product spec's and is load-bearing at every step: the
// address must exist before it is announced; the participant snapshot must be
// taken at the announcement, because that is what "the holders at announcement"
// means; and a workspace is quiesced BEFORE it is detached, so nothing this
// daemon is still doing races the successor's adoption.
func (c *controller) Handover(ctx context.Context) error {
	successor, err := c.deps.Spawner.Spawn(ctx, c.deps.SelfAddress)
	if err != nil {
		c.log.Error(opHandover, "the successor did not come up; nothing was announced",
			withCause(dlog.Context{"self_address": c.deps.SelfAddress}, err))
		return fmt.Errorf("rollout: handover: spawn the successor: %w", err)
	}
	fields := dlog.Context{"successor": successor, "self_address": c.deps.SelfAddress}
	c.log.Info(opHandover, "the successor is up in joining mode", fields)

	workspaces, untransferable, err := c.served(ctx)
	if err != nil {
		return err
	}

	c.deps.Announcer.ShutdownAnnounced(&agentreplv1.DaemonShutdownAnnounced{
		Address: &successor,
		Cause: &agentreplv1.DaemonShutdownCause{
			Kind: &agentreplv1.DaemonShutdownCause_SelfMergeRollout{
				SelfMergeRollout: &agentreplv1.DaemonShutdownSelfMergeRollout{},
			},
		},
		ExpectedOutageMs: int64(c.deps.ExpectedOutage / 1e6),
		MintedAtMs:       milliseconds(c.deps.Clock.Now()),
	})

	// THE SNAPSHOT IS TAKEN HERE, at the announcement, for every workspace at
	// once — not per workspace as each transfer comes round, by which time a
	// client that acted on the announcement would already have moved.
	snapshot := make(map[ids.WorkspaceID]Participants, len(workspaces))
	c.mu.Lock()
	for _, ws := range workspaces {
		p := c.deps.Participants.Participants(ws.ID)
		snapshot[ws.ID] = p
		c.rendezvous[ws.ID] = &entry{expected: p, done: make(chan struct{})}
	}
	c.mu.Unlock()
	for _, ws := range workspaces {
		p := snapshot[ws.ID]
		c.logTransition(opHandover, ws.ID, "rendezvous", "unarmed", "armed", dlog.Context{
			"expected_host": p.Host,
			"expected_web":  p.Web,
		})
	}
	c.log.Info(opHandover, "announced the stand-down and snapshotted the expected participants",
		merge(fields, dlog.Context{"workspaces": len(workspaces)}))

	if err := c.writeManifest(ctx, c.manifest(ctx, successor, workspaces, snapshot)); err != nil {
		return err
	}

	var windows sync.WaitGroup
	for _, ws := range workspaces {
		if err := c.transfer(ctx, ws, successor, snapshot[ws.ID], &windows); err != nil {
			return err
		}
	}

	// THE OUTGOING DAEMON TIMES THE WINDOW, so it must still be alive when the
	// window closes: letting the windows settle before the exit is what makes
	// the accountability record get written at all. The exit is therefore
	// delayed by at most one AdoptionWindow past the last transfer — a bounded
	// stand-down, paid while the successor is already serving every workspace.
	windows.Wait()

	// NOTHING IS LEFT STANDING. Every workspace this daemon served has either
	// moved to the successor above or is stood down here; the exit below owns
	// no process either way.
	c.standDownTheUntransferred(ctx, untransferable)

	c.log.Info(opHandover, "every workspace is transferred; exiting", fields)
	if err := c.deps.Exit(ctx); err != nil {
		c.log.Error(opHandover, "the orderly exit could not be started", withCause(fields, err))
		return fmt.Errorf("rollout: handover: %w", err)
	}
	return nil
}

// transfer moves one workspace to the successor.
func (c *controller) transfer(ctx context.Context, ws wsm.Workspace, successor string, expected Participants, windows *sync.WaitGroup) error {
	fields := dlog.Context{"workspace": string(ws.ID), "successor": successor}

	if err := c.awaitFreeForever(ctx, ws.ID, opTransfer, fields); err != nil {
		return err
	}

	// QUIESCE FIRST. From here on this daemon does NO work for the workspace:
	// every arrival is held rather than served, so nothing the successor is
	// about to own is still moving under it.
	if err := c.deps.Quiesce(ctx, ws.ID); err != nil {
		c.log.Error(opTransfer, "could not quiesce the workspace's intake", withCause(fields, err))
		return fmt.Errorf("rollout: transfer %q: quiesce: %w", ws.ID, err)
	}
	c.log.Debug(opTransfer, "quiesced the workspace's intake", fields)

	// DETACH, NEVER KILL. The shim keeps running and KEEPS its kernel lock
	// through the whole handover; the lock is the shim's, and the successor
	// dials the still-locked process.
	if client, ok := c.deps.Shims.Client(ws.ID); ok {
		client.Detach()
		c.log.Debug(opTransfer, "detached from the workspace's shim, leaving it running", fields)
	} else {
		c.log.Debug(opTransfer, "the workspace has no live shim to detach from", fields)
	}

	if err := c.releaseServing(ctx, ws.ID, fields); err != nil {
		return fmt.Errorf("rollout: transfer %q: release serving: %w", ws.ID, err)
	}

	c.recordTransfer(ws.ID, successor)
	c.deps.Pusher.PushTransferred(ws.ID, successor)
	c.log.Info(opTransfer, "pushed the transfer notice on the workspace's host and web streams",
		merge(fields, dlog.Context{"expected_host": expected.Host, "expected_web": expected.Web}))

	windows.Add(1)
	go func() {
		defer windows.Done()
		c.timeAdoption(ctx, ws.ID, fields)
	}()
	return nil
}

// releaseServing clears this daemon's serving claim. The successor waits for
// this release before beginning adoption, so the normal path owns and clears
// the row here. The already-released and already-moved arms preserve a durable
// completed edge if an incumbent resumes after losing an operation's answer.
func (c *controller) releaseServing(ctx context.Context, ws ids.WorkspaceID, fields dlog.Context) error {
	owner, err := c.deps.DB.Serving(ctx, ws)
	if err != nil {
		c.log.Error(opTransfer, "could not read serving ownership before releasing it", withCause(fields, err))
		return err
	}
	if owner == nil {
		c.log.Debug(opTransfer, "serving ownership was already released", fields)
		return nil
	}
	if *owner != c.deps.Instance {
		c.log.Info(opTransfer, "the successor already owns the workspace", merge(fields, dlog.Context{
			"owner": string(*owner),
		}))
		return nil
	}
	if err := c.deps.DB.ReleaseServing(ctx, ws, c.deps.Instance); err != nil {
		c.log.Error(opTransfer, "could not release serving ownership", withCause(fields, err))
		return err
	}
	c.log.Debug(opTransfer, "released serving ownership", fields)
	return nil
}

// timeAdoption gives the successor its window and records the workspace's OWN
// fault when it expires. There is deliberately no abort and no retry
// machinery: expiry is remediated as it comes up.
func (c *controller) timeAdoption(ctx context.Context, ws ids.WorkspaceID, fields dlog.Context) {
	// An adoption that already landed needs no window: the headless case claims
	// serving inside Join, before the push it is answering was even read.
	if c.adopted(ctx, ws, fields) {
		return
	}
	// THE WINDOW IS A DEADLINE, NOT A DELAY. The rendezvous closes the moment
	// every expected participant has adopted, and waiting on it is what lets
	// the outgoing daemon exit as soon as the successor is serving. Slept out
	// instead, the exit is held for the whole window after the last transfer
	// even when the adoption landed immediately — precisely the outage the
	// handover exists to bound.
	//
	// Two things end the wait early: the rendezvous closing (the participants
	// adopted through this daemon's own handlers) and serving ownership moving
	// to the successor (the headless case, claimed inside the successor's
	// Join, which this daemon can only observe by looking). The look is a real
	// ticker rather than the injected clock: the clock times the WINDOW, which
	// a test drives; the look only ever shortens the wait.
	expired := c.deps.Clock.After(c.deps.AdoptionWindow)
	done := c.rendezvousDone(ws)
	look := time.NewTicker(adoptionPoll)
	defer look.Stop()
	for waiting := true; waiting; {
		select {
		case <-ctx.Done():
			c.log.Debug(opAdoption, "the adoption window ended with its context", fields)
			return
		case <-done:
			c.log.Debug(opAdoption, "the rendezvous completed inside its window", fields)
			return
		case <-look.C:
			if c.adopted(ctx, ws, fields) {
				return
			}
		case <-expired:
			waiting = false
		}
	}
	if c.adopted(ctx, ws, fields) {
		return
	}
	workspace := ws
	if _, err := c.deps.DB.OpenFault(ctx, wsm.Fault{
		Workspace: &workspace,
		Kind:      FaultAdoptionExpired,
		Detail: fmt.Sprintf("the successor did not claim this workspace within the %s adoption window",
			c.deps.AdoptionWindow),
		Evidence: map[string]string{"adoption_window": c.deps.AdoptionWindow.String()},
		OpenedAt: c.deps.Clock.Now(),
	}); err != nil {
		c.log.Error(opAdoption, "could not record the expired adoption window", withCause(fields, err))
		return
	}
	c.log.Warn(opAdoption, "the adoption window expired; recorded the workspace's own fault",
		merge(fields, dlog.Context{"adoption_window": c.deps.AdoptionWindow.String()}))
}

// rendezvousDone answers the channel that closes when this workspace's
// adoption completes, or a nil channel (which blocks forever) when no
// rendezvous is armed for it.
func (c *controller) rendezvousDone(ws ids.WorkspaceID) <-chan struct{} {
	c.mu.Lock()
	defer c.mu.Unlock()
	if e, ok := c.rendezvous[ws]; ok {
		return e.done
	}
	return nil
}

// adopted reports whether some OTHER daemon instance now serves the workspace,
// which is the only evidence of a completed adoption the outgoing daemon has:
// there is no daemon-to-daemon channel, so serving ownership in WSM is the
// whole of the signal.
func (c *controller) adopted(ctx context.Context, ws ids.WorkspaceID, fields dlog.Context) bool {
	owner, err := c.deps.DB.Serving(ctx, ws)
	if err != nil {
		c.log.Error(opAdoption, "could not read serving ownership", withCause(fields, err))
		return false
	}
	if owner == nil || *owner == c.deps.Instance {
		return false
	}
	c.log.Debug(opAdoption, "the successor owns the workspace",
		merge(fields, dlog.Context{"owner": string(*owner)}))
	return true
}

// awaitFreeForever waits for a workspace to fall free, FOREVER, naming the
// holdout every HoldoutWarnEvery. A never-free workspace leaves the rollout in
// a two-daemon steady state, which is the ruled outcome: the wait never gives
// up and nothing is ever interrupted to end it.
func (c *controller) awaitFreeForever(ctx context.Context, ws ids.WorkspaceID, operation string, fields dlog.Context) error {
	if c.deps.Freeness.Free(ws) {
		c.log.Debug(operation, "the workspace was already free", fields)
		return nil
	}
	done := make(chan error, 1)
	waitCtx, cancel := context.WithCancel(ctx)
	defer cancel()
	go func() { done <- c.deps.Freeness.AwaitFree(waitCtx, ws) }()

	for waited := 0; ; waited++ {
		select {
		case err := <-done:
			if err != nil {
				c.log.Error(operation, "the freeness wait ended before the workspace fell free",
					withCause(fields, err))
				return fmt.Errorf("rollout: wait for %q: %w", ws, err)
			}
			c.log.Debug(operation, "the workspace fell free", fields)
			return nil
		case <-c.deps.Clock.After(c.deps.HoldoutWarnEvery):
			c.log.Warn(operation, "still waiting for a workspace to fall free; nothing will be interrupted to hurry it",
				merge(fields, dlog.Context{
					"warnings": waited + 1,
					"cadence":  c.deps.HoldoutWarnEvery.String(),
				}))
		}
	}
}

// served lists the workspaces this daemon currently serves, SPLIT IN TWO:
// the ones a handover moves, and the ones it cannot. Serving ownership is the
// WSM fact that arbitrates the overlap, so it — not "has a shim" — is what
// decides whether a workspace is this daemon's at all.
//
// THE SECOND LIST IS NOT A LEFTOVER, IT IS AN OBLIGATION. Every workspace this
// daemon serves leaves the handover one of exactly two ways: transferred to
// the successor, or stood down. There is no third way, because this daemon is
// about to exit and nothing else knows the shim exists.
func (c *controller) served(ctx context.Context) (transfer, untransferable []wsm.Workspace, err error) {
	all, err := c.deps.DB.ListWorkspaces(ctx)
	if err != nil {
		c.log.Error(opHandover, "could not list the workspaces to hand over", withCause(nil, err))
		return nil, nil, fmt.Errorf("rollout: handover: %w", err)
	}
	out := make([]wsm.Workspace, 0, len(all))
	var left []wsm.Workspace
	for _, ws := range all {
		owner, err := c.deps.DB.Serving(ctx, ws.ID)
		if err != nil {
			c.log.Error(opHandover, "could not read a workspace's serving ownership",
				withCause(dlog.Context{"workspace": string(ws.ID)}, err))
			return nil, nil, fmt.Errorf("rollout: handover: %w", err)
		}
		if owner == nil || *owner != c.deps.Instance {
			continue
		}
		// A WORKSPACE WHOSE WORKTREE IS GONE HAS NOTHING TO HAND OVER. A merged
		// workspace's worktree is removed at the merge's terminal while its
		// registry row survives, and the successor cannot even resolve a log
		// sink for a directory that is not there — so it fails that adoption,
		// and the incumbent then waits out a whole adoption window for a
		// workspace nobody can serve.
		//
		// ITS SHIM IS STILL RUNNING, THOUGH, and that is why this is a second
		// list rather than a `continue`. The merge does not stop the session it
		// merged, so a self-merge rollout — merge a workspace, the daemon
		// reloads itself — used to exit with that shim standing: not
		// transferred, so not detached and nothing to adopt; not swept, because
		// a handover deliberately never runs the immediate shutdown's sweep.
		// It held both kernel locks and ~95 MiB for as long as the machine
		// stayed up, and the next daemon read its locks as a live session on a
		// worktree that no longer exists.
		if _, statErr := os.Stat(ws.Dir); statErr != nil {
			c.log.Info(opHandover, "the workspace's worktree is gone; it is not handed over",
				dlog.Context{"workspace": string(ws.ID), "dir": ws.Dir, "cause": statErr.Error()})
			left = append(left, ws)
			continue
		}
		out = append(out, ws)
	}
	return out, left, nil
}

// standDownTheUntransferred stops the shim of every workspace this daemon
// serves and is NOT handing over, so the exit below leaves nothing behind.
//
// IT RUNS AT THE EXIT, not at the split, because until the transfers are done
// this daemon is still the one serving everything and a stand-down is time
// spent inside the announced outage. By here every transfer has landed and the
// only thing left to do is leave cleanly.
//
// IT GOES THROUGH THE FLEET, never through the client directly. The fleet ends
// the SESSION before the process -- the shim writes its own terminals as the
// session ends, and a signal alone gives it no chance to -- and, before either,
// it tells the session watcher. A watcher that has not been told reads this
// daemon's own act as a transport fault: it records a severing at ERROR, marks
// the link degraded, and reopens watches at a shim the next line is about to
// stop.
//
// NOTHING HERE MAY STOP THE EXIT, so each failure is recorded at ERROR and the
// walk continues: a shim that would not go is a leaked process the caller
// cannot do anything about, and this record is the only thing that will ever
// say so, while an exit skipped over one leaks the daemon as well.
func (c *controller) standDownTheUntransferred(ctx context.Context, workspaces []wsm.Workspace) {
	for _, ws := range workspaces {
		fields := dlog.Context{"workspace": string(ws.ID), "dir": ws.Dir}
		if _, live := c.deps.Shims.Client(ws.ID); !live {
			c.log.Debug(opHandover, "the untransferred workspace has no live shim to stand down", fields)
			continue
		}
		if err := c.deps.Shims.StandDown(ctx, ws.ID); err != nil {
			c.log.Error(opHandover, "an untransferred workspace's shim would not stand down; it will outlive this daemon and hold the workspace lock",
				withCause(fields, err))
			continue
		}
		c.log.Info(opHandover, "stood down the shim of a workspace this handover does not transfer", fields)
	}
}

// manifest builds the stand-down record for every workspace being handed over.
// A handover kills nothing, so every workspace with a live shim is
// IntentPreserve; a workspace with NO SHIM PID is IntentNoSession, because
// there is no process whose survival the successor could judge and calling it
// "preserve" makes its free lock read as a session that silently died.
func (c *controller) manifest(ctx context.Context, successor string, workspaces []wsm.Workspace, snapshot map[ids.WorkspaceID]Participants) Manifest {
	m := Manifest{
		Daemon:    c.deps.Instance,
		Successor: successor,
		WrittenAt: c.deps.Clock.Now(),
		Sessions:  make([]ManifestSession, 0, len(workspaces)),
	}
	for _, ws := range workspaces {
		record := ManifestSession{
			Workspace:    ws.ID,
			Dir:          ws.Dir,
			Intent:       IntentNoSession,
			ExpectedHost: snapshot[ws.ID].Host,
			ExpectedWeb:  snapshot[ws.ID].Web,
		}
		session, found, err := c.deps.DB.Session(ctx, ws.ID)
		if err != nil {
			c.log.Error(opManifest, "could not read a session for the intent manifest",
				withCause(dlog.Context{"workspace": string(ws.ID)}, err))
		} else if found {
			record.VendorSessionID = session.VendorSessionID
			if session.ShimPID != nil {
				record.ShimPID = *session.ShimPID
				// THE PID IS WHAT MAKES IT A PRESERVE. A handover leaves a
				// running shim running, and this is the entry whose free lock
				// on the other side genuinely means the session died.
				record.Intent = IntentPreserve
			}
		}
		m.Sessions = append(m.Sessions, record)
	}
	return m
}
