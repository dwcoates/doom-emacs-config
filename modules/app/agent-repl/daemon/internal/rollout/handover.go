package rollout

import (
	"context"
	"errors"
	"fmt"
	"os"
	"sort"
	"sync"
	"sync/atomic"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/bounce"
	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// FaultAdoptionExpired is the fault kind an expired adoption window records.
const FaultAdoptionExpired = "adoption_window_expired"

// adoptionPoll is how often the outgoing daemon looks for serving ownership
// having moved. It only ever shortens the adoption window.
const adoptionPoll = 25 * time.Millisecond

// handoverPlan is what the announcement settled and the transfers run on: the
// successor's address, the two lists `served` split, and the participants each
// workspace had AT THE ANNOUNCEMENT.
type handoverPlan struct {
	successor      string
	workspaces     []wsm.Workspace
	untransferable []wsm.Workspace
	snapshot       map[ids.WorkspaceID]Participants
	forced         bool
	fields         dlog.Context
	// reclaimed counts the workspaces this daemon took BACK after handing
	// them toward the successor: an adoption window that expired, or a
	// transfer that failed after its quiesce. A reclaimed workspace is served
	// here again, so a handover with any cannot finish and does not exit.
	reclaimed atomic.Int32
}

// ErrAlreadyRollingOut refuses a second handover while one is in flight.
//
// A HANDOVER IN FLIGHT IS NEVER STARTED AGAIN. It may stand for as long as a
// workspace stays busy, and its successor was spawned from the build on disk
// when it began; a second handover would spawn a second successor beside the
// first, and the two would race for every workspace this daemon releases.
type ErrAlreadyRollingOut struct {
	// WaitingOn is the workspaces the handover in flight has not transferred.
	WaitingOn []ids.WorkspaceID
}

func (e *ErrAlreadyRollingOut) Error() string {
	return fmt.Sprintf("rollout: a handover is already in flight, waiting on %d workspace(s)", len(e.WaitingOn))
}

// RollingOut implements Controller: whether a handover is in flight, and the
// workspaces it has not transferred yet.
func (c *controller) RollingOut() ([]ids.WorkspaceID, bool) {
	c.mu.Lock()
	defer c.mu.Unlock()
	if c.handover == nil {
		return nil, false
	}
	return c.waitingLocked(), true
}

// waitingLocked lists the workspaces the handover in flight has not
// transferred, sorted. The caller holds c.mu.
func (c *controller) waitingLocked() []ids.WorkspaceID {
	var waiting []ids.WorkspaceID
	for ws := range c.rendezvous {
		if _, moved := c.transferred[ws]; !moved {
			waiting = append(waiting, ws)
		}
	}
	sort.Slice(waiting, func(i, j int) bool { return waiting[i] < waiting[j] })
	return waiting
}

// handoverSlot is the one handover in flight and the successor it spawned.
type handoverSlot struct {
	// successor is nil until the spawn answers a process; from then on the
	// slot owns it.
	successor Successor
	// armed is the rendezvous entries this handover's announcement armed, so
	// an abandoned handover disarms exactly what it armed.
	armed []ids.WorkspaceID
}

// claimHandover claims the successor slot, or refuses naming what the
// handover in flight is still waiting on. The slot is never emptied on
// success: a handover that completes ends in this process's exit.
func (c *controller) claimHandover() (*handoverSlot, error) {
	c.mu.Lock()
	defer c.mu.Unlock()
	if c.handover == nil {
		c.handover = &handoverSlot{}
		return c.handover, nil
	}
	return nil, &ErrAlreadyRollingOut{WaitingOn: c.waitingLocked()}
}

// abandonHandover ends a handover that FAILED before it was accepted: it stops
// the successor the slot holds, disarms the rendezvous the announcement armed,
// and only then empties the slot, so the next attempt is not refused by one
// that will never finish.
//
// THE SLOT IS EMPTIED ONLY ON THE SUCCESSOR'S CONFIRMED DEATH. A successor
// that would not stop is still a joining daemon polling the manifest path;
// emptying the slot then would let the next deploy spawn a second one beside
// it, and the two would race for every workspace. So the slot stays claimed,
// every later handover is refused as already in flight, and the record below
// names the pid-bearing cause for the operator.
func (c *controller) abandonHandover(ctx context.Context, slot *handoverSlot, fields dlog.Context, cause error) {
	if slot.successor != nil {
		if err := slot.successor.Stop(ctx); err != nil {
			c.log.Error(opHandover, "the abandoned handover's successor could not be stopped; the handover stays in flight so no second successor is spawned beside it",
				withCause(merge(fields, dlog.Context{"abandoned_because": cause.Error()}), err))
			return
		}
		c.log.Info(opHandover, "stopped the abandoned handover's successor", fields)
	}
	c.mu.Lock()
	for _, ws := range slot.armed {
		delete(c.rendezvous, ws)
	}
	if c.handover == slot {
		c.handover = nil
	}
	c.mu.Unlock()
	for _, ws := range slot.armed {
		c.logTransition(opHandover, ws, "rendezvous", "armed", "unarmed", dlog.Context{"abandoned": true})
	}
	c.log.Info(opHandover, "the abandoned handover released its slot; the next handover may begin", fields)
}

// beginHandover is everything up to and including the announcement and the
// intent manifest: SPAWN, list what is served, ANNOUNCE, snapshot, record.
// It is bounded — nothing in it waits on a workspace — which is what lets a
// caller answer "the rollout was accepted" before the unbounded part starts.
//
// EVERY FAILURE PAST THE CLAIM ABANDONS THE HANDOVER through one door
// (abandonHandover), which stops whatever successor the spawn started before
// it lets the slot go.
func (c *controller) beginHandover(ctx context.Context, force bool) (*handoverPlan, error) {
	slot, err := c.claimHandover()
	if err != nil {
		// INFO, NOT WARN: the refusal is the contract's own answer to a caller
		// that asked twice, and nothing about it is wrong with this daemon.
		c.log.Info(opHandover, "refused a handover while one is already in flight",
			withCause(dlog.Context{"self_address": c.deps.SelfAddress}, err))
		return nil, err
	}
	fields := dlog.Context{"self_address": c.deps.SelfAddress, "forced": force}
	if err := c.clearStaleManifest(fields); err != nil {
		c.abandonHandover(ctx, slot, fields, err)
		return nil, err
	}
	successor, err := c.deps.Spawner.Spawn(ctx, c.deps.SelfAddress)
	if successor != nil {
		slot.successor = successor
	}
	if err != nil {
		c.log.Error(opHandover, "the successor did not come up; nothing was announced",
			withCause(merge(fields, dlog.Context{"successor_started": successor != nil}), err))
		c.abandonHandover(ctx, slot, fields, err)
		return nil, fmt.Errorf("rollout: handover: spawn the successor: %w", err)
	}
	address := successor.Address()
	fields = merge(fields, dlog.Context{"successor": address, "successor_pid": successor.PID()})
	// NOTHING IS ANNOUNCED, QUIESCED OR TRANSFERRED TO A SUCCESSOR THAT HAS
	// NOT ANSWERED. Its address report only says its listener is bound; the
	// 2026-09-27 successor bound, reported, and died 3ms later on a state
	// layout it could not read, and every workspace handed to it was left
	// held with no daemon serving it. So the handover stops here, before a
	// single workspace is touched, unless the successor answers a health
	// probe -- and a successor that dies first is named with its exit.
	ready, cancelReady := context.WithTimeout(ctx, c.deps.ReadyBound)
	err = successor.Ready(ready)
	cancelReady()
	if err != nil {
		c.log.Error(opHandover, "the successor never proved it was serving; the handover is abandoned before any workspace was quiesced or announced, and this daemon keeps serving",
			withCause(merge(fields, dlog.Context{"ready_bound": c.deps.ReadyBound.String()}), err))
		c.abandonHandover(ctx, slot, fields, err)
		return nil, fmt.Errorf("rollout: handover: the successor never proved it was serving: %w", err)
	}
	c.log.Info(opHandover, "the successor answered its health probe; it is serving in joining mode", fields)

	workspaces, untransferable, err := c.served(ctx)
	if err != nil {
		c.log.Error(opHandover, "could not list what this daemon serves; the handover is abandoned before anything was announced",
			withCause(fields, err))
		c.abandonHandover(ctx, slot, fields, err)
		return nil, err
	}

	c.deps.Announcer.ShutdownAnnounced(&agentreplv1.DaemonShutdownAnnounced{
		Address: &address,
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
		slot.armed = append(slot.armed, ws.ID)
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

	manifest := c.manifest(ctx, address, workspaces, snapshot)
	manifest.Forced = force
	if err := c.writeManifest(ctx, manifest); err != nil {
		// THE ANNOUNCEMENT HAS GONE OUT, and there is no arm that retracts it:
		// a client that dialed the successor sees it stop, and goes on being
		// served here, where nothing was quiesced or released. What must not
		// happen is the successor waiting forever for this manifest.
		c.log.Error(opHandover, "the intent manifest could not be written after the announcement; the handover is abandoned and its successor stopped",
			withCause(merge(fields, dlog.Context{"workspaces": len(workspaces)}), err))
		c.abandonHandover(ctx, slot, fields, err)
		return nil, err
	}
	return &handoverPlan{
		successor:      address,
		workspaces:     workspaces,
		untransferable: untransferable,
		snapshot:       snapshot,
		forced:         force,
		fields:         fields,
	}, nil
}

// completeHandover is the UNBOUNDED half: every workspace's transfer is
// asked of the prompt queue's bounce registry AT ONCE, and each happens at its
// own workspace's freeness — independently, so a busy workspace never delays
// a free one queued behind it. A forced handover transfers them all now.
//
// ONE goroutine follows the handover to its end — the transfers, the adoption
// windows, the stand-down of what cannot be transferred, the exit — and it
// names the workspaces still being waited on every HoldoutWarnEvery. Nothing
// waits per workspace.
func (c *controller) completeHandover(ctx context.Context, plan *handoverPlan) {
	fields := plan.fields
	var windows sync.WaitGroup
	outcomes := make(chan transferOutcome, len(plan.workspaces))
	requested := 0
	for _, ws := range plan.workspaces {
		ws := ws
		req := bounce.Request{
			Reason:       string(ReasonHandoverTransfer),
			Force:        plan.forced,
			KeepDraining: true,
			Run: func(runCtx context.Context, id ids.WorkspaceID) error {
				return c.transfer(runCtx, ws, plan, plan.snapshot[id], &windows)
			},
			Done: func(err error) { outcomes <- transferOutcome{ws: ws.ID, err: err} },
		}
		decision, err := c.deps.Bounces.RequestBounce(ctx, ws.ID, req)
		if err != nil {
			c.log.Error(opTransfer, "the bounce registry refused the workspace's transfer; the handover cannot finish while this daemon still serves it",
				withCause(merge(fields, dlog.Context{"workspace": string(ws.ID)}), err))
			continue
		}
		requested++
		c.log.Info(opTransfer, "asked the bounce registry to transfer the workspace", merge(fields, dlog.Context{
			"workspace":      string(ws.ID),
			"now":            decision.Now,
			"forced":         decision.Forced,
			"turn_in_flight": decision.TurnInFlight,
			"detached_work":  decision.DetachedWork,
		}))
	}
	refused := len(plan.workspaces) - requested
	c.handoverDone.Add(1)
	go func() {
		defer c.handoverDone.Done()
		c.followHandover(ctx, plan, outcomes, requested, refused, &windows)
	}()
}

// transferOutcome is one transfer's end.
type transferOutcome struct {
	ws  ids.WorkspaceID
	err error
}

// followHandover waits for every requested transfer to finish, names the
// holdouts on the cadence, and exits once every workspace has moved.
func (c *controller) followHandover(ctx context.Context, plan *handoverPlan, outcomes <-chan transferOutcome, requested, refused int, windows *sync.WaitGroup) {
	fields := plan.fields
	pending := make(map[ids.WorkspaceID]bool, requested)
	for _, ws := range plan.workspaces {
		pending[ws.ID] = true
	}
	failed := refused
	for warnings := 0; requested > 0; {
		select {
		case <-ctx.Done():
			c.log.Debug(opHandover, "the daemon's lifetime ended while transfers were still pending",
				merge(fields, dlog.Context{"pending": len(pending)}))
			return
		case out := <-outcomes:
			requested--
			delete(pending, out.ws)
			if out.err != nil {
				failed++
				c.log.Error(opTransfer, "a workspace's transfer failed; it stays served by this daemon",
					withCause(merge(fields, dlog.Context{"workspace": string(out.ws)}), out.err))
			}
		case <-c.deps.Clock.After(c.deps.HoldoutWarnEvery):
			warnings++
			holdouts := make([]string, 0, len(pending))
			for ws := range pending {
				holdouts = append(holdouts, string(ws))
			}
			sort.Strings(holdouts)
			c.log.Warn(opHandover, "still waiting for workspaces to fall free; nothing will be interrupted to hurry them",
				merge(fields, dlog.Context{
					"holdouts": holdouts,
					"warnings": warnings,
					"cadence":  c.deps.HoldoutWarnEvery.String(),
				}))
		}
	}

	// THE OUTGOING DAEMON TIMES THE WINDOW, so it must still be alive when the
	// window closes: letting the windows settle before the exit is what makes
	// the accountability record get written at all.
	windows.Wait()

	if reclaimed := plan.reclaimed.Load(); reclaimed > 0 {
		// THE RECLAIMED ARE SERVED HERE AGAIN, so exiting would abandon them
		// exactly as a failed transfer would.
		c.log.Error(opHandover, "the handover cannot finish: workspaces whose adoption never landed were reclaimed and are served here again; not exiting",
			merge(fields, dlog.Context{"reclaimed": reclaimed, "untransferred": failed}))
		return
	}
	if failed > 0 {
		// A WORKSPACE THIS DAEMON STILL SERVES IS NOT ABANDONED. Exiting would
		// leave it with no daemon at all; staying leaves the two-daemon steady
		// state the ruling accepts, loudly.
		c.log.Error(opHandover, "the handover cannot finish: workspaces that did not transfer are still served here; not exiting",
			merge(fields, dlog.Context{"untransferred": failed}))
		return
	}

	// NOTHING IS LEFT STANDING. Every workspace this daemon served has either
	// moved to the successor above or is stood down here; the exit below owns
	// no process either way.
	c.standDownTheUntransferred(ctx, plan.untransferable)

	c.log.Info(opHandover, "every workspace is transferred; exiting", fields)
	if err := c.deps.Exit(ctx); err != nil {
		c.log.Error(opHandover, "the orderly exit could not be started", withCause(fields, err))
	}
}

// transfer moves one workspace to the successor. It is the bounce registry's
// action for a handover: the registry has already decided the workspace is
// free (or the handover is forced) and drained its dispatch.
//
// EVERY FAILURE PAST THE QUIESCE TAKES THE WORKSPACE BACK (reclaim): the hold
// it took is released and, if the shim was already detached, the running shim
// is re-adopted, so a transfer that failed leaves the workspace served HERE --
// never held by a lease nobody will release.
func (c *controller) transfer(ctx context.Context, ws wsm.Workspace, plan *handoverPlan, expected Participants, windows *sync.WaitGroup) error {
	successor := plan.successor
	fields := dlog.Context{"workspace": string(ws.ID), "successor": successor}

	// QUIESCE FIRST. From here on this daemon does NO work for the workspace:
	// every arrival is held rather than served, so nothing the successor is
	// about to own is still moving under it.
	lease, err := c.deps.Quiesce(ctx, ws.ID)
	if err != nil {
		c.log.Error(opTransfer, "could not quiesce the workspace's intake", withCause(fields, err))
		return fmt.Errorf("rollout: transfer %q: quiesce: %w", ws.ID, err)
	}
	fields["lease"] = string(lease)
	c.log.Debug(opTransfer, "quiesced the workspace's intake", fields)
	_, hadShim := c.deps.Shims.Client(ws.ID)

	// DETACH, NEVER KILL. The shim keeps running and KEEPS its kernel lock
	// through the whole handover; the lock is the shim's, and the successor
	// dials the still-locked process. The watches close WITH the detach: the
	// shim is the successor's from here, and its streams ending later are the
	// successor's business, never this daemon's fault.
	handed, err := c.deps.Shims.HandOver(ws.ID)
	switch {
	case err != nil:
		c.log.Error(opTransfer, "could not hand the workspace's shim over; taking the workspace back", withCause(fields, err))
		return errors.Join(fmt.Errorf("rollout: transfer %q: hand over the shim: %w", ws.ID, err),
			c.takeBack(ctx, ws.ID, lease, hadShim, fields))
	case handed:
		c.log.Debug(opTransfer, "detached from the workspace's shim, leaving it running", fields)
	default:
		c.log.Debug(opTransfer, "the workspace has no live shim to detach from", fields)
	}

	if err := c.releaseServing(ctx, ws.ID, fields); err != nil {
		return errors.Join(fmt.Errorf("rollout: transfer %q: release serving: %w", ws.ID, err),
			c.takeBack(ctx, ws.ID, lease, hadShim, fields))
	}

	c.recordTransfer(ws.ID, successor)
	c.deps.Pusher.PushTransferred(ws.ID, successor)
	c.log.Info(opTransfer, "pushed the transfer notice on the workspace's host and web streams",
		merge(fields, dlog.Context{"expected_host": expected.Host, "expected_web": expected.Web}))

	windows.Add(1)
	go func() {
		defer windows.Done()
		if c.timeAdoption(ctx, ws.ID, lease, handed, fields) {
			plan.reclaimed.Add(1)
		}
	}()
	return nil
}

// takeBack is a failed transfer's reclaim: the workspace never reached the
// successor, so it is served here again. Its answer is the reclaim's own
// failure, joined onto the transfer's.
func (c *controller) takeBack(ctx context.Context, ws ids.WorkspaceID, lease ids.LeaseID, hadShim bool, fields dlog.Context) error {
	_, err := c.reclaim(ctx, ws, lease, hadShim, fields)
	return err
}

// reclaim takes a workspace this daemon handed toward the successor BACK and
// serves it again. It answers whether the workspace is this daemon's again:
// false when the successor already owns it, which is not a failure.
//
// THE SERVING ROW ARBITRATES. The successor's adoption and this reclaim race
// for the same released row, and ClaimUnownedServing lets exactly one of them
// win; only the winner dials the shim. A successor that won has adopted the
// workspace, and there is nothing to take back.
//
// THE HOLD IS RELEASED ON EVERY PATH PAST A WON CLAIM, the shim re-attach's
// failure included: a workspace served here with its quiesce hold standing is
// the 2026-09-27 defect -- `restarting` drawn forever, every prompt refused.
// A shim that will not re-attach leaves the workspace served without a
// session, which its next prompt's revival adopts or replaces.
//
// It runs on a context the handover's own cancellation cannot refuse: the
// release it owes is exactly what must not be skipped.
func (c *controller) reclaim(ctx context.Context, ws ids.WorkspaceID, lease ids.LeaseID, reattach bool, fields dlog.Context) (bool, error) {
	ctx = context.WithoutCancel(ctx)
	claimed, holder, err := c.deps.DB.ClaimUnownedServing(ctx, ws, c.deps.Instance)
	if err != nil {
		c.log.Error(opTransfer, "could not take the workspace back: its serving ownership could not be claimed; its hold stays until this daemon's state handle closes",
			withCause(fields, err))
		return false, fmt.Errorf("rollout: reclaim %q: claim serving: %w", ws, err)
	}
	if !claimed {
		c.log.Info(opTransfer, "the successor already owns the workspace; nothing to take back",
			merge(fields, dlog.Context{"owner": string(holder)}))
		return false, nil
	}
	c.untransfer(ws)
	var failures []error
	if _, live := c.deps.Shims.Client(ws); reattach && !live {
		if _, err := c.deps.Shims.Adopt(ctx, ws); err != nil {
			c.log.Error(opTransfer, "the taken-back workspace's running shim could not be re-attached; it is served without a session until its next prompt revives one",
				withCause(fields, err))
			failures = append(failures, fmt.Errorf("rollout: reclaim %q: re-attach the shim: %w", ws, err))
		}
	}
	if err := c.releaseHold(ctx, ws, lease, fields); err != nil {
		failures = append(failures, err)
	}
	if c.deps.PublishViews != nil {
		if err := c.deps.PublishViews(ctx, ws); err != nil {
			c.log.Error(opTransfer, "the taken-back workspace's views could not be republished", withCause(fields, err))
			failures = append(failures, fmt.Errorf("rollout: reclaim %q: publish views: %w", ws, err))
		}
	}
	c.log.Info(opTransfer, "took the workspace back; this daemon serves it again", fields)
	return true, errors.Join(failures...)
}

// releaseHold releases the quiesce hold a transfer took -- by its own id, never
// another holder's -- and tells the queue, which is what drains the intake it
// held, and the host view, whose composer arm the hold is.
func (c *controller) releaseHold(ctx context.Context, ws ids.WorkspaceID, lease ids.LeaseID, fields dlog.Context) error {
	if lease == "" {
		c.log.Debug(opTransfer, "the transfer took no hold of its own; another holder's lease is left as it stands", fields)
		c.deps.LeaseChanged(ws)
		c.publishHost(ws)
		return nil
	}
	if err := c.deps.DB.ReleaseLease(ctx, lease); err != nil {
		c.log.Error(opTransfer, "could not release the transfer's hold; it stays until this daemon's state handle closes",
			withCause(fields, err))
		return fmt.Errorf("rollout: release the hold %s of %q: %w", lease, ws, err)
	}
	c.deps.LeaseChanged(ws)
	c.publishHost(ws)
	c.log.Debug(opTransfer, "released the transfer's hold", fields)
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

// timeAdoption gives the successor its window, and TAKES THE WORKSPACE BACK
// when it expires. It answers whether it did.
//
// AN EXPIRED WINDOW RECLAIMS; it does not merely record. Before this, expiry
// only opened the workspace's fault and left it quiesced, detached and
// released -- served by no daemon, its hold standing -- until a later boot
// (2026-09-27: five workspaces, then every prompt refused for hours). Now the
// workspace is reclaimed through the serving row's arbitration: a successor
// that adopted in the last instant keeps it, and otherwise this daemon serves
// it again with its hold released. The fault is still opened, as the record
// of an adoption that never landed.
func (c *controller) timeAdoption(ctx context.Context, ws ids.WorkspaceID, lease ids.LeaseID, detached bool, fields dlog.Context) bool {
	// An adoption that already landed needs no window: the headless case claims
	// serving inside Join, before the push it is answering was even read.
	if c.adopted(ctx, ws, fields) {
		return false
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
			return false
		case <-done:
			c.log.Debug(opAdoption, "the rendezvous completed inside its window", fields)
			return false
		case <-look.C:
			if c.adopted(ctx, ws, fields) {
				return false
			}
		case <-expired:
			waiting = false
		}
	}
	window := dlog.Context{"adoption_window": c.deps.AdoptionWindow.String()}
	reclaimed, err := c.reclaim(ctx, ws, lease, detached, merge(fields, window))
	if !reclaimed {
		if err == nil {
			// The arbitration answered for the successor: it adopted in the
			// window's last instant.
			return false
		}
		c.log.Error(opAdoption, "the adoption window expired and the workspace could not be taken back",
			withCause(merge(fields, window), err))
		return false
	}
	workspace := ws
	if _, faultErr := c.deps.DB.OpenFault(context.WithoutCancel(ctx), wsm.Fault{
		Workspace: &workspace,
		Kind:      FaultAdoptionExpired,
		Detail: fmt.Sprintf("the successor did not claim this workspace within the %s adoption window; this daemon took it back",
			c.deps.AdoptionWindow),
		Evidence: map[string]string{"adoption_window": c.deps.AdoptionWindow.String()},
		OpenedAt: c.deps.Clock.Now(),
	}); faultErr != nil {
		c.log.Error(opAdoption, "could not record the expired adoption window", withCause(fields, faultErr))
	}
	if err != nil {
		c.log.Error(opAdoption, "the adoption window expired; the workspace was taken back, but not cleanly",
			withCause(merge(fields, window), err))
		return true
	}
	c.log.Error(opAdoption, "the adoption window expired; the successor never adopted the workspace, so this daemon took it back and serves it again",
		merge(fields, window))
	return true
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
			// A LIVE SESSION IS THE TRUTH, the row is only its record. A
			// workspace this daemon holds a live shim client for IS served
			// by this daemon whatever the row says, and skipping it is how a
			// cold-started daemon's handover orphaned three live shims
			// (2026-09-24: the rows named dead instance ce34b5e9cd834d09, the
			// successor adopted nothing). The disagreement is an invariant
			// violation -- every bring-up claims serving (workspace
			// claimServing) -- so it is stated at ERROR, the row is repaired
			// so the transfer's release and the successor's adoption see this
			// daemon as the owner, and the workspace is handed over.
			if _, live := c.deps.Shims.Client(ws.ID); !live {
				c.log.Debug(opHandover, "a workspace this daemon holds no session for is served by another instance or none; it is not handed over",
					dlog.Context{"workspace": string(ws.ID), "owner": ownerText(owner)})
				continue
			}
			if err := c.reclaimLiveSession(ctx, ws.ID, owner); err != nil {
				return nil, nil, err
			}
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

// ownerText spells a serving owner for a record; none is the empty string.
func ownerText(owner *ids.InstanceID) string {
	if owner == nil {
		return ""
	}
	return string(*owner)
}

// reclaimLiveSession repairs the serving row of a workspace this daemon holds
// a live session for while the row names another instance (or none), stating
// the violation at ERROR first. A claim that fails abandons the handover: the
// transfer would then release nothing and the successor would refuse the
// adoption of a workspace "served by an unexpected daemon".
func (c *controller) reclaimLiveSession(ctx context.Context, ws ids.WorkspaceID, owner *ids.InstanceID) error {
	fields := dlog.Context{"workspace": string(ws), "owner": ownerText(owner), "instance": string(c.deps.Instance)}
	c.log.Error(opHandover, "this daemon holds a live session for a workspace whose serving row names another instance; the live session is the truth, so it is reclaimed and handed over", fields)
	if err := c.deps.DB.ClaimServing(ctx, ws, c.deps.Instance); err != nil {
		c.log.Error(opHandover, "could not reclaim serving ownership of a workspace this daemon holds a live session for", withCause(fields, err))
		return fmt.Errorf("rollout: handover: reclaim serving for %q: %w", ws, err)
	}
	return nil
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
