package rollout

import (
	"context"
	"fmt"
	"sync"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// FaultAdoptionExpired is the fault kind an expired adoption window records.
const FaultAdoptionExpired = "adoption_window_expired"

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

	workspaces, err := c.served(ctx)
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
		c.rendezvous[ws.ID] = &entry{expected: p}
	}
	c.mu.Unlock()
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

	// The adoption windows are TIMED, not waited on for progress: expiry is the
	// workspace's own fault record and nothing else. Letting them settle before
	// the exit is what makes the record get written at all.
	windows.Wait()

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

	if err := c.deps.DB.ReleaseServing(ctx, ws.ID, c.deps.Instance); err != nil {
		c.log.Warn(opTransfer, "could not release serving ownership", withCause(fields, err))
	}

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

// timeAdoption gives the successor its window and records the workspace's OWN
// fault when it expires. There is deliberately no abort and no retry
// machinery: expiry is remediated as it comes up.
func (c *controller) timeAdoption(ctx context.Context, ws ids.WorkspaceID, fields dlog.Context) {
	select {
	case <-ctx.Done():
		c.log.Debug(opAdoption, "the adoption window ended with its context", fields)
		return
	case <-c.deps.Clock.After(c.deps.AdoptionWindow):
	}
	owner, err := c.deps.DB.Serving(ctx, ws)
	if err != nil {
		c.log.Error(opAdoption, "could not read serving ownership at the window's expiry", withCause(fields, err))
		return
	}
	if owner != nil && *owner != c.deps.Instance {
		c.log.Debug(opAdoption, "the successor claimed the workspace inside its window",
			merge(fields, dlog.Context{"owner": string(*owner)}))
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

// served lists the workspaces this daemon currently serves. Serving ownership
// is the WSM fact that arbitrates the overlap, so it — not "has a shim" — is
// what a handover moves.
func (c *controller) served(ctx context.Context) ([]wsm.Workspace, error) {
	all, err := c.deps.DB.ListWorkspaces(ctx)
	if err != nil {
		c.log.Error(opHandover, "could not list the workspaces to hand over", withCause(nil, err))
		return nil, fmt.Errorf("rollout: handover: %w", err)
	}
	out := make([]wsm.Workspace, 0, len(all))
	for _, ws := range all {
		owner, err := c.deps.DB.Serving(ctx, ws.ID)
		if err != nil {
			c.log.Error(opHandover, "could not read a workspace's serving ownership",
				withCause(dlog.Context{"workspace": string(ws.ID)}, err))
			return nil, fmt.Errorf("rollout: handover: %w", err)
		}
		if owner != nil && *owner == c.deps.Instance {
			out = append(out, ws)
		}
	}
	return out, nil
}

// manifest builds the stand-down record for every workspace being handed over.
// Every one of them is IntentPreserve: a handover kills nothing.
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
			Intent:       IntentPreserve,
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
			}
		}
		m.Sessions = append(m.Sessions, record)
	}
	return m
}
