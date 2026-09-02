package rollout

import (
	"context"
	"fmt"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/sessionlock"
)

// Join is the JOINING daemon's half of the handover.
//
// It reads the intent manifest (the only thing the outgoing daemon left it —
// there is NO daemon-to-daemon channel), reconciles every session's disposition
// against the kernel locks, arms the rendezvous from the participants the
// manifest recorded, and adopts every HEADLESS workspace at once: zero expected
// participants means there is nothing to wait for, and Emacs may not even be
// running.
func (c *controller) Join(ctx context.Context) error {
	// JOINING MODE IS THE FACT, not the manifest. A successor owns NOTHING
	// until it adopts, and the manifest may not exist yet when it boots: the
	// incumbent writes it only after the successor has reported its address.
	// Until then every per-workspace rpc must answer not_yet_adopted rather
	// than fall through to a read-only state handle.
	c.mu.Lock()
	c.joiningMode = true
	c.mu.Unlock()

	found, err := c.joinFromManifest(ctx)
	if err != nil {
		return err
	}
	if !found {
		// THE MANIFEST ARRIVES LATE BY DESIGN: the incumbent writes it only
		// after this daemon has reported its address, so a successor that
		// boots first finds nothing. A participant's own adopt call re-arms
		// from a manifest that arrived after boot — but a HEADLESS workspace
		// has no participant to trigger that, so the successor keeps looking
		// until the manifest lands, and the incumbent's transfer never waits
		// out an adoption window for a workspace nobody was ever going to
		// adopt on its behalf.
		c.log.Debug(opJoin, "no intent manifest yet; watching for it",
			dlog.Context{"path": c.deps.IntentManifest})
		go c.awaitManifest(ctx)
	}
	return nil
}

// manifestPoll is how often a joining daemon looks for a manifest that has not
// arrived yet.
const manifestPoll = 25 * time.Millisecond

// awaitManifest keeps looking for the intent manifest until it lands or the
// daemon stops.
func (c *controller) awaitManifest(ctx context.Context) {
	ticker := time.NewTicker(manifestPoll)
	defer ticker.Stop()
	for {
		select {
		case <-ctx.Done():
			return
		case <-ticker.C:
		}
		found, err := c.joinFromManifest(ctx)
		if err != nil {
			c.log.Error(opJoin, "the intent manifest could not be joined from",
				withCause(dlog.Context{"path": c.deps.IntentManifest}, err))
			return
		}
		if found {
			return
		}
	}
}

// joinFromManifest arms the rendezvous from the intent manifest and adopts
// every headless workspace. It reports whether a manifest was there to read.
func (c *controller) joinFromManifest(ctx context.Context) (bool, error) {
	m, found, err := ReadManifest(c.deps.IntentManifest)
	if err != nil {
		c.log.Error(opJoin, "could not read the intent manifest",
			withCause(dlog.Context{"path": c.deps.IntentManifest}, err))
		return false, err
	}
	if !found {
		return false, nil
	}
	if _, err := c.Reconcile(ctx); err != nil {
		return false, err
	}

	headless := make([]ids.WorkspaceID, 0, len(m.Sessions))
	c.mu.Lock()
	if c.joining == nil {
		c.joining = make(map[ids.WorkspaceID]bool, len(m.Sessions))
	}
	if c.owned == nil {
		c.owned = make(map[ids.WorkspaceID]bool, len(m.Sessions))
	}
	for _, session := range m.Sessions {
		expected := Participants{Host: session.ExpectedHost, Web: session.ExpectedWeb}
		c.rendezvous[session.Workspace] = &entry{expected: expected, done: make(chan struct{})}
		c.joining[session.Workspace] = true
		if expected.Count() == 0 {
			headless = append(headless, session.Workspace)
		}
	}
	c.mu.Unlock()

	c.log.Info(opJoin, "armed the adopt rendezvous from the intent manifest", dlog.Context{
		"outgoing_daemon": string(m.Daemon),
		"workspaces":      len(m.Sessions),
		"headless":        len(headless),
	})

	for _, ws := range headless {
		if err := c.adopt(ctx, ws, "headless"); err != nil {
			// A headless workspace that will not adopt is that workspace's own
			// failure, not the join's: the rest still transfer.
			c.log.Error(opJoin, "a headless workspace could not be adopted",
				withCause(dlog.Context{"workspace": string(ws)}, err))
		}
	}
	return true, nil
}

// armFromManifest arms the rendezvous from the intent manifest as it stands
// NOW, adding what is not already armed and touching nothing that is: an entry
// a participant has already called on keeps its ledger.
func (c *controller) armFromManifest() error {
	m, found, err := ReadManifest(c.deps.IntentManifest)
	if err != nil || !found {
		return err
	}
	added := 0
	c.mu.Lock()
	for _, session := range m.Sessions {
		if _, already := c.rendezvous[session.Workspace]; already {
			continue
		}
		c.rendezvous[session.Workspace] = &entry{
			expected: Participants{Host: session.ExpectedHost, Web: session.ExpectedWeb},
			done:     make(chan struct{}),
		}
		if c.joining == nil {
			c.joining = map[ids.WorkspaceID]bool{}
		}
		c.joining[session.Workspace] = true
		added++
	}
	c.mu.Unlock()
	if added > 0 {
		c.log.Info(opJoin, "armed the adopt rendezvous from a manifest that arrived after boot",
			dlog.Context{"workspaces": added})
	}
	return nil
}

// AdoptHost is Emacs's half of the rendezvous, called on the NEW daemon.
func (c *controller) AdoptHost(ctx context.Context, ws ids.WorkspaceID) error {
	return c.rendezvousCall(ctx, ws, opAdoptHost, func(e *entry) { e.hostCalled = true }, func(e *entry) bool {
		return e.expected.Host
	})
}

// AdoptWeb is the webview's half of the rendezvous.
//
// THE WEB SIDE NEVER REDIALS. The webapp does not dial the successor: Emacs
// reloads the webview at the successor's address and the FRESH page calls this
// once at boot, before opening any view stream. So a call with no transfer
// announced is the ORDINARY case — every non-handover page boot makes one —
// and is recorded at INFO, never as a warning and never as a fault. The web
// slot is satisfied by the first call from ANY connection, because it is the
// reloaded page, not a surviving stream, that is the participant.
func (c *controller) AdoptWeb(ctx context.Context, ws ids.WorkspaceID) error {
	return c.rendezvousCall(ctx, ws, opAdoptWeb, func(e *entry) { e.webCalled = true }, func(e *entry) bool {
		return e.expected.Web
	})
}

// rendezvousCall is the body both adopt verbs share: the same ledger, the same
// refusals, the same completion. The VERB identifies the participant, which is
// why the two differ only in which slot they fill.
func (c *controller) rendezvousCall(ctx context.Context, ws ids.WorkspaceID, operation string, fill func(*entry), expects func(*entry) bool) error {
	fields := dlog.Context{"workspace": string(ws)}

	c.mu.Lock()
	e, armed := c.rendezvous[ws]
	if !armed && c.joiningMode {
		// THE MANIFEST MAY HAVE ARRIVED SINCE BOOT. The incumbent writes it
		// only after the successor has reported its address, so a successor
		// that armed nothing at boot is the ordinary case, not a refusal: it
		// re-reads once, here, when a participant actually calls.
		c.mu.Unlock()
		if err := c.armFromManifest(); err != nil {
			c.log.Error(operation, "could not re-read the intent manifest", withCause(fields, err))
			return err
		}
		c.mu.Lock()
		e, armed = c.rendezvous[ws]
	}
	if !armed {
		c.mu.Unlock()
		// INFO, not WARN: on the web side this is what a page boot looks like
		// when nothing was handed over, which is almost every boot.
		c.log.Info(operation, "no transfer was announced for this workspace", fields)
		return ErrNoTransferAnnounced
	}
	if e.adopted {
		c.mu.Unlock()
		c.log.Debug(operation, "the workspace is already adopted; the call succeeds at once", fields)
		return nil
	}
	if !expects(e) {
		c.mu.Unlock()
		c.log.Warn(operation, "this participant's stream was not open at announcement", fields)
		return ErrParticipantNotExpected
	}
	fill(e)
	if !e.satisfied() {
		outstanding := dlog.Context{
			"expected_host": e.expected.Host, "expected_web": e.expected.Web,
			"host_called": e.hostCalled, "web_called": e.webCalled,
		}
		done := e.done
		c.mu.Unlock()
		c.log.Debug(operation, "an expected participant has not called yet; waiting for the rendezvous",
			merge(fields, outstanding))
		// EVERY EXPECTED PARTICIPANT SUCCEEDS TOGETHER. The callers arrive
		// concurrently and the one that arrives first has not failed: it
		// waits for the one that completes the rendezvous. `not_yet_adopted`
		// on an ADOPT call means only that this caller's own context expired
		// first, which is the retry-with-backoff case.
		select {
		case <-done:
			c.mu.Lock()
			failed := e.failed
			c.mu.Unlock()
			if failed != nil {
				return failed
			}
			c.log.Debug(operation, "the rendezvous completed while this caller waited", fields)
			return nil
		case <-ctx.Done():
			c.log.Debug(operation, "the caller gave up before the rendezvous completed", fields)
			return ErrNotYetAdopted
		}
	}
	c.mu.Unlock()

	err := c.adopt(ctx, ws, operation)
	c.mu.Lock()
	e.settle(err)
	c.mu.Unlock()
	if err != nil {
		return err
	}
	c.log.Info(operation, "every expected participant called; the workspace is adopted", fields)
	return nil
}

// adopt completes one workspace's adoption on the joining daemon: claim the
// running shim, claim serving ownership, drain the intake the outgoing daemon
// quiesced, publish fresh views — and, once every joining workspace is owned,
// write daemon.addr.
//
// The kernel lock is PROBED rather than taken: it is the SHIM's, held
// continuously across the handover, and a workspace whose lock reads FREE has
// no surviving shim to adopt.
func (c *controller) adopt(ctx context.Context, ws ids.WorkspaceID, source string) error {
	fields := dlog.Context{"workspace": string(ws), "source": source}

	// THE HANDLE BECOMES A WRITING ONE HERE. A successor opens read-only
	// because the incumbent is still the sole writer; adopting a workspace is
	// the moment it starts writing that workspace's rows, and the incumbent
	// stopped writing them at its transfer notice.
	if err := c.deps.DB.Promote(ctx); err != nil {
		c.log.Error(opAdopt, "the state handle could not be promoted to writing", withCause(fields, err))
		return fmt.Errorf("rollout: adopt %q: promote the state handle: %w", ws, err)
	}
	c.flushDispositions(ctx)
	record, err := c.deps.DB.Workspace(ctx, ws)
	if err != nil {
		c.log.Error(opAdopt, "could not read the workspace being adopted", withCause(fields, err))
		return fmt.Errorf("rollout: adopt %q: %w", ws, err)
	}
	// A FREE LOCK MEANS THERE IS NO PROCESS TO DIAL. A workspace registered but
	// never opened transfers on its WSM facts alone; dialing a shim that was
	// never spawned fails the adoption of a workspace that is in no trouble at
	// all, and the incumbent then waits out an adoption window for it.
	dial := true
	if c.deps.LockProbe != nil {
		state, probeErr := c.deps.LockProbe(record.Dir)
		fields["lock"] = state.String()
		switch {
		case probeErr != nil:
			c.log.Warn(opAdopt, "the workspace lock probe could not tell; adopting on the WSM facts alone",
				withCause(fields, probeErr))
		case state == sessionlock.StateFree:
			c.log.Debug(opAdopt, "no shim holds this workspace's lock; adopting its facts with no session", fields)
			dial = false
		case state == sessionlock.StateUnknown:
			// "Could not tell" is NEVER read as free, and it is never read as
			// held either: it is said out loud.
			c.log.Warn(opAdopt, "the workspace lock probe could not tell; adopting on the WSM facts alone", fields)
		default:
			c.log.Debug(opAdopt, "a shim still holds the workspace's lock, as a handover expects", fields)
		}
	}

	if dial {
		if _, err := c.deps.Shims.Adopt(ctx, ws); err != nil {
			c.log.Error(opAdopt, "could not adopt the workspace's running shim", withCause(fields, err))
			return fmt.Errorf("rollout: adopt %q: dial the running shim: %w", ws, err)
		}
	}
	if err := c.deps.DB.ClaimServing(ctx, ws, c.deps.Instance); err != nil {
		c.log.Error(opAdopt, "could not claim serving ownership", withCause(fields, err))
		return fmt.Errorf("rollout: adopt %q: claim serving: %w", ws, err)
	}
	if err := c.deps.DrainIntake(ctx, ws); err != nil {
		c.log.Error(opAdopt, "could not drain the held intake", withCause(fields, err))
		return fmt.Errorf("rollout: adopt %q: drain the held intake: %w", ws, err)
	}
	if err := c.deps.PublishViews(ctx, ws); err != nil {
		c.log.Error(opAdopt, "could not publish the workspace's fresh views", withCause(fields, err))
		return fmt.Errorf("rollout: adopt %q: publish views: %w", ws, err)
	}

	c.mu.Lock()
	if e, ok := c.rendezvous[ws]; ok {
		e.adopted = true
	}
	if c.owned == nil {
		c.owned = map[ids.WorkspaceID]bool{}
	}
	c.owned[ws] = true
	complete := len(c.joining) > 0
	for joined := range c.joining {
		if !c.owned[joined] {
			complete = false
			break
		}
	}
	c.mu.Unlock()

	c.log.Info(opAdopt, "adopted the workspace", fields)

	if complete && c.deps.WriteDaemonAddr != nil {
		// A JOINING DAEMON OTHERWISE NEVER WRITES daemon.addr: until every
		// workspace is its own, the incumbent's file is still the truth.
		c.advertise(ctx, fields)
	}
	return nil
}

// advertise writes daemon.addr once every workspace is owned, retrying while
// the OUTGOING daemon still holds the boot claim.
//
// THE ADVERTISEMENT IS NOT PART OF THE ADOPTION. The incumbent releases its
// claim when it exits, and it exits when the adoptions complete — so failing an
// adoption because the claim is still held deadlocks the handover on itself.
// The workspace is adopted either way; only the address file waits.
func (c *controller) advertise(ctx context.Context, fields dlog.Context) {
	if err := c.deps.WriteDaemonAddr(ctx); err == nil {
		c.log.Info(opAdopt, "every workspace is owned; wrote daemon.addr", nil)
		return
	}
	c.log.Debug(opAdopt, "the outgoing daemon still holds the boot claim; advertising once it lets go", fields)
	// THE RETRY OUTLIVES THE CALL. The context here is an rpc's, cancelled the
	// moment the adopt answers — and the claim is released by the incumbent's
	// exit, which happens after that answer.
	ctx = context.WithoutCancel(ctx)
	go func() {
		ticker := time.NewTicker(manifestPoll)
		defer ticker.Stop()
		for {
			select {
			case <-ctx.Done():
				return
			case <-ticker.C:
			}
			if err := c.deps.WriteDaemonAddr(ctx); err == nil {
				c.log.Info(opAdopt, "every workspace is owned; wrote daemon.addr", nil)
				return
			}
		}
	}()
}
