package rollout

import (
	"context"
	"fmt"

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
	m, found, err := ReadManifest(c.deps.IntentManifest)
	if err != nil {
		c.log.Error(opJoin, "could not read the intent manifest",
			withCause(dlog.Context{"path": c.deps.IntentManifest}, err))
		return err
	}
	if !found {
		c.log.Debug(opJoin, "no intent manifest is present; this daemon is not joining anything",
			dlog.Context{"path": c.deps.IntentManifest})
		return nil
	}
	if _, err := c.Reconcile(ctx); err != nil {
		return err
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
		c.rendezvous[session.Workspace] = &entry{expected: expected}
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
		c.mu.Unlock()
		c.log.Debug(operation, "an expected participant has not called yet", merge(fields, outstanding))
		return ErrNotYetAdopted
	}
	c.mu.Unlock()

	if err := c.adopt(ctx, ws, operation); err != nil {
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

	record, err := c.deps.DB.Workspace(ctx, ws)
	if err != nil {
		c.log.Error(opAdopt, "could not read the workspace being adopted", withCause(fields, err))
		return fmt.Errorf("rollout: adopt %q: %w", ws, err)
	}
	if c.deps.LockProbe != nil {
		state, probeErr := c.deps.LockProbe(record.Dir)
		fields["lock"] = state.String()
		switch {
		case probeErr != nil:
			c.log.Warn(opAdopt, "the workspace lock probe could not tell; adopting on the WSM facts alone",
				withCause(fields, probeErr))
		case state == sessionlock.StateFree:
			c.log.Warn(opAdopt, "no shim holds this workspace's lock; there is nothing running to adopt", fields)
		case state == sessionlock.StateUnknown:
			// "Could not tell" is NEVER read as free, and it is never read as
			// held either: it is said out loud.
			c.log.Warn(opAdopt, "the workspace lock probe could not tell; adopting on the WSM facts alone", fields)
		default:
			c.log.Debug(opAdopt, "a shim still holds the workspace's lock, as a handover expects", fields)
		}
	}

	if _, err := c.deps.Shims.Adopt(ctx, ws); err != nil {
		c.log.Error(opAdopt, "could not adopt the workspace's running shim", withCause(fields, err))
		return fmt.Errorf("rollout: adopt %q: dial the running shim: %w", ws, err)
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
		if err := c.deps.WriteDaemonAddr(ctx); err != nil {
			c.log.Error(opAdopt, "could not write daemon.addr after adopting every workspace",
				withCause(fields, err))
			return fmt.Errorf("rollout: adopt %q: write daemon.addr: %w", ws, err)
		}
		c.log.Info(opAdopt, "every workspace is owned; wrote daemon.addr", nil)
	}
	return nil
}
