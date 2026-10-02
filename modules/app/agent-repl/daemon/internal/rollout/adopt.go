package rollout

import (
	"context"
	"errors"
	"fmt"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/daemonaddr"
	"claude-repld/internal/deployprogress"
	"claude-repld/internal/dlog"
	"claude-repld/internal/health"
	"claude-repld/internal/ids"
	"claude-repld/internal/sessionlock"
	"claude-repld/internal/wsm"
)

// Join is the JOINING daemon's half of the handover.
//
// It reads the intent manifest (the only thing the outgoing daemon left it —
// there is NO daemon-to-daemon channel), reconciles every session's disposition
// against the kernel locks, arms the rendezvous from the participants the
// manifest recorded, and starts adopting every HEADLESS workspace without a
// participant call. Every adoption still waits for the incumbent's serving
// release: that durable edge says the workspace passed freeness, its intake is
// quiesced and its shim is detached.
func (c *controller) Join(ctx context.Context) error {
	// JOINING MODE IS THE FACT, not the manifest. A successor owns NOTHING
	// until it adopts, and the manifest may not exist yet when it boots: the
	// incumbent writes it only after the successor has reported its address.
	// Until then every per-workspace rpc must answer not_yet_adopted rather
	// than fall through to a read-only state handle.
	c.mu.Lock()
	c.joiningMode = true
	c.mu.Unlock()

	// THE TAKEOVER IS KEYED ON THE INCUMBENT'S EXIT, NOT ON EVERY RENDEZVOUS
	// FINISHING. It used to be triggered only by an adoption that left every
	// joining workspace owned — so ONE workspace whose adoption never
	// completed (closed mid-handover; its window expired on the incumbent)
	// kept this daemon joining forever, with no daemon.addr written at all:
	// every later rollout was refused and nothing could find the daemon.
	lifetime := c.deps.Lifetime
	if lifetime == nil {
		lifetime = context.WithoutCancel(ctx)
	}
	go c.awaitIncumbentExit(lifetime)

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
	c.recordManifestSeen()
	c.recordForcedTakeover(m.Forced)
	c.recordDeployTakeover(m.Deploy)
	// A JOINING SUCCESSOR ADOPTED NOTHING AT BOOT — the manifest is present
	// here by construction, so the no-manifest accounting cannot apply.
	_, _, landed, err := c.reconcile(ctx, Survivors{})
	if err != nil {
		return false, err
	}

	armed := c.armSessions(m.Daemon, m.Sessions)
	// RETIRED ONLY ONCE IT IS ARMED FROM, so a participant's own re-read
	// (armFromManifest) can never find the file gone before the rendezvous
	// it states exists. A read-only handle defers the accounting, and then
	// flushDispositions retires it once the deferred records are written.
	if landed {
		c.retireManifest("the joining daemon armed from it and recorded every disposition it names")
	}
	// THE HEADLESS SET IS TAKEN FROM THE LEDGER, NOT FROM THIS READ. Arming is
	// additive, so a workspace armed by an EARLIER read — a participant's own
	// adopt call re-reads the manifest before this poll gets to it — adds
	// nothing here. It still has to be adopted: a headless workspace has no
	// participant to adopt it and the incumbent would otherwise wait out the
	// whole adoption window for it. claimHeadless hands back every armed
	// headless workspace this daemon has not already claimed, so exactly one
	// read adopts each of them.
	headless := c.claimHeadless()

	c.log.Info(opJoin, "armed the adopt rendezvous from the intent manifest", dlog.Context{
		"outgoing_daemon": string(m.Daemon),
		"workspaces":      armed,
		"armed_total":     c.rendezvousSize(),
		"headless":        len(headless),
	})

	for _, ws := range headless {
		if err := c.adopt(ctx, ws, "headless"); err != nil {
			// A headless workspace that will not adopt is that workspace's own
			// failure, not the join's: the rest still transfer.
			c.log.Error(opJoin, "a headless workspace could not be adopted",
				withCause(dlog.Context{"workspace": string(ws)}, err))
			// THE CLAIM IS RELEASED ON FAILURE so this workspace can be tried
			// again; an adoption that never happened must not look like one
			// that did.
			c.releaseHeadless(ws)
			// AND SOMETHING MUST ACTUALLY TRY AGAIN. Releasing the claim only
			// makes a retry POSSIBLE; for a headless workspace nothing was
			// making one. `awaitManifest` stops the moment a manifest is read
			// (this function answered "found"), `armFromManifest` arms and
			// never adopts, and a headless workspace has no participant whose
			// own adopt call would come round — so a single transient failure
			// here used to strand the workspace on the incumbent for good.
			//
			// THE COST OF THAT GAP, and it is the whole 30s: the incumbent
			// waits out its entire AdoptionWindow for a workspace nobody is
			// going to adopt (`handover.go`), which delays `windows.Wait()`,
			// which delays its exit, which delays the primary stream close
			// EMACS PROMOTES THE SUCCESSOR ON (`lisp/daemon-link.el`
			// `agent-repl-link--handle-close'). That is the exact shape of
			// `TestEmacsHandoverTransfersAtFreeness' failing under load: the
			// promotion arrived at 32s and 42s against ~2s healthy, and the
			// headless workspace was still on the predecessor's address. The
			// failing run's dlog was not preserved, so which call inside
			// `adopt` returned the error is not on record; what IS on record
			// is that no second attempt could ever have been made.
			go c.retryHeadless(ctx, ws)
		}
	}
	return true, nil
}

// The headless re-adoption backoff. This is a FAILURE path, not a happy one:
// the first retry is soon enough that an ordinary transient costs a small
// fraction of the adoption window, and the cap keeps a workspace that will
// never adopt from turning a 30s window into 1200 log lines.
const (
	headlessRetryInitial = 100 * time.Millisecond
	headlessRetryMax     = 2 * time.Second
)

// retryHeadless keeps trying to adopt one headless workspace until it adopts,
// somebody else adopts it, or the daemon stops.
//
// IT RE-CLAIMS BEFORE EVERY ATTEMPT, so it can never race a manifest read that
// picked the workspace up in the meantime: `claimHeadless` and this both go
// through the same one-at-a-time claim, and whichever takes it is the sole
// adopter of that attempt.
//
// It is driven by the injected Clock, so a test drives the backoff rather than
// waiting it out.
func (c *controller) retryHeadless(ctx context.Context, ws ids.WorkspaceID) {
	delay := headlessRetryInitial
	for attempt := 1; ; attempt++ {
		select {
		case <-ctx.Done():
			return
		case <-c.deps.Clock.After(delay):
		}
		if !c.reclaimHeadless(ws) {
			// Adopted in the meantime, by a manifest read or a participant.
			return
		}
		fields := dlog.Context{"workspace": string(ws), "attempt": attempt}
		if err := c.adopt(ctx, ws, "headless-retry"); err != nil {
			c.log.Warn(opJoin, "a headless workspace could not be adopted; retrying",
				withCause(fields, err))
			c.releaseHeadless(ws)
			if delay *= 2; delay > headlessRetryMax {
				delay = headlessRetryMax
			}
			continue
		}
		c.log.Info(opJoin, "adopted the headless workspace on a retry", fields)
		return
	}
}

// reclaimHeadless takes the headless claim for ONE workspace, and reports
// whether this caller now holds it. It answers false for a workspace that has
// since been adopted, is claimed by somebody else, or is no longer armed.
func (c *controller) reclaimHeadless(ws ids.WorkspaceID) bool {
	c.mu.Lock()
	e, ok := c.rendezvous[ws]
	if !ok || e.adopted || e.headlessClaimed {
		c.mu.Unlock()
		return false
	}
	e.headlessClaimed = true
	c.mu.Unlock()
	c.logTransition(opJoin, ws, "headless_claimed", false, true, nil)
	return true
}

// armSessions arms the rendezvous for every manifest session NOT already
// armed, and reports how many entries it added.
//
// THE RENDEZVOUS IS ONE-SHOT PER HANDOVER. An entry a participant has already
// called on — or that has already completed its adoption — keeps its ledger:
// re-arming it would reset host_called/web_called, orphan the waiters on the
// old `done` channel, and leave a recovered page's own boot adopt waiting for
// a host participant that already came and went. The manifest is read more
// than once by design (a successor boots before the incumbent writes it, and
// both the awaiting poll and a participant's own call re-read it), so every
// read must be additive.
func (c *controller) armSessions(outgoing ids.InstanceID, sessions []ManifestSession) int {
	added := 0
	armed := make([]ManifestSession, 0, len(sessions))
	c.mu.Lock()
	if c.joining == nil {
		c.joining = make(map[ids.WorkspaceID]bool, len(sessions))
	}
	if c.owned == nil {
		c.owned = make(map[ids.WorkspaceID]bool, len(sessions))
	}
	for _, session := range sessions {
		if _, already := c.rendezvous[session.Workspace]; already {
			continue
		}
		expected := Participants{Host: session.ExpectedHost, Web: session.ExpectedWeb}
		c.rendezvous[session.Workspace] = &entry{
			outgoing: outgoing,
			expected: expected,
			done:     make(chan struct{}),
		}
		c.joining[session.Workspace] = true
		armed = append(armed, session)
		added++
	}
	c.mu.Unlock()
	for _, session := range armed {
		c.logTransition(opJoin, session.Workspace, "rendezvous", "unarmed", "armed", dlog.Context{
			"expected_host": session.ExpectedHost,
			"expected_web":  session.ExpectedWeb,
		})
	}
	return added
}

// claimHeadless takes ownership of every armed headless workspace — zero
// expected participants — that has not been claimed or adopted already, and
// reports them. A workspace comes back from it AT MOST ONCE, so two manifest
// reads adopt it once between them, and the caller is the sole adopter of what
// it is handed.
func (c *controller) claimHeadless() []ids.WorkspaceID {
	c.mu.Lock()
	claimed := make([]ids.WorkspaceID, 0, len(c.rendezvous))
	for ws, e := range c.rendezvous {
		if e.expected.Count() != 0 || e.adopted || e.headlessClaimed {
			continue
		}
		e.headlessClaimed = true
		claimed = append(claimed, ws)
	}
	c.mu.Unlock()
	for _, ws := range claimed {
		c.logTransition(opJoin, ws, "headless_claimed", false, true, nil)
	}
	return claimed
}

// rendezvousSize reports how many workspaces are armed in total.
func (c *controller) rendezvousSize() int {
	c.mu.Lock()
	defer c.mu.Unlock()
	return len(c.rendezvous)
}

// armFromManifest arms the rendezvous from the intent manifest as it stands
// NOW, adding what is not already armed and touching nothing that is.
func (c *controller) armFromManifest() error {
	m, found, err := ReadManifest(c.deps.IntentManifest)
	if err != nil || !found {
		return err
	}
	c.recordManifestSeen()
	c.recordForcedTakeover(m.Forced)
	c.recordDeployTakeover(m.Deploy)
	added := c.armSessions(m.Daemon, m.Sessions)
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
		// THE MANIFEST MAY NOT HAVE ARRIVED YET. The incumbent writes it only
		// after the successor has reported its address, so a successor that
		// armed nothing at boot is the ordinary case, not a refusal: it
		// re-reads here, when a participant actually calls, and WAITS for the
		// manifest that has not landed.
		c.mu.Unlock()
		if err := c.awaitArm(ctx, ws, operation, fields); err != nil {
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
	before := dlog.Context{"host_called": e.hostCalled, "web_called": e.webCalled}
	fill(e)
	after := dlog.Context{"host_called": e.hostCalled, "web_called": e.webCalled}
	if !e.satisfied() {
		outstanding := dlog.Context{
			"expected_host": e.expected.Host, "expected_web": e.expected.Web,
			"host_called": e.hostCalled, "web_called": e.webCalled,
		}
		done := e.done
		c.mu.Unlock()
		c.logTransition(operation, ws, "participants_called", before, after,
			dlog.Context{"rendezvous_satisfied": false})
		c.log.Debug(operation, "an expected participant has not called yet; waiting for the rendezvous",
			merge(fields, outstanding))
		return c.awaitRendezvous(ctx, e, done, operation, fields)
	}
	if e.adopting {
		// THE ADOPTION IS ALREADY RUNNING on another caller's behalf. It is
		// run exactly once per rendezvous; this caller shares its outcome.
		done := e.done
		c.mu.Unlock()
		c.log.Debug(operation, "the rendezvous is satisfied and its adoption is already running; waiting on it", fields)
		return c.awaitRendezvous(ctx, e, done, operation, fields)
	}
	e.adopting = true
	c.mu.Unlock()
	c.logTransition(operation, ws, "participants_called", before, after,
		dlog.Context{"rendezvous_satisfied": true})

	// THE ADOPTION IS THE DAEMON'S WORK, NOT THIS CALLER'S, so it runs on a
	// context the caller cannot cancel. The rendezvous is satisfied by the
	// line above: every expected participant has called, and the workspace is
	// being taken over on behalf of all of them. Whether the ONE connection
	// that happened to complete it is still listening has nothing to do with
	// whether the takeover finishes.
	//
	// THE COST OF LETTING IT BE CANCELLED IS THE WHOLE ADOPTION WINDOW, and
	// that is the defect this replaces. Emacs gives a unary rpc
	// `agent-repl-connect-unary-timeout-seconds' (10s, `lisp/connect.el') and
	// its adopt is an ordinary unary call; the steps below are a state-handle
	// promotion and four writes against a SQLite handle the OUTGOING daemon is
	// still writing, whose busy timeout alone is 5s apiece. On a loaded box the
	// host's call can therefore expire mid-`adopt' -- and cancelling the
	// adoption there left serving ownership unclaimed, settled the rendezvous
	// FAILED (`e.settle'), and left nothing to try again with: Emacs does not
	// re-adopt after a transport failure, and `retryHeadless' covers only
	// workspaces with no participants. The incumbent then waited out its whole
	// `DefaultAdoptionWindow' (30s) for an adoption that had all but finished,
	// which delays `windows.Wait()', which delays its exit, which delays the
	// primary stream close Emacs promotes the successor on
	// (`lisp/daemon-link.el', `agent-repl-link--handle-close'). That is
	// `TestEmacsHandoverTransfersAtFreeness' missing its 21s bound on a
	// promotion healthy runs make in ~2s.
	//
	// `advertise' below already detaches for the same reason; this is the same
	// rule one step earlier.
	err := c.adopt(context.WithoutCancel(ctx), ws, operation)
	c.mu.Lock()
	e.settle(err)
	// A FAILED ADOPTION MAY BE TRIED AGAIN by a later caller, as before: only
	// a CONCURRENT second run is what the latch forbids.
	if err != nil {
		e.adopting = false
	}
	c.mu.Unlock()
	if err != nil {
		return err
	}
	c.log.Info(operation, "every expected participant called; the workspace is adopted", fields)
	return nil
}

// awaitRendezvous waits for a rendezvous's one adoption to settle, and shares
// its outcome with this caller.
//
// EVERY EXPECTED PARTICIPANT SUCCEEDS TOGETHER. The callers arrive
// concurrently and the one that arrives first has not failed: it waits for the
// one that completes the rendezvous. `not_yet_adopted` on an ADOPT call means
// only that this caller's own context expired first, which is the
// retry-with-backoff case.
func (c *controller) awaitRendezvous(ctx context.Context, e *entry, done <-chan struct{}, operation string, fields dlog.Context) error {
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
	// AN ADOPTION IN FLIGHT IS NEVER STARTED AGAIN for the same workspace: the
	// takeover's straggler pass skips whatever is marked here, so a rendezvous
	// still waiting on the serving release is left to finish on its own.
	c.mu.Lock()
	if c.adopting == nil {
		c.adopting = map[ids.WorkspaceID]bool{}
	}
	c.adopting[ws] = true
	c.mu.Unlock()
	defer func() {
		c.mu.Lock()
		delete(c.adopting, ws)
		c.mu.Unlock()
	}()
	outgoing, err := c.outgoingDaemon(ws)
	if err != nil {
		c.log.Error(opAdopt, "could not resolve the incumbent serving owner", withCause(fields, err))
		return err
	}
	fields["outgoing_daemon"] = string(outgoing)
	if err := c.awaitServingRelease(ctx, ws, outgoing, fields); err != nil {
		return err
	}
	// A REFUSED MID-WORK ADOPTION IS THE INCUMBENT'S TO TAKE BACK, and its
	// take-back retires the marker. Until then the released row is the one
	// the incumbent is about to claim, and no attempt here may race it.
	if c.refusedMidWork(ws) {
		c.log.Info(opAdopt, "this daemon refused the workspace's mid-work adoption and the incumbent has not taken it back yet; not adopting", fields)
		return fmt.Errorf("rollout: adopt %q: %w", ws, ErrMidWorkRefused)
	}

	// THE HANDLE BECOMES A WRITING ONE HERE. A successor opens read-only
	// because the incumbent is still the sole writer; adopting a workspace is
	// the moment it starts writing that workspace's rows. The serving release
	// above is the durable proof that the incumbent already passed freeness,
	// quiesced the intake and stopped writing this workspace.
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

	// THE CARRY IS READ BEFORE THE CLAIM: a mid-work adoption this daemon
	// cannot make safely is refused with the row still unowned, which is the
	// row the incumbent's take-back claims.
	carry, carried, err := c.takeCarry(ws, outgoing, fields)
	if err != nil {
		return err
	}
	if carried && carry.MidWork && dial {
		if ok, why := midWorkCompatible(carry); !ok {
			return c.refuseMidWork(ws, why, fields)
		}
	}
	var parked *conversationv1.SessionCold
	if carried && len(carry.ColdGate) > 0 {
		parked, err = decodeCold(carry.ColdGate)
		if err != nil {
			c.log.Error(opAdopt, "could not decode the carried cold gate; the workspace is not adopted", withCause(fields, err))
			return fmt.Errorf("rollout: adopt %q: decode the carried cold gate: %w", ws, err)
		}
	}

	// THE ROW IS CLAIMED BEFORE THE SHIM IS DIALED, through the one
	// arbitration the incumbent's reclaim also goes through. An incumbent
	// whose adoption window expired takes the workspace back by the same
	// claim, and exactly one of the two may dial the shim: two daemons each
	// holding a client for one shim is the state this ordering makes
	// impossible.
	claimed, holder, err := c.deps.DB.ClaimUnownedServing(ctx, ws, c.deps.Instance)
	if err != nil {
		c.log.Error(opAdopt, "could not claim serving ownership", withCause(fields, err))
		return fmt.Errorf("rollout: adopt %q: claim serving: %w", ws, err)
	}
	if !claimed {
		err := fmt.Errorf("rollout: adopt %q: the incumbent %s took the workspace back: %w", ws, holder, ErrReclaimed)
		c.log.Error(opAdopt, "the incumbent took the workspace back before this adoption claimed it; it is not adopted here",
			withCause(merge(fields, dlog.Context{"serving_daemon": string(holder)}), err))
		return err
	}
	// FROM THE CLAIM ON, THE HANDOVER HOLD IS THIS DAEMON'S TO DRAIN, and it
	// is drained whether or not the shim answers: a workspace this daemon
	// serves with the incumbent's hold still standing refuses every prompt
	// for as long as the hold stands. A shim that will not answer leaves the
	// workspace served without a session, which its next prompt revives.
	var dialErr error
	if dial {
		dialErr = c.dialAdopted(ctx, ws, parked, fields)
	}
	// A MID-WORK ADOPTION LETS NO HELD PROMPT GO BEFORE IT KNOWS THE TURN IN
	// FLIGHT: until the adopted shim's re-announcement is taken up, the
	// watcher answers no turn for a turn the shim is running, and the drain
	// below would start a second turn beside it. A shim that never
	// re-announces is refused, and the incumbent moves it at freeness.
	if dial && dialErr == nil && carried && carry.MidWork && parked == nil {
		facts, cancel := context.WithTimeout(ctx, c.deps.FactsBound)
		err := c.deps.Shims.AwaitFacts(facts, ws)
		cancel()
		if err != nil {
			c.log.Info(opAdopt, "the adopted shim never re-announced its session facts; the mid-work adoption is refused",
				withCause(merge(fields, dlog.Context{"facts_bound": c.deps.FactsBound.String()}), err))
			return c.refuseAdopted(ctx, ws, "the adopted shim never re-announced its session facts within "+c.deps.FactsBound.String(), fields)
		}
	}
	// THE CARRIED QUEUE MEMORY IS INSTALLED BEFORE THE HOLD IS DRAINED: the
	// drain is what runs the carried acts ahead of the held prompts.
	var carryErr error
	if carried {
		if err := c.deps.Bounces.AdoptHandoff(ctx, ws, carry.Queue); err != nil {
			c.log.Error(opAdopt, "could not install the carried queue memory; the held intake is drained all the same", withCause(fields, err))
			carryErr = fmt.Errorf("rollout: adopt %q: install the carried queue memory: %w", ws, err)
		}
	}
	if err := c.deps.DrainIntake(ctx, ws); err != nil {
		c.log.Error(opAdopt, "could not drain the held intake", withCause(fields, err))
		return errors.Join(dialErr, carryErr, c.consumeCarry(ws, carried),
			fmt.Errorf("rollout: adopt %q: drain the held intake: %w", ws, err))
	}
	if carried && dialErr == nil && carryErr == nil {
		if err := c.deps.Bounces.RejudgeHeld(ctx, ws); err != nil {
			c.log.Error(opAdopt, "could not re-judge the held prompts whose verdicts the move superseded", withCause(fields, err))
			carryErr = fmt.Errorf("rollout: adopt %q: re-judge the held prompts: %w", ws, err)
		}
	}
	// THE CARRY IS CONSUMED ON EVERY PATH PAST THE DRAIN: its removal is what
	// tells the incumbent a mid-work adoption landed rather than was refused.
	if err := errors.Join(dialErr, carryErr, c.consumeCarry(ws, carried)); err != nil {
		return err
	}
	// A WORKSPACE ADOPTED WITH NO SHIM BEHIND IT HAS ITS SESSION STARTED
	// (sessionless.go). The marker is raised here, before the views are
	// published, so the roster never draws the adopted row idle and usable
	// while nothing serves it; the start itself runs once the adoption is
	// complete and the workspace is this daemon's.
	sessionless := !dial && !record.Closed
	if sessionless {
		c.deps.BringingUp(ws, true)
	}
	if err := c.deps.PublishViews(ctx, ws); err != nil {
		c.log.Error(opAdopt, "could not publish the workspace's fresh views", withCause(fields, err))
		if sessionless {
			// The start this marker announced will not run: the adoption
			// failed, and the straggler pass or the incumbent decides what
			// serves the workspace next.
			c.deps.BringingUp(ws, false)
		}
		return fmt.Errorf("rollout: adopt %q: publish views: %w", ws, err)
	}

	c.mu.Lock()
	wasOwned := c.owned[ws]
	if e, ok := c.rendezvous[ws]; ok {
		e.adopted = true
	}
	if c.owned == nil {
		c.owned = map[ids.WorkspaceID]bool{}
	}
	c.owned[ws] = true
	// ONLY A JOINING DAEMON ADVERTISES FROM HERE: once it has taken over
	// (becomeIncumbent), daemon.addr is already written and a late adoption
	// finishing must not write it a second time.
	complete := c.joiningMode && len(c.joining) > 0
	for joined := range c.joining {
		if !c.owned[joined] {
			complete = false
			break
		}
	}
	c.mu.Unlock()
	c.logTransition(opAdopt, ws, "owned", wasOwned, true,
		dlog.Context{"all_joining_owned": complete})

	c.log.Info(opAdopt, "adopted the workspace", fields)

	if sessionless {
		c.startSessionless([]wsm.Workspace{record}, fields)
	}

	// A DIALED ADOPTION IS A HEALTHY ATTACH, the recovery edge of every
	// standing fault whose lifetime ends there (health/lifetime.go) -- the
	// bounce dispositions flushed at the promotion above among them.
	if dial {
		workspace := ws
		health.CloseOnEdge(ctx, c.deps.DB, c.log.With(fields), health.EdgeHealthyAttach,
			health.EdgeScope{Workspace: &workspace}, c.deps.Clock.Now())
	}

	// THE REPLACEMENTS THE MOVE CARRIED RUN HERE, after the adoption (owner
	// ruling, 2026-09-27: a restart racing a running move runs on the daemon
	// the workspace moved to). Each goes through this daemon's own registry,
	// at this daemon's freeness or at once when it was forced.
	if carried {
		c.runCarried(ctx, ws, carry.Replacements, fields)
	}

	// THE ADOPTED SHIM IS JUDGED NOW THAT IT IS OURS. Its build report arrived
	// when the adoption dialed it, which was before this daemon owned the
	// workspace and so before it could bounce it.
	if _, err := c.checkStale(ctx, ws, c.takeoverForce(ws)); err != nil {
		c.log.Error(opStaleness, "the adopted shim could not be judged against the installed build", withCause(fields, err))
	}

	if complete && c.deps.WriteDaemonAddr != nil {
		// A JOINING DAEMON OTHERWISE NEVER WRITES daemon.addr: until every
		// workspace is its own, the incumbent's file is still the truth.
		c.advertise(ctx, fields)
	}
	return nil
}

// recordForcedTakeover latches that the handover this successor joins was
// FORCED, so the stale shims it adopts are bounced at once rather than
// registered behind their work.
func (c *controller) recordForcedTakeover(forced bool) {
	if !forced {
		return
	}
	c.mu.Lock()
	before := c.forcedTakeover
	c.forcedTakeover = true
	c.mu.Unlock()
	if !before {
		c.log.Info(opJoin, "the handover being joined was forced; stale adopted shims are bounced at once", nil)
	}
}

// recordManifestSeen latches that the incumbent's intent manifest has been
// read, so the transfer set is known from here on.
func (c *controller) recordManifestSeen() {
	c.mu.Lock()
	first := !c.manifestSeen
	c.manifestSeen = true
	c.mu.Unlock()
	if first {
		c.log.Debug(opJoin, "rollout state changed", dlog.Context{
			"state": "manifest_seen", "before": false, "after": true,
			"path": c.deps.IntentManifest,
		})
	}
}

// awaitArm holds a participant's adopt call until the incumbent's intent
// manifest arrives and arms this workspace's rendezvous.
//
// THE ANNOUNCEMENT COMES BEFORE THE MANIFEST. `Handover` spawns the successor,
// announces its address, snapshots the participants and only THEN writes the
// manifest — so a participant that dials the announced address the moment it
// hears it can reach a successor that has been told nothing at all. Answering
// that call `no_transfer_announced` loses the transfer outright: Emacs does not
// re-adopt after a refusal, so the incumbent then waits out its entire
// AdoptionWindow for a workspace nobody is going to adopt, which delays its
// exit and with it the primary stream close Emacs promotes the successor on.
// That is `TestASuccessorDoesNotAdoptABusyWorkspaceBeforeTheIncumbentTransfersIt`
// failing under host contention with `no_transfer_announced` and passing alone.
//
// SO THE CALL WAITS, exactly the way adoption waits for the incumbent's
// durable serving release one step later (`awaitServingRelease`).
//
// IT IS BOUNDED BY THE HANDOVER'S OWN WINDOW. AdoptionWindow is how long the
// incumbent gives an adoption; a manifest that has not arrived by then belongs
// to no handover this call can join, and the refusal stands.
//
// IT ENDS THE MOMENT THE MANIFEST IS IN HAND, armed or not. A manifest that
// arrived and does not name this workspace is a genuine no_transfer_announced
// — which is the ORDINARY answer on every non-handover web page boot, and must
// stay immediate.
//
// Returning nil does not mean armed: the caller re-reads the ledger and
// answers the refusal itself, so the two arms keep one record between them.
func (c *controller) awaitArm(ctx context.Context, ws ids.WorkspaceID, operation string, fields dlog.Context) error {
	// THE BOUND IS ARMED ONLY IF THIS CALL ACTUALLY WAITS. The overwhelmingly
	// common path — a manifest already in hand, or none because no handover is
	// in flight — answers on the first read and must cost nothing.
	var bound <-chan time.Time
	waiting := false
	for {
		if err := c.armFromManifest(); err != nil {
			c.log.Error(operation, "could not re-read the intent manifest", withCause(fields, err))
			return err
		}
		c.mu.Lock()
		_, armed := c.rendezvous[ws]
		seen := c.manifestSeen
		c.mu.Unlock()
		if armed {
			if waiting {
				c.log.Info(operation, "the intent manifest arrived and armed this workspace; the held adopt call resumes", fields)
			}
			return nil
		}
		if seen {
			// The transfer set is known and this workspace is not in it.
			return nil
		}
		if !waiting {
			c.log.Info(operation, "no intent manifest yet; holding this adopt call until the incumbent writes one",
				merge(fields, dlog.Context{
					"poll_interval": manifestPoll.String(),
					"bound":         c.deps.AdoptionWindow.String(),
				}))
			waiting = true
			bound = c.deps.Clock.After(c.deps.AdoptionWindow)
		}
		select {
		case <-ctx.Done():
			c.log.Debug(operation, "the caller gave up before the intent manifest arrived", fields)
			return ErrNotYetAdopted
		case <-bound:
			// INFO, NOT WARN. This is the same refusal the caller is about to
			// record, reached the slow way; the ruling on `no_transfer_announced`
			// is that it is never a warning and never a fault.
			c.log.Info(operation, "no intent manifest arrived within the adoption window; nothing was announced for this workspace",
				merge(fields, dlog.Context{"bound": c.deps.AdoptionWindow.String()}))
			return nil
		case <-c.deps.Clock.After(manifestPoll):
		}
	}
}

// outgoingDaemon returns the incumbent recorded on a workspace's armed
// rendezvous. Adoption without that identity cannot distinguish the intended
// predecessor from an unrelated serving owner, so it is an invariant error.
func (c *controller) outgoingDaemon(ws ids.WorkspaceID) (ids.InstanceID, error) {
	c.mu.Lock()
	defer c.mu.Unlock()
	e, ok := c.rendezvous[ws]
	if !ok {
		return "", fmt.Errorf("rollout: adopt %q: no rendezvous is armed", ws)
	}
	if e.outgoing == "" {
		return "", fmt.Errorf("rollout: adopt %q: the intent manifest names no outgoing daemon", ws)
	}
	return e.outgoing, nil
}

// awaitServingRelease holds adoption behind the incumbent's durable freeness
// edge. The successor may be asked to adopt as soon as the shutdown
// announcement arrives, while the incumbent is still waiting for the turn to
// finish. Claiming before the serving row clears lets the successor race the
// incumbent's release and can strand every transfer notice behind that error.
func (c *controller) awaitServingRelease(ctx context.Context, ws ids.WorkspaceID, outgoing ids.InstanceID, fields dlog.Context) error {
	waiting := false
	for {
		owner, err := c.deps.DB.Serving(ctx, ws)
		if err != nil {
			c.log.Error(opAdopt, "could not read serving ownership while waiting for the incumbent", withCause(fields, err))
			return fmt.Errorf("rollout: adopt %q: read serving ownership: %w", ws, err)
		}
		if owner == nil {
			c.log.Debug(opAdopt, "the incumbent released serving ownership; adoption may begin",
				merge(fields, dlog.Context{"serving_daemon": ""}))
			return nil
		}
		if *owner == c.deps.Instance {
			c.log.Debug(opAdopt, "this daemon already owns the workspace; adoption may resume",
				merge(fields, dlog.Context{"serving_daemon": string(*owner)}))
			return nil
		}
		if *owner != outgoing {
			err := fmt.Errorf("rollout: adopt %q: workspace is served by unexpected daemon %q, want incumbent %q", ws, *owner, outgoing)
			c.log.Error(opAdopt, "an unexpected daemon owns the workspace at the adoption boundary", withCause(merge(fields, dlog.Context{"serving_daemon": string(*owner)}), err))
			return err
		}
		if !waiting {
			c.log.Info(opAdopt, "waiting for the incumbent to release serving ownership at freeness",
				merge(fields, dlog.Context{
					"serving_daemon": string(*owner),
					"poll_interval":  manifestPoll.String(),
				}))
			waiting = true
		}
		select {
		case <-ctx.Done():
			err := context.Cause(ctx)
			c.log.Error(opAdopt, "the adoption lifetime ended before the incumbent released serving ownership", withCause(fields, err))
			return fmt.Errorf("rollout: adopt %q: wait for incumbent serving release: %w", ws, err)
		case <-c.deps.Clock.After(manifestPoll):
		}
	}
}

// advertiseBackoffCeiling caps the wait between two attempts to take the boot
// claim and write daemon.addr. The first retry waits manifestPoll and each one
// after doubles it. The claim is released by the incumbent's own exit, bounded
// by daemonaddr.ClaimWaitBound (2.32s), so an ordinary handover spends a
// handful of attempts here and a claim that is never released costs one
// attempt a second rather than forty.
const advertiseBackoffCeiling = time.Second

// advertiseRefusedBound is how long the claim may stay refused before the
// successor says so at ERROR: well past daemonaddr.ClaimWaitBound, the
// longest an orderly incumbent holds the claim after its last transfer.
const advertiseRefusedBound = 30 * time.Second

// advertiseDelay is the wait before retry n (n >= 1): manifestPoll doubled
// n-1 times, capped at advertiseBackoffCeiling.
func advertiseDelay(n int) time.Duration {
	delay := manifestPoll
	for i := 1; i < n; i++ {
		if delay >= advertiseBackoffCeiling/2 {
			return advertiseBackoffCeiling
		}
		delay *= 2
	}
	if delay > advertiseBackoffCeiling {
		return advertiseBackoffCeiling
	}
	return delay
}

// advertise writes daemon.addr once every workspace is owned, retrying while
// the OUTGOING daemon still holds the boot claim.
//
// THE ADVERTISEMENT IS NOT PART OF THE ADOPTION. The incumbent releases its
// claim when it exits, and it exits when the adoptions complete — so failing an
// adoption because the claim is still held deadlocks the handover on itself.
// The workspace is adopted either way; only the address file waits.
//
// ONLY A HELD CLAIM IS WAITED ON, AND WITH A BOUNDED BACKOFF. Any other
// failure -- a state root that is gone, a directory that cannot be written --
// is not the incumbent departing and will not fix itself, so it is an ERROR
// and the retry stops. It once retried every failure every 25ms forever.
func (c *controller) advertise(ctx context.Context, fields dlog.Context) {
	err := c.deps.WriteDaemonAddr(ctx)
	if err == nil {
		c.log.Info(opAdopt, "every workspace is owned; wrote daemon.addr", nil)
		c.becomeIncumbent(fields)
		return
	}
	if !errors.Is(err, daemonaddr.ErrClaimed) {
		c.log.Error(opAdopt, "daemon.addr could not be written; not retrying a failure that is not a held boot claim", withCause(fields, err))
		return
	}
	c.log.Debug(opAdopt, "the outgoing daemon still holds the boot claim; advertising once it lets go", withCause(fields, err))
	// THE RETRY OUTLIVES THE CALL. The context here is an rpc's, cancelled the
	// moment the adopt answers — and the claim is released by the incumbent's
	// exit, which happens after that answer. The daemon's own lifetime still
	// ends it.
	lifetime := c.deps.Lifetime
	if lifetime == nil {
		lifetime = context.WithoutCancel(ctx)
	}
	go c.retryAdvertise(lifetime, fields)
}

// becomeIncumbent ends this daemon's JOINING standing: it is now the only
// daemon there is.
//
// THE PROOF IS THE ADDRESS FILE. daemon.addr is written only once the outgoing
// daemon has released its boot claim, and it releases the claim by exiting. So
// from the moment the write succeeds no other daemon serves ANY workspace, and
// a workspace this daemon was never handed — a closed one, a forgotten one,
// one the outgoing daemon held but could not transfer — is simply this
// daemon's. Left joining, the successor answered `not_yet_adopted` for every
// such workspace FOREVER: a closed workspace could never be reopened after a
// rollout, and the sidecar's forwarded diagnostics for it were refused in a
// loop that never ended.
//
// A workspace still mid-rendezvous stays governed by its own `joining` entry:
// its adoption is a separate, per-workspace fact this does not overrule.
func (c *controller) becomeIncumbent(fields dlog.Context) {
	c.mu.Lock()
	was := c.joiningMode
	c.joiningMode = false
	signal := c.tookOverSignalLocked()
	c.mu.Unlock()
	defer func() {
		select {
		case <-signal:
		default:
			close(signal)
		}
	}()
	if !was {
		return
	}
	c.log.Info(opAdopt, "the outgoing daemon is gone; this daemon now serves every workspace it was not handed",
		merge(fields, dlog.Context{"state": "joining_mode", "before": true, "after": false}))
	// THE HANDLE BECOMES A WRITING ONE AT THE TAKEOVER, whatever was handed
	// over. It used to be promoted only inside a per-workspace adoption, so a
	// successor handed NOTHING -- an incumbent whose handover listed no
	// workspace -- took over on the joining-mode read-only handle and kept it
	// for its whole life: every write it then made was refused, the first of
	// them the takeover's own orphan claims (live deploy 2026-09-24 15:07,
	// successor pid 80861, `wsm: handle is read-only` x3). The takeover is
	// the proof the outgoing daemon has stopped writing (it released its boot
	// claim by exiting), so this is the moment the one-writer invariant moves.
	promoted := c.promoteAtTakeover(fields)
	c.adoptStragglers(fields)
	if promoted {
		c.recoverOrphans(fields)
	}
	c.bounceStaleAdopted(fields)
}

// promoteAtTakeover promotes the state handle to writing, reporting whether it
// writes. A refusal is ERROR and the orphan recovery is not attempted: it would
// adopt shims whose serving claims could not be written.
func (c *controller) promoteAtTakeover(fields dlog.Context) bool {
	if err := c.deps.DB.Promote(c.lifetime(context.Background())); err != nil {
		c.log.Error(opAdopt, "the state handle could not be promoted to writing at the takeover; the shims nothing adopted are not recovered", withCause(fields, err))
		return false
	}
	return true
}

// tookOverSignalLocked answers the channel that closes when this daemon takes
// over. Callers hold c.mu.
func (c *controller) tookOverSignalLocked() chan struct{} {
	if c.tookOver == nil {
		c.tookOver = make(chan struct{})
	}
	return c.tookOver
}

// tookOverSignal is tookOverSignalLocked for a caller that does not hold c.mu.
func (c *controller) tookOverSignal() <-chan struct{} {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.tookOverSignalLocked()
}

// adoptStragglers takes every workspace that was being handed over but never
// finished its rendezvous. The incumbent is gone, so nobody else can serve it:
// it is OWNED from this instant (a request for it gets this daemon's answer,
// never not_yet_adopted), and its running shim, if one survived, is adopted
// through the same path a headless workspace takes. An adoption that fails is
// that workspace's own logged error; the workspace stays owned either way.
func (c *controller) adoptStragglers(fields dlog.Context) {
	c.mu.Lock()
	var stragglers []ids.WorkspaceID
	for ws := range c.joining {
		if !c.owned[ws] && !c.adopting[ws] {
			stragglers = append(stragglers, ws)
		}
	}
	if c.owned == nil {
		c.owned = map[ids.WorkspaceID]bool{}
	}
	for _, ws := range stragglers {
		c.owned[ws] = true
	}
	c.mu.Unlock()
	if len(stragglers) == 0 {
		return
	}
	lifetime := c.deps.Lifetime
	if lifetime == nil {
		lifetime = context.Background()
	}
	c.log.Info(opAdopt, "adopting the workspaces whose handover never finished",
		merge(fields, dlog.Context{"workspaces": len(stragglers)}))
	for _, ws := range stragglers {
		c.stragglerAdoptions.Add(1)
		go func(ws ids.WorkspaceID) {
			defer c.stragglerAdoptions.Done()
			if err := c.adopt(lifetime, ws, "straggler"); err != nil {
				c.log.Error(opAdopt, "a workspace whose handover never finished could not have its session adopted; it is served without one",
					withCause(merge(fields, dlog.Context{"workspace": string(ws)}), err))
			}
		}(ws)
	}
}

// bounceStaleAdopted judges every workspace this daemon now serves against the
// installed shim bundle, once it has taken over.
//
// THE HANDOVER'S OTHER HALF. A deploy that rebuilt the daemon and the shim
// hands over, and the successor ADOPTS the running shims — which are still the
// old build. Each adopted shim reports its build the moment it is attached,
// but a report that arrived while this daemon was still joining was not its to
// act on (checkStale refuses a workspace it does not own yet), so the takeover
// re-judges them all. Each stale one goes to the bounce registry, which bounces
// it now or when its work ends; nothing here waits on a workspace.
func (c *controller) bounceStaleAdopted(fields dlog.Context) {
	lifetime := c.lifetime(context.Background())
	workspaces, err := c.deps.DB.ListWorkspaces(lifetime)
	if err != nil {
		c.log.Error(opStaleness, "could not list the workspaces to check for stale shims", withCause(fields, err))
		return
	}
	checked := 0
	deferred := map[ids.WorkspaceID][]deployprogress.Note{}
	for _, ws := range workspaces {
		if _, live := c.deps.Shims.Client(ws.ID); !live {
			continue
		}
		checked++
		check, err := c.checkStale(lifetime, ws.ID, c.takeoverForce(ws.ID))
		if err != nil {
			c.log.Error(opStaleness, "an adopted shim could not be judged against the installed build",
				withCause(merge(fields, dlog.Context{"workspace": string(ws.ID)}), err))
			continue
		}
		if shimDeferred(check) {
			deferred[ws.ID] = append(deferred[ws.ID], deployprogress.ShimWhenIdle)
		}
	}
	c.log.Info(opStaleness, "judged every adopted shim against the installed build",
		merge(fields, dlog.Context{"live_shims": checked}))
	c.finishDeployStory(deferred, fields)
}

// retryAdvertise is advertise's retry loop, on the injected clock.
func (c *controller) retryAdvertise(ctx context.Context, fields dlog.Context) {
	started := c.deps.Clock.Now()
	reported := false
	for attempt := 1; ; attempt++ {
		select {
		case <-ctx.Done():
			c.log.Debug(opAdopt, "the daemon's lifetime ended before daemon.addr could be written", fields)
			return
		case <-c.deps.Clock.After(advertiseDelay(attempt)):
		}
		err := c.deps.WriteDaemonAddr(ctx)
		if err == nil {
			c.log.Info(opAdopt, "every workspace is owned; wrote daemon.addr", merge(fields, dlog.Context{"attempts": attempt + 1}))
			c.becomeIncumbent(fields)
			return
		}
		if !errors.Is(err, daemonaddr.ErrClaimed) {
			c.log.Error(opAdopt, "daemon.addr could not be written; not retrying a failure that is not a held boot claim",
				withCause(merge(fields, dlog.Context{"attempts": attempt + 1}), err))
			return
		}
		if held := c.deps.Clock.Now().Sub(started); !reported && held >= advertiseRefusedBound {
			reported = true
			c.log.Error(opAdopt, "the boot claim is still held long after the outgoing daemon should have exited; still retrying",
				withCause(merge(fields, dlog.Context{"attempts": attempt + 1, "held": held.String()}), err))
		}
	}
}

// awaitIncumbentExit writes daemon.addr the moment the outgoing daemon lets go
// of the boot claim, and then takes over.
//
// The claim is released by the incumbent's EXIT, so a successful write is the
// proof that no other daemon serves anything. It waits as long as the handover
// does — a busy workspace can hold the incumbent for an hour — so unlike
// retryAdvertise it never reports a claim held "too long": a long handover is
// ordinary. It retries on the same capped backoff, and a failure that is not a
// held claim ends it loudly.
func (c *controller) awaitIncumbentExit(ctx context.Context) {
	for attempt := 1; ; attempt++ {
		select {
		case <-ctx.Done():
			c.log.Debug(opAdopt, "the daemon's lifetime ended before the incumbent exited", nil)
			return
		case <-c.deps.Clock.After(incumbentExitDelay(attempt)):
		}
		if c.deps.WriteDaemonAddr == nil {
			return
		}
		err := c.deps.WriteDaemonAddr(ctx)
		if err == nil {
			c.log.Info(opAdopt, "the incumbent exited; wrote daemon.addr", dlog.Context{"attempts": attempt})
			c.becomeIncumbent(nil)
			return
		}
		if !errors.Is(err, daemonaddr.ErrClaimed) {
			c.log.Error(opAdopt, "daemon.addr could not be written; not retrying a failure that is not a held boot claim",
				withCause(dlog.Context{"attempts": attempt}, err))
			return
		}
	}
}

// The exit watcher's backoff. Its OWN cadence, slower than the adoption
// polls: an incumbent's exit is a rare edge that may be an hour away, and a
// successor has nothing to gain from asking every 25ms.
const (
	incumbentExitPollInitial = 150 * time.Millisecond
	incumbentExitPollCeiling = 2400 * time.Millisecond
)

// incumbentExitDelay is the wait before the watcher's nth look.
func incumbentExitDelay(n int) time.Duration {
	delay := incumbentExitPollInitial
	for i := 1; i < n && delay < incumbentExitPollCeiling; i++ {
		delay *= 2
	}
	if delay > incumbentExitPollCeiling {
		return incumbentExitPollCeiling
	}
	return delay
}

// releaseHeadless undoes a headless claim whose adoption failed.
func (c *controller) releaseHeadless(ws ids.WorkspaceID) {
	c.mu.Lock()
	changed := false
	if e, ok := c.rendezvous[ws]; ok {
		changed = e.headlessClaimed
		e.headlessClaimed = false
	}
	c.mu.Unlock()
	if changed {
		c.logTransition(opJoin, ws, "headless_claimed", true, false, nil)
	}
}
