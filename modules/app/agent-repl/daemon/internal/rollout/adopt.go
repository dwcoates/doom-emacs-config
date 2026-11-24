package rollout

import (
	"context"
	"errors"
	"fmt"
	"path/filepath"
	"sync"
	"time"

	"agentrepl/logging/logsite"
	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/daemonaddr"
	"agent-repld/internal/deployprogress"
	"agent-repld/internal/dlog"
	"agent-repld/internal/health"
	"agent-repld/internal/ids"
	"agent-repld/internal/inflight"
	"agent-repld/internal/kernelevent"
	"agent-repld/internal/restarttiming"
	"agent-repld/internal/rostermirror"
	"agent-repld/internal/sessionlock"
	"agent-repld/internal/sessionwatcher"
	"agent-repld/internal/standdown"
	"agent-repld/internal/wsm"
)

// Join is the JOINING daemon's half of the handover.
//
// It reads the intent manifest (the only thing the outgoing daemon left it —
// there is NO daemon-to-daemon channel for the handover's state: ownership,
// dispositions, carried work and every write go through the manifest and the
// serving row, never a call between the daemons), reconciles every session's disposition
// against the kernel locks, arms the rendezvous from the participants the
// manifest recorded, and starts adopting every HEADLESS workspace without a
// participant call. Every adoption still waits for the incumbent's serving
// release: that durable edge says the workspace passed freeness, its intake is
// quiesced and its shim is detached.
//
// ONE WRITTEN EXCEPTION: THE ROSTER MIRROR (owner ruling, 2026-11-13). While
// joining, this daemon subscribes to the incumbent's public
// WatchWorkspaceRoster stream and draws each workspace it has NOT ADOPTED
// YET from the incumbent's own row (internal/rostermirror,
// startRosterMirror). Its scope is the joining window and the roster's
// status only: it reads, never writes, carries no ownership, and ends when
// the handover does (the incumbent's exit ends the stream and the takeover
// stops it; an abandoned handover stops this daemon). Why: Emacs takes its
// whole roster from the successor during a handover, and the successor's own
// footer state for a workspace the incumbent still serves is frozen, so a
// mid-work holdout drew `idle_async` while the incumbent published
// `thinking` six times (2026-11-13, eval-cache-torch-human). Only the daemon
// serving a workspace can say its status.
func (c *controller) Join(ctx context.Context) error {
	// JOINING MODE IS THE FACT, not the manifest. A successor owns NOTHING
	// until it adopts, and the manifest may not exist yet when it boots: the
	// incumbent writes it only after the successor has reported its address.
	// Until then every per-workspace rpc must answer not_yet_adopted rather
	// than fall through to a read-only state handle.
	c.mu.Lock()
	c.enterJoiningLocked()
	c.unlock()

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
	inflight.Standing(func() { c.awaitIncumbentExit(lifetime) })
	c.startRosterMirror(lifetime)
	// THE CARRY FOLLOWER'S WATCH IS TAKEN BEFORE THE MANIFEST IS READ below,
	// so a transfer that re-states a workspace after that read is reported to
	// it, and the read itself is the follower's first look. THE FOLLOWER
	// STARTS ONLY ONCE THE READ HAS ADOPTED what it armed: started before, a
	// change the read itself committed woke it to claim a headless workspace
	// ahead of this read, and adopt it off this call.
	carries, stopCarries := c.handoverChanges(lifetime)
	found, err := c.joinFromManifest(ctx)
	if err != nil {
		stopCarries()
		return err
	}
	inflight.Standing(func() { c.followCarries(lifetime, carries, stopCarries) })
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
		inflight.Standing(func() { c.awaitManifest(ctx) })
	}
	return nil
}

// handoverChanges reports every change a handover's waits read: the intent
// manifest written or removed, a carry or a refusal sealed or retired, and
// every commit to the state database (serving ownership, the handed hold),
// whichever daemon made it. Each wait checks its condition after this returns
// and again at every report, so nothing a handover waits on is ever looked
// for on a timer.
//
// The intent directory's parent, the state root, is followed too: before the
// first handover neither the intent directory nor the carries exist, and the
// root's change is the intent directory being made.
func (c *controller) handoverChanges(ctx context.Context) (<-chan error, func()) {
	intent := filepath.Dir(c.deps.IntentManifest)
	return kernelevent.Merge(ctx, kernelevent.FollowPaths(filepath.Dir(intent), intent, c.carryDir()), c.deps.DB.Changes)
}

// awaitManifest waits for the intent manifest until it lands or the daemon
// stops, joining from it at once.
func (c *controller) awaitManifest(ctx context.Context) {
	changes, stop := c.handoverChanges(ctx)
	defer stop()
	for {
		found, err := c.joinFromManifest(ctx)
		if err != nil {
			c.log.Error(opJoin, "the intent manifest could not be joined from",
				dlog.WithCause(dlog.Context{"path": c.deps.IntentManifest}, err))
			return
		}
		if found {
			return
		}
		select {
		case <-ctx.Done():
			return
		case err := <-changes:
			if err != nil {
				c.log.Error(opJoin, "could not watch for the intent manifest; this daemon does not join until it restarts",
					dlog.WithCause(dlog.Context{"path": c.deps.IntentManifest}, err))
				return
			}
		}
	}
}

// joinFromManifest arms the rendezvous from the intent manifest and adopts
// every headless workspace. It reports whether a manifest was there to read.
func (c *controller) joinFromManifest(ctx context.Context) (bool, error) {
	m, found, err := ReadManifest(c.deps.IntentManifest)
	if err != nil {
		c.log.Error(opJoin, "could not read the intent manifest",
			dlog.WithCause(dlog.Context{"path": c.deps.IntentManifest}, err))
		return false, err
	}
	if !found {
		return false, nil
	}
	c.recordForcedTakeover(m.Forced)
	c.recordDeployTakeover(m)
	// A JOINING SUCCESSOR ADOPTED NOTHING AT BOOT — the manifest is present
	// here by construction, so the no-manifest accounting cannot apply.
	_, _, landed, err := c.reconcile(ctx, Survivors{})
	if err != nil {
		return false, err
	}

	armed := c.armSessions(m.Daemon, m.WrittenAt, m.Sessions)
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
	// whole adoption window for it. claimArmed hands back every armed
	// headless workspace this daemon has not already claimed, so exactly one
	// read adopts each of them.
	claimed := c.claimArmed()

	c.log.Info(opJoin, "armed the adopt rendezvous from the intent manifest", dlog.Context{
		"outgoing_daemon": string(m.Daemon),
		"workspaces":      armed,
		"armed_total":     c.rendezvousSize(),
		"claimed":         len(claimed),
	})

	c.adoptClaimed(ctx, claimed)
	return true, nil
}

// adoptClaimed adopts each claimed workspace, releasing the claim of one that
// fails and retrying it.
//
// THE DAEMON ADOPTS; NO PARTICIPANT DOES. Every workspace the manifest arms is
// claimed here, whatever participants it names, and adopted as soon as the
// incumbent releases its serving row. An editor or a page that calls in only
// waits for that adoption's outcome (rendezvousCall). Before this the
// adoption of a workspace with participants ran only once every expected
// participant had called, so one participant that never came held the whole
// handover: on 2026-10-30 torch-human-support-ceac's page kept calling
// AdoptWebWorkspace on the OUTGOING daemon, the successor waited on its web
// slot for 25 minutes, every adoption window expired, the incumbent took the
// workspace back and offered it again six times, and neither daemon could
// finish -- the incumbent never exited and the successor stayed joining.
//
// EVERY WORKSPACE IS ADOPTED ON ITS OWN GOROUTINE: each serving release comes
// at that workspace's own freeness, and each adoption ends by taking the
// workspace's merges, so adopting one after another holds every workspace
// behind the slowest before it. Headless workspaces were adopted in line, on
// the belief that their rows were released before the manifest named them;
// a deploy releases each row at its transfer, and on 2026-11-14 16:10 the
// line waited on a busy workspace and then on 37 merge adoptions until
// linux-runtime-port and linux-wifi-port, last in it, outlived their 30s
// adoption window and were taken back.
//
// The call still returns once every headless adoption has ended, as the line
// did; a workspace with participants is answered through its rendezvous and
// is not waited on here.
func (c *controller) adoptClaimed(ctx context.Context, claimed []ids.WorkspaceID) {
	// EVERY CLAIMED ADOPTION IS ORDERED BEFORE ANY OF THEM STARTS
	// (adoptorder.go), so no goroutine's scheduling lets a lower rank pass.
	c.order.register(c.ranksOf(claimed))
	var headless sync.WaitGroup
	for _, ws := range claimed {
		if c.ExpectedParticipants(ws) == 0 {
			headless.Add(1)
			inflight.Go(func() {
				defer headless.Done()
				c.adoptOne(ctx, ws)
			})
			continue
		}
		inflight.Go(func() { c.adoptOne(ctx, ws) })
	}
	headless.Wait()
}

// ranksOf answers each of CLAIMED's adoption rank, as its armed entry holds
// it.
func (c *controller) ranksOf(claimed []ids.WorkspaceID) map[ids.WorkspaceID]adoptionRank {
	c.mu.Lock()
	defer c.unlock()
	ranks := make(map[ids.WorkspaceID]adoptionRank, len(claimed))
	for _, ws := range claimed {
		e, armed := c.rendezvous[ws]
		if !armed {
			panic(fmt.Sprintf("rollout: claimed workspace %s has no armed rendezvous", ws))
		}
		ranks[ws] = e.rank
	}
	return ranks
}

// adoptOne adopts one claimed workspace, and on failure releases its claim and
// hands it to the retry.
func (c *controller) adoptOne(ctx context.Context, ws ids.WorkspaceID) {
	if !c.awaitParticipantsForTest(ctx, ws) {
		return
	}
	err := c.driveAdoption(ctx, ws, "daemon")
	if err == nil {
		return
	}
	// A workspace that will not adopt is that workspace's own failure, not
	// the join's: the rest still transfer. A MID-WORK REFUSAL IS NO FAILURE
	// (ErrMidWorkRefused): the successor declined a mid-work adoption it
	// cannot make safely, and the incumbent takes the workspace back and
	// transfers it again at freeness, by design. Whether this goroutine or a
	// participant's adopt reaches the refusal first is only timing.
	if errors.Is(err, ErrMidWorkRefused) {
		c.log.Info(opJoin, "the mid-work adoption was refused; the incumbent transfers the workspace again at freeness, and the adoption is retried",
			dlog.WithCause(dlog.Context{"workspace": string(ws)}, err))
	} else {
		c.log.Error(opJoin, "a workspace could not be adopted; retrying",
			dlog.WithCause(dlog.Context{"workspace": string(ws)}, err))
	}
	// THE CLAIM IS RELEASED ON FAILURE so this workspace can be tried again;
	// an adoption that never happened must not look like one that did.
	c.releaseAdoption(ws)
	c.setRetrying(ws, true)
	// AND SOMETHING MUST ACTUALLY TRY AGAIN. Releasing the claim only makes a
	// retry POSSIBLE: `awaitManifest` stops the moment a manifest is read and
	// `armFromManifest` arms without adopting, so a single transient failure
	// here used to strand the workspace on the incumbent, which then waited
	// out its whole AdoptionWindow for it and delayed its exit
	// (`TestEmacsHandoverTransfersAtFreeness' under load, promotion at 32s and
	// 42s against ~2s healthy).
	inflight.Standing(func() { c.retryAdoption(ctx, ws) })
}

// awaitParticipantsForTest is the Deps.AdoptAwaitsParticipants seam: with it
// set, the daemon's adoption of a workspace that names participants waits for
// every one of them to call. It answers false when the lifetime ended first.
// Without the seam it returns at once.
func (c *controller) awaitParticipantsForTest(ctx context.Context, ws ids.WorkspaceID) bool {
	if !c.deps.AdoptAwaitsParticipants {
		return true
	}
	// A HELD ADOPTION HOLDS NO OTHER BACK: it waits on its participants, as
	// one waiting on its release waits on the incumbent.
	c.order.awaiting(ws)
	c.mu.Lock()
	e, armed := c.rendezvous[ws]
	var called chan struct{}
	if armed && e.expected.Count() > 0 {
		called = e.called
	}
	c.unlock()
	if called == nil {
		return true
	}
	c.log.Info(opJoin, "the test-run seam holds the daemon's adoption until the workspace's participants call",
		dlog.Context{"workspace": string(ws), "seam": EnvTestAdoptAwaitsParticipants})
	select {
	case <-called:
		return true
	case <-ctx.Done():
		return false
	}
}

// driveAdoption runs ONE adoption of an armed workspace on the daemon's behalf
// and settles its rendezvous with the outcome, so every participant waiting
// on it is answered. The caller holds the workspace's adoption claim, which
// is what makes it the sole runner.
//
// IT RUNS ON A CONTEXT NO CALLER CAN CANCEL: the adoption is the daemon's
// work, and an editor's 10s unary deadline expiring mid-adoption used to
// leave serving ownership unclaimed and the rendezvous settled failed
// (`lisp/connect.el' `agent-repl-connect-unary-timeout-seconds').
func (c *controller) driveAdoption(ctx context.Context, ws ids.WorkspaceID, source string) error {
	c.mu.Lock()
	e, armed := c.rendezvous[ws]
	if armed && e.adopted {
		c.unlock()
		return nil
	}
	var run *adoptionRun
	if armed {
		e.beginAdoption()
		run = e.run
	}
	c.unlock()
	err := c.adopt(context.WithoutCancel(ctx), ws, source)
	if armed {
		c.mu.Lock()
		e.settle(err)
		if err != nil {
			// A FAILED ADOPTION MAY BE RUN AGAIN by the next claim holder.
			e.adopting = false
		}
		c.unlock()
		close(run.ended)
	}
	return err
}

// The re-adoption backoff. This is a FAILURE path, not a happy one:
// the first retry is soon enough that an ordinary transient costs a small
// fraction of the adoption window, and the cap keeps a workspace that will
// never adopt from turning a 30s window into 1200 log lines.
const (
	adoptRetryInitial = 100 * time.Millisecond
	adoptRetryMax     = 2 * time.Second
)

// retryAdoption keeps trying to adopt one workspace until it adopts, somebody
// else adopts it, or the daemon stops.
//
// IT RE-CLAIMS BEFORE EVERY ATTEMPT, so it can never race a manifest read that
// picked the workspace up in the meantime: `claimArmed` and this both go
// through the same one-at-a-time claim, and whichever takes it is the sole
// adopter of that attempt.
//
// It is driven by the injected Clock, so a test drives the backoff rather than
// waiting it out.
func (c *controller) retryAdoption(ctx context.Context, ws ids.WorkspaceID) {
	defer c.setRetrying(ws, false)
	delay := adoptRetryInitial
	for attempt := 1; ; attempt++ {
		select {
		case <-ctx.Done():
			return
		case <-c.deps.Clock.After(delay):
		}
		if !c.reclaimArmed(ws) {
			// Adopted in the meantime, or no longer armed.
			return
		}
		fields := dlog.Context{"workspace": string(ws), "attempt": attempt}
		if err := c.driveAdoption(ctx, ws, "daemon-retry"); err != nil {
			if errors.Is(err, ErrMidWorkRefused) {
				c.log.Info(opJoin, "the mid-work adoption was refused again; the incumbent transfers the workspace at freeness, and the adoption is retried",
					dlog.WithCause(fields, err))
			} else {
				c.log.Warn(opJoin, "a workspace could not be adopted; retrying",
					dlog.WithCause(fields, err))
			}
			c.releaseAdoption(ws)
			if delay *= 2; delay > adoptRetryMax {
				delay = adoptRetryMax
			}
			continue
		}
		c.log.Info(opJoin, "adopted the workspace on a retry", fields)
		return
	}
}

// reclaimArmed takes the adoption claim for ONE workspace, and reports
// whether this caller now holds it. It answers false for a workspace that has
// since been adopted, is claimed by somebody else, or is no longer armed.
func (c *controller) reclaimArmed(ws ids.WorkspaceID) bool {
	c.mu.Lock()
	e, ok := c.rendezvous[ws]
	if !ok || e.adopted || e.adoptionClaimed {
		c.unlock()
		return false
	}
	e.claim()
	c.unlock()
	c.logTransition(opJoin, ws, "adoption_claimed", false, true, nil)
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
//
// THE MANIFEST IS SEEN IN THE SAME CRITICAL SECTION ITS SESSIONS ARE ARMED IN.
// Seen with nothing armed yet, the transfer set reads as empty: the queue's
// standing (servesQueueLocked) and the joining test (stillJoiningLocked)
// would answer "nothing left to adopt" between the two, so a successor's
// queue flipped from served to held and back once its sessions were armed
// (2026-11-13 23:38:19, each flip suspending the running merges).
func (c *controller) armSessions(outgoing ids.InstanceID, writtenAt time.Time, sessions []ManifestSession) int {
	added := 0
	armed := make([]ManifestSession, 0, len(sessions))
	c.mu.Lock()
	firstSeen := !c.manifestSeen
	c.manifestSeen = true
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
			rank:     rankOf(session),
			called:   make(chan struct{}),
			run:      newAdoptionRun(),
		}
		c.joining[session.Workspace] = true
		armed = append(armed, session)
		added++
	}
	c.unlock()
	if firstSeen {
		c.log.Debug(opJoin, "rollout state changed", dlog.Context{
			"state": "manifest_seen", "before": false, "after": true,
			"path": c.deps.IntentManifest, "armed": added,
		})
	}
	var host, web []ids.WorkspaceID
	for _, session := range armed {
		c.logTransition(opJoin, session.Workspace, "rendezvous", "unarmed", "armed", dlog.Context{
			"expected_host": session.ExpectedHost,
			"expected_web":  session.ExpectedWeb,
		})
		if session.ExpectedHost {
			host = append(host, session.Workspace)
		}
		if session.ExpectedWeb {
			web = append(web, session.Workspace)
		}
	}
	// EACH PARTICIPANT THE OUTGOING DAEMON NAMED IS TO COME BACK HERE, timed
	// from its manifest (restarttiming.ReconnectSuccessor), AS ONE WAVE PER
	// PHASE: a wave per workspace would time each alone, spread always zero.
	c.deps.Reconnects.Expect(restarttiming.ReconnectSuccessor, writtenAt, restarttiming.ReconnectHost, host...)
	c.deps.Reconnects.Expect(restarttiming.ReconnectSuccessor, writtenAt, restarttiming.ReconnectWeb, web...)
	return added
}

// claimArmed takes ownership of every armed workspace that has not been
// claimed or adopted already, WHATEVER PARTICIPANTS IT NAMES, and reports
// them: the daemon adopts each one itself (adoptClaimed). A workspace comes back from it AT MOST ONCE, so two manifest
// reads adopt it once between them, and the caller is the sole adopter of what
// it is handed.
func (c *controller) claimArmed() []ids.WorkspaceID {
	c.mu.Lock()
	claimed := make([]ids.WorkspaceID, 0, len(c.rendezvous))
	for ws, e := range c.rendezvous {
		if e.adopted || e.adoptionClaimed || e.retrying {
			continue
		}
		e.claim()
		claimed = append(claimed, ws)
	}
	c.unlock()
	for _, ws := range claimed {
		c.logTransition(opJoin, ws, "adoption_claimed", false, true, nil)
	}
	return claimed
}

// rendezvousSize reports how many workspaces are armed in total.
func (c *controller) rendezvousSize() int {
	c.mu.Lock()
	defer c.unlock()
	return len(c.rendezvous)
}

// armFromManifest arms the rendezvous from the intent manifest as it stands
// NOW, adding what is not already armed and touching nothing that is.
func (c *controller) armFromManifest() error {
	m, found, err := ReadManifest(c.deps.IntentManifest)
	if err != nil || !found {
		return err
	}
	c.recordForcedTakeover(m.Forced)
	c.recordDeployTakeover(m)
	added := c.armSessions(m.Daemon, m.WrittenAt, m.Sessions)
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

	// THE NEWEST TRANSFER'S PARTICIPANTS, not the announcement's: the carry
	// is written before the release the notice follows, so it is already
	// there when its participant calls (reoffer.go).
	carried, sealedBy, carriedOK := c.carriedParticipants(ws)

	c.mu.Lock()
	// A CALL ON THE DAEMON THAT HANDED THE WORKSPACE AWAY is a lagging
	// participant, and it is pointed onward at once. The outgoing daemon arms
	// an entry of its own at announcement (handover.go) that no participant
	// completes here, so joining it parked the call until the caller's own
	// timeout: on 2026-10-22 a page the web host replayed at the departing
	// daemon's address sat 10s in that entry, failed its boot, and the
	// successor's rendezvous waited the same 10s for a page that never came.
	if successor := c.transferred[ws]; successor != "" {
		c.unlock()
		c.log.Info(operation, "this daemon handed the workspace to a successor; the caller is pointed there",
			dlog.Merge(fields, dlog.Context{"successor": successor}))
		return ErrTransferredAway
	}
	e, armed := c.rendezvous[ws]
	// A CALL ON THE DAEMON THAT IS HANDING THE WORKSPACE OVER, BEFORE ITS
	// TRANSFER, is an ordinary boot here: this daemon still serves the
	// workspace, and the caller hears of the transfer on the streams it opens
	// now. Joining the outgoing daemon's own entry claimed it and ran an
	// adoption of the workspace ON ITS OWN SERVER, which has no incumbent to
	// adopt from: on 2026-11-13 three busy workspaces' pages booted on the
	// incumbent before their transfer, each boot failed with "the intent
	// manifest names no outgoing daemon", and the incumbent retried each
	// self-adoption every 2s until it exited (414 errors).
	if armed && e.servedHere {
		c.unlock()
		c.log.Info(operation, "this daemon still serves the workspace it is handing over; the caller is served here until the transfer, which its streams are told of", fields)
		return ErrNoTransferAnnounced
	}
	if armed && carriedOK {
		c.followCarriedLocked(ws, e, carried, sealedBy)
	}
	if !armed && !c.joiningMode {
		// A SUCCESSOR THAT TOOK OVER BEFORE ITS WATCH READ THE MANIFEST has
		// armed nothing yet: it takes over at the end of the incumbent's
		// transfer pass, and its file watch reads the manifest on its own
		// schedule (2026-11-16, an adopt refused no_transfer_announced 20ms
		// after the takeover). The incumbent writes the manifest before it
		// pushes any transfer notice, so one read here finds every transfer a
		// caller can have heard of; a caller of no transfer reads none.
		c.unlock()
		if err := c.armFromManifest(); err != nil {
			c.log.Error(operation, "could not re-read the intent manifest", dlog.WithCause(fields, err))
			return err
		}
		c.mu.Lock()
		e, armed = c.rendezvous[ws]
	}
	if !armed && c.joiningMode {
		// THE MANIFEST MAY NOT HAVE ARRIVED YET. The incumbent writes it only
		// after the successor has reported its address, so a successor that
		// armed nothing at boot is the ordinary case, not a refusal: it
		// re-reads here, when a participant actually calls, and WAITS for the
		// manifest that has not landed.
		c.unlock()
		if err := c.awaitArm(ctx, ws, operation, fields); err != nil {
			return err
		}
		c.mu.Lock()
		e, armed = c.rendezvous[ws]
	}
	if !armed {
		c.unlock()
		// INFO, not WARN: on the web side this is what a page boot looks like
		// when nothing was handed over, which is almost every boot.
		c.log.Info(operation, "no transfer was announced for this workspace", fields)
		return ErrNoTransferAnnounced
	}
	if e.adopted {
		c.unlock()
		c.log.Debug(operation, "the workspace is already adopted; the call succeeds at once", fields)
		return nil
	}
	if !expects(e) {
		c.unlock()
		// INFO, not WARN: a page Emacs loaded mid-handover is not part of
		// the transfer, and it waits for the expected participants to
		// complete the rendezvous before its adoption is answered at once.
		c.log.Info(operation, "this participant's stream was not open at announcement; it waits for the rendezvous", fields)
		return ErrParticipantNotExpected
	}
	before := dlog.Context{"host_called": e.hostCalled, "web_called": e.webCalled}
	fill(e)
	e.markCalled()
	after := dlog.Context{"host_called": e.hostCalled, "web_called": e.webCalled}
	// A PARTICIPANT NEVER RUNS THE ADOPTION AND NEVER HOLDS IT BACK. The
	// daemon adopts every armed workspace itself (adoptClaimed); a call only
	// records its slot and waits for that adoption's outcome. A workspace no
	// manifest read has claimed yet -- a participant's own re-read armed it
	// first -- is claimed here and handed to the daemon, never run on this
	// caller's context.
	claimNow := !e.adoptionClaimed
	if claimNow {
		// A RE-RUN IS A NEW COMPLETION: an earlier failed run's closed done
		// must not answer this caller with that run's failure.
		e.claim()
	}
	run := e.run
	state := dlog.Context{
		"expected_host": e.expected.Host, "expected_web": e.expected.Web,
		"host_called": e.hostCalled, "web_called": e.webCalled,
		"adopting": e.adopting,
	}
	c.unlock()
	c.logTransition(operation, ws, "participants_called", before, after, nil)
	if claimNow {
		c.logTransition(operation, ws, "adoption_claimed", false, true, nil)
		lifetime := c.lifetime(ctx)
		inflight.Go(func() { c.adoptClaimed(lifetime, []ids.WorkspaceID{ws}) })
	}
	c.log.Info(operation, "a participant called; it waits for the daemon's adoption of the workspace",
		dlog.Merge(fields, state))
	return c.awaitRendezvous(ctx, e, run, operation, fields)
}

// awaitRendezvous waits for a rendezvous's one adoption to settle, and shares
// its outcome with this caller.
//
// EVERY EXPECTED PARTICIPANT SUCCEEDS TOGETHER. The callers arrive
// concurrently and the one that arrives first has not failed: it waits for the
// one that completes the rendezvous. `not_yet_adopted` on an ADOPT call means
// only that this caller's own context expired first, which is the
// retry-with-backoff case.
func (c *controller) awaitRendezvous(ctx context.Context, e *entry, run *adoptionRun, operation string, fields dlog.Context) error {
	select {
	case <-run.done:
		// THIS RUN'S OUTCOME, never the entry's: a re-run that began since
		// replaced e.run and leaves this one as it settled.
		if run.failed != nil {
			return run.failed
		}
		c.log.Debug(operation, "the rendezvous completed while this caller waited", fields)
		return nil
	case <-ctx.Done():
		// INFO, naming who never called: this is where a participant's own
		// timeout ends its wait, and the record says which side it waited on.
		c.mu.Lock()
		outstanding := dlog.Context{
			"expected_host": e.expected.Host, "expected_web": e.expected.Web,
			"host_called": e.hostCalled, "web_called": e.webCalled,
		}
		c.unlock()
		c.log.Info(operation, "the caller gave up before the rendezvous completed", dlog.Merge(fields, outstanding))
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
	probeUntold := logsite.Msg("the workspace lock probe could not tell; adopting on the WSM facts alone")
	fields := dlog.Context{"workspace": string(ws), "source": source}
	// AN ADOPTION IN FLIGHT IS NEVER STARTED AGAIN for the same workspace: the
	// takeover's straggler pass skips whatever is marked here, so a rendezvous
	// still waiting on the serving release is left to finish on its own.
	c.mu.Lock()
	if c.adopting == nil {
		c.adopting = map[ids.WorkspaceID]bool{}
	}
	c.adopting[ws] = true
	c.unlock()
	defer func() {
		c.mu.Lock()
		delete(c.adopting, ws)
		c.unlock()
	}()
	// AN ADOPTION THAT ENDS, BY ANY PATH, HOLDS NO LOWER RANK BACK.
	defer c.order.end(ws)
	outgoing, err := c.outgoingDaemon(ws)
	if err != nil {
		c.log.Error(opAdopt, "could not resolve the incumbent serving owner", dlog.WithCause(fields, err))
		return err
	}
	fields["outgoing_daemon"] = string(outgoing)
	if err := c.awaitServingRelease(ctx, ws, outgoing, fields); err != nil {
		return err
	}
	// THE RELEASED ADOPTION WAITS ITS TURN (adoptorder.go): no adoption of a
	// higher rank is still pending or running ahead of it.
	if err := c.order.admit(ctx, ws); err != nil {
		c.log.Error(opAdopt, "the adoption lifetime ended while the adoption waited its turn", dlog.WithCause(fields, err))
		return fmt.Errorf("rollout: adopt %q: wait for its turn: %w", ws, err)
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
		c.log.Error(opAdopt, "the state handle could not be promoted to writing", dlog.WithCause(fields, err))
		return fmt.Errorf("rollout: adopt %q: promote the state handle: %w", ws, err)
	}
	c.flushDispositions(ctx, ws)
	record, err := c.deps.DB.Workspace(ctx, ws)
	if err != nil {
		c.log.Error(opAdopt, "could not read the workspace being adopted", dlog.WithCause(fields, err))
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
			c.log.Log(dlog.LevelWarn, opAdopt, probeUntold,
				dlog.WithCause(fields, probeErr))
		case state == sessionlock.StateFree:
			c.log.Debug(opAdopt, "no shim holds this workspace's lock; adopting its facts with no session", fields)
			dial = false
		case state == sessionlock.StateUnknown:
			// "Could not tell" is NEVER read as free, and it is never read as
			// held either: it is said out loud.
			c.log.Log(dlog.LevelWarn, opAdopt, probeUntold, fields)
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
			c.log.Error(opAdopt, "could not decode the carried cold gate; the workspace is not adopted", dlog.WithCause(fields, err))
			return fmt.Errorf("rollout: adopt %q: decode the carried cold gate: %w", ws, err)
		}
	}
	var resumeFrom *sessionwatcher.Pointers
	if carried && carry.Pointers != nil {
		from, err := carry.Pointers.decode()
		if err != nil {
			c.log.Error(opAdopt, "could not decode the carried watch pointers; the workspace is not adopted", dlog.WithCause(fields, err))
			return fmt.Errorf("rollout: adopt %q: decode the carried watch pointers: %w", ws, err)
		}
		resumeFrom = &from
	}
	// A SHIM PARKED AT ITS COLD GATE HOLDS NO LOCK, and it is running: it
	// refused the resume before it took one (workspace.Fleet.AdoptParked), and
	// the incumbent detached it for this daemon. Its free lock is not "no
	// process to dial", and undialed it would be served by no daemon at all:
	// a hidden workspace's bring-up is withheld, so nothing else here would
	// ever reach it (shims 76760, 76813 and 76849, 2026-10-23 17:59).
	//
	// UNLESS THE GATE'S SHIM WAS STOOD DOWN: a gate runs no shim (owner
	// ruling, 2026-10-27), and dialing one that is gone failed the adoption of
	// every gated workspace on every deploy (37 on 2026-10-29 19:32), each
	// then adopted on Emacs's retry. With no shim the workspace is adopted
	// sessionless, and its start raises the gate from its record.
	if parked != nil && !dial {
		if carry.ColdGateShim != nil && !*carry.ColdGateShim {
			c.log.Info(opAdopt, "the gate's shim was stood down by the incumbent; nothing is dialed, and the gate is raised from its record", fields)
		} else {
			c.log.Debug(opAdopt, "the lock reads free because the shim is parked at its cold gate; dialing it", fields)
			dial = true
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
		c.log.Error(opAdopt, "could not claim serving ownership", dlog.WithCause(fields, err))
		return fmt.Errorf("rollout: adopt %q: claim serving: %w", ws, err)
	}
	if !claimed {
		err := fmt.Errorf("rollout: adopt %q: the incumbent %s took the workspace back: %w", ws, holder, ErrReclaimed)
		c.log.Error(opAdopt, "the incumbent took the workspace back before this adoption claimed it; it is not adopted here",
			dlog.WithCause(dlog.Merge(fields, dlog.Context{"serving_daemon": string(holder)}), err))
		return err
	}
	// FROM THE CLAIM UNTIL THE WORKSPACE IS OWNED HERE, daemon.addr waits for
	// this adoption (awaitClaimedOwned): the claim is what lets the incumbent
	// exit, and a client that dials the address the exit writes must find the
	// workspace served, never refused not_yet_adopted.
	c.mu.Lock()
	c.beginClaimedLocked()
	c.unlock()
	claimEnded := false
	defer func() {
		if claimEnded {
			return
		}
		c.mu.Lock()
		c.endClaimedLocked()
		c.unlock()
	}()
	// FROM THE CLAIM ON, THE HANDOVER HOLD IS THIS DAEMON'S TO DRAIN, and it
	// is drained whether or not the shim answers: a workspace this daemon
	// serves with the incumbent's hold still standing refuses every prompt
	// for as long as the hold stands. A shim that will not answer leaves the
	// workspace served without a session, which its next prompt revives.
	var dialErr error
	if dial {
		// THE WATCHES THIS DIAL OPENS CATCH UP FROM THE INCUMBENT'S POINTERS
		// (CarriedPointers): what the shim wrote while no daemon watched it
		// reaches this one.
		if resumeFrom != nil {
			c.deps.Shims.ResumeWatchesFrom(ws, *resumeFrom)
		}
		dialErr = c.dialAdopted(ctx, ws, parked, fields)
	}
	// EVERY DIALED ADOPTION WAITS FOR THE SESSION'S FACTS before the
	// workspace becomes this daemon's (2026-11-23). Until the adopted shim's
	// re-announcement is taken up, the watcher answers no turn for a turn the
	// shim is running, and the roster holds neither the session's facts nor
	// its live work: it drew an adopted row `done` for a second before its
	// live work made it `idle_async` again. The watcher releases the wait only
	// once every view stands on the facts (sessionwatcher.factsInLocked), so
	// from the ownership below the row is drawn from what this daemon has read.
	//
	// A MID-WORK ADOPTION whose shim never re-announces is refused, and the
	// incumbent moves it at freeness: the drain below would otherwise start a
	// second turn beside the one running. A free one is already this daemon's
	// to serve, so a late re-announcement is recorded at ERROR and the
	// adoption completes; its row is drawn from its facts when they come.
	if dial && dialErr == nil && parked == nil {
		facts, cancel := context.WithTimeout(ctx, c.deps.FactsBound)
		err := c.deps.Shims.AwaitFacts(facts, ws)
		cancel()
		factsFields := dlog.Merge(fields, dlog.Context{"facts_bound": c.deps.FactsBound.String(), "mid_work": carried && carry.MidWork})
		switch {
		case err != nil && carried && carry.MidWork:
			c.log.Info(opAdopt, "the adopted shim never re-announced its session facts; the mid-work adoption is refused",
				dlog.WithCause(factsFields, err))
			return c.refuseAdopted(ctx, ws, "the adopted shim never re-announced its session facts within "+c.deps.FactsBound.String(), fields)
		case err != nil:
			c.log.Error(opAdopt, "the adopted shim did not re-announce its session facts within the bound; the free workspace is adopted without them, and its row is drawn from them when they come",
				dlog.WithCause(factsFields, err))
		}
	}
	// THE CARRIED QUEUE MEMORY IS INSTALLED BEFORE THE HOLD IS DRAINED: the
	// drain is what runs the carried acts ahead of the held prompts.
	var carryErr error
	if carried {
		if err := c.deps.Bounces.AdoptHandoff(ctx, ws, carry.Queue); err != nil {
			c.log.Error(opAdopt, "could not install the carried queue memory; the held intake is drained all the same", dlog.WithCause(fields, err))
			carryErr = fmt.Errorf("rollout: adopt %q: install the carried queue memory: %w", ws, err)
		}
	}
	// THE DRAIN RELEASES THE HOLD THE INCUMBENT CARRIED, by its identity: a
	// relaunch this daemon began meanwhile holds a lease of the same holder,
	// which is that relaunch's to release. With no carry the incumbent sealed
	// no move, so its quiesce took no hold this adoption drains.
	var hold *ids.LeaseID
	if carried {
		hold = carry.Hold
	} else {
		none := ids.LeaseID("")
		hold = &none
	}
	if err := c.deps.DrainIntake(ctx, ws, hold); err != nil {
		c.log.Error(opAdopt, "could not drain the held intake", dlog.WithCause(fields, err))
		return errors.Join(dialErr, carryErr, c.consumeCarry(ws, carried),
			fmt.Errorf("rollout: adopt %q: drain the held intake: %w", ws, err))
	}
	if carried && dialErr == nil && carryErr == nil {
		if err := c.deps.Bounces.RejudgeHeld(ctx, ws); err != nil {
			c.log.Error(opAdopt, "could not re-judge the held prompts whose verdicts the move superseded", dlog.WithCause(fields, err))
			carryErr = fmt.Errorf("rollout: adopt %q: re-judge the held prompts: %w", ws, err)
		}
	}
	// THE CARRY IS CONSUMED ON EVERY PATH PAST THE DRAIN: its removal is what
	// tells the incumbent a mid-work adoption landed rather than was refused.
	if err := errors.Join(dialErr, carryErr, c.consumeCarry(ws, carried)); err != nil {
		return err
	}
	// A WORKSPACE ADOPTED WITH NO SHIM BEHIND IT HAS ITS SESSION STARTED
	// (sessionless.go) when a takeover brings it up (takeoverSplit): its shim
	// was up at the last stop. The split is read only for a workspace adopted
	// with no shim to dial, the one it decides, so its record is never written
	// for an adoption that dials. The marker is
	// raised here, before the views are published, so the roster never draws
	// the adopted row idle and usable while nothing serves it; the start
	// itself runs once the adoption is complete and the workspace is this
	// daemon's.
	runs := false
	if !dial && !record.Closed {
		up, _, _ := c.takeoverSplit(ctx, []wsm.Workspace{record}, fields)
		runs = len(up) == 1
	}
	sessionless := !dial && !record.Closed && runs
	// A WORKSPACE THAT RUNS NOTHING MAY STILL HAVE A LISTENER: an inert shim
	// the incumbent held and detached, whose lock reads free. It is stood
	// down once the workspace is this daemon's (standDownInert).
	withheldInert := !dial && !record.Closed && !runs
	if sessionless {
		c.deps.BringingUp(ws, true)
	}
	if err := c.deps.PublishViews(ctx, ws); err != nil {
		c.log.Error(opAdopt, "could not publish the workspace's fresh views", dlog.WithCause(fields, err))
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
		// THE PARTICIPANTS ARE ANSWERED AT OWNERSHIP, not once the rest of
		// this adoption is done: the workspace is served here from this
		// instant, and what follows (its merges, its faults, its carried
		// replacements) is this daemon's own work. Answered at the end, a
		// participant waited out that tail too: on 2026-11-13 24 workspaces
		// adopted within 3s were still unanswered when Emacs's 10s
		// AdoptHostWorkspace deadline passed, each held behind its merges'
		// adoption. A tail that fails is still this adoption's error, and
		// its caller's to retry; the settled answer stands.
		e.settle(nil)
	}
	if c.owned == nil {
		c.owned = map[ids.WorkspaceID]bool{}
	}
	c.owned[ws] = true
	c.endClaimedLocked()
	claimEnded = true
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
	c.unlock()
	c.logTransition(opAdopt, ws, "owned", wasOwned, true,
		dlog.Context{"all_joining_owned": complete})
	// THE ROW IS THIS DAEMON'S OWN FROM HERE ON: it stops drawing the
	// incumbent's status.
	c.mirrorOwnershipChanged()

	// THE ADOPTION NAMES ITS PLACE IN THE ORDER: its rank, and how many
	// ordered adoptions reached ownership before it and with it.
	if rank, nth, ordered := c.order.owns(ws); ordered {
		fields = dlog.Merge(fields, dlog.Context{"adoption_rank": rank.String(), "adopted_nth": nth})
	}
	c.log.Info(opAdopt, "adopted the workspace", fields)

	// THE WORKSPACE'S MERGES ARE THIS DAEMON'S NOW: a merge the incumbent
	// suspended for the transfer resumes here at its recorded step, in the
	// bubble it already had (merge/transfer.go).
	if err := c.deps.Merges.AdoptWorkspace(ctx, ws); err != nil {
		c.log.Error(opAdopt, "could not take the adopted workspace's merges", dlog.WithCause(fields, err))
		return fmt.Errorf("rollout: adopt %q: take its merges: %w", ws, err)
	}

	if sessionless {
		c.startSessionless([]wsm.Workspace{record}, fields)
	}
	if withheldInert {
		c.standDownInert(ctx, ws, fields)
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
	if _, err := c.checkStale(ctx, ws, c.takeoverForce(ws), judgedForAdoption); err != nil {
		c.log.Error(opStaleness, "the adopted shim could not be judged against the installed build", dlog.WithCause(fields, err))
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
	c.unlock()
	if !before {
		c.log.Info(opJoin, "the handover being joined was forced; stale adopted shims are bounced at once", nil)
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
	// THE BOUND AND THE WATCH ARE TAKEN ONLY IF THIS CALL ACTUALLY WAITS. The
	// overwhelmingly common path — a manifest already in hand, or none
	// because no handover is in flight — answers on the first read and must
	// cost nothing.
	var bound <-chan time.Time
	var changes <-chan error
	waiting := false
	for {
		if err := c.armFromManifest(); err != nil {
			c.log.Error(operation, "could not re-read the intent manifest", dlog.WithCause(fields, err))
			return err
		}
		c.mu.Lock()
		_, armed := c.rendezvous[ws]
		seen := c.manifestSeen
		c.unlock()
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
				dlog.Merge(fields, dlog.Context{"bound": c.deps.AdoptionWindow.String()}))
			waiting = true
			bound = c.deps.Clock.After(c.deps.AdoptionWindow)
			// The watch is taken before the next read, so a manifest written
			// in between is reported.
			var stop func()
			changes, stop = c.handoverChanges(ctx)
			defer stop()
			continue
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
				dlog.Merge(fields, dlog.Context{"bound": c.deps.AdoptionWindow.String()}))
			return nil
		case err := <-changes:
			if err != nil {
				c.log.Error(operation, "could not watch for the intent manifest", dlog.WithCause(fields, err))
				return err
			}
		}
	}
}

// outgoingDaemon returns the incumbent recorded on a workspace's armed
// rendezvous. Adoption without that identity cannot distinguish the intended
// predecessor from an unrelated serving owner, so it is an invariant error.
func (c *controller) outgoingDaemon(ws ids.WorkspaceID) (ids.InstanceID, error) {
	c.mu.Lock()
	defer c.unlock()
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
	// ONE WATCH ANSWERS EVERY WAITING ADOPTION (servingWatch), which takes
	// its change watch before its first read, so a release in between is
	// reported.
	waiter, stop := c.serving.watch(ws)
	defer stop()
	waiting := false
	for {
		var read servingRead
		select {
		case <-ctx.Done():
			err := context.Cause(ctx)
			c.log.Error(opAdopt, "the adoption lifetime ended before the incumbent released serving ownership", dlog.WithCause(fields, err))
			return fmt.Errorf("rollout: adopt %q: wait for incumbent serving release: %w", ws, err)
		case read = <-waiter.reads:
		}
		if read.watchFailed {
			c.log.Error(opAdopt, "could not watch the state database for the incumbent's release", dlog.WithCause(fields, read.err))
			return fmt.Errorf("rollout: adopt %q: watch for incumbent serving release: %w", ws, read.err)
		}
		if read.err != nil {
			c.log.Error(opAdopt, "could not read serving ownership while waiting for the incumbent", dlog.WithCause(fields, read.err))
			return fmt.Errorf("rollout: adopt %q: read serving ownership: %w", ws, read.err)
		}
		owner := read.owner
		if owner == nil {
			c.log.Debug(opAdopt, "the incumbent released serving ownership; adoption may begin",
				dlog.Merge(fields, dlog.Context{"serving_daemon": ""}))
			return nil
		}
		if *owner == c.deps.Instance {
			c.log.Debug(opAdopt, "this daemon already owns the workspace; adoption may resume",
				dlog.Merge(fields, dlog.Context{"serving_daemon": string(*owner)}))
			return nil
		}
		if *owner != outgoing {
			err := fmt.Errorf("rollout: adopt %q: workspace is served by unexpected daemon %q, want incumbent %q", ws, *owner, outgoing)
			c.log.Error(opAdopt, "an unexpected daemon owns the workspace at the adoption boundary", dlog.WithCause(dlog.Merge(fields, dlog.Context{"serving_daemon": string(*owner)}), err))
			return err
		}
		if !waiting {
			c.log.Info(opAdopt, "waiting for the incumbent to release serving ownership at freeness",
				dlog.Merge(fields, dlog.Context{"serving_daemon": string(*owner)}))
			waiting = true
			c.order.awaiting(ws)
		}
	}
}

// advertiseRefusedBound is how long the claim may stay held before the
// successor says so at ERROR: well past daemonaddr.ClaimWaitBound, the
// longest an orderly incumbent holds the claim after its last transfer.
const advertiseRefusedBound = 30 * time.Second

// advertise writes daemon.addr once every workspace is owned, retrying while
// the OUTGOING daemon still holds the boot claim.
//
// THE ADVERTISEMENT IS NOT PART OF THE ADOPTION. The incumbent releases its
// claim when it exits, and it exits when the adoptions complete — so failing an
// adoption because the claim is still held deadlocks the handover on itself.
// The workspace is adopted either way; only the address file waits.
//
// ONLY A HELD CLAIM IS WAITED ON, AND IN THE KERNEL (retryAdvertise). Any
// other failure -- a state root that is gone, a directory that cannot be
// written -- is not the incumbent departing and will not fix itself, so it is
// an ERROR and nothing is retried. It once retried every failure every 25ms
// forever.
func (c *controller) advertise(ctx context.Context, fields dlog.Context) {
	wrote, err := c.writeDaemonAddrOnce(ctx, fields)
	if !wrote && err == nil {
		return
	}
	if c.refusedStandingDown(err, fields) {
		return
	}
	if err == nil {
		c.log.Info(opAdopt, "every workspace is owned; wrote daemon.addr", nil)
		c.becomeIncumbent(fields)
		return
	}
	if !errors.Is(err, daemonaddr.ErrClaimed) {
		c.log.Error(opAdopt, "daemon.addr could not be written; not retrying a failure that is not a held boot claim", dlog.WithCause(fields, err))
		return
	}
	c.log.Debug(opAdopt, "the outgoing daemon still holds the boot claim; advertising once it lets go", dlog.WithCause(fields, err))
	// THE RETRY OUTLIVES THE CALL. The context here is an rpc's, cancelled the
	// moment the adopt answers — and the claim is released by the incumbent's
	// exit, which happens after that answer. The daemon's own lifetime still
	// ends it.
	lifetime := c.deps.Lifetime
	if lifetime == nil {
		lifetime = context.WithoutCancel(ctx)
	}
	inflight.Standing(func() { c.retryAdvertise(lifetime, fields) })
}

// refusedStandingDown answers whether ERR, a WriteDaemonAddr answer, is the
// refusal of a daemon that has begun standing down (daemonaddr.ErrDeparted),
// and records it, once, at INFO: an expected outcome, never a fault, and no
// write. The caller then neither retries nor takes over: this daemon is on
// its way out (2026-11-22, pid 30922 published 4s into its stand-down).
func (c *controller) refusedStandingDown(err error, fields dlog.Context) bool {
	if !errors.Is(err, daemonaddr.ErrDeparted) {
		return false
	}
	c.log.Info(opAdopt, "daemon.addr was not published: this daemon is standing down",
		dlog.Merge(fields, dlog.Context{"address": c.deps.SelfAddress, "reason": err.Error()}))
	return true
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
//
// ONE CALLER OWNS THE TAKEOVER. Both boot-claim waiters (awaitIncumbentExit,
// retryAdvertise) land here once the claim frees, concurrently. The caller
// that flips joining mode off owns it (takeoverOwned): it runs the takeover
// and then closes TookOver. Any other caller returns at once, closing the
// signal only for a daemon that never joined. Every close is made under mu,
// so the signal cannot be closed twice (a second waiter once panicked on
// `close of closed channel`, merge gate 2026-10-17,
// TestADeployThatFindsTheDaemonStaleHandsOverAFreeWorkspace).
func (c *controller) becomeIncumbent(fields dlog.Context) {
	// THE HANDED WORKSPACES ARE ARMED BEFORE JOINING MODE ENDS. Standing
	// answers owned for any workspace not marked joining once joining mode is
	// off, and the manifest's own watcher (awaitManifest) arms on its own
	// schedule, so a handed workspace read as owned in between and a call the
	// outgoing daemon forwarded was served before its adoption (merge gate
	// 2026-11-18, 6ms apart). The outgoing daemon writes the manifest before
	// it yields, so it is here to arm from; arming is additive, so the
	// watcher's later arm adds nothing.
	c.armFromManifestBeforeTakeover(fields)
	// THE ELDERS ARE READ BEFORE JOINING MODE ENDS, and recorded in the same
	// critical section that ends it. IncumbentAddress answers the incumbent
	// while either holds, so recording the elders only after joining mode
	// ended left a moment with neither, in which the incumbent still served
	// and a not_yet_adopted named no address to act on it at (merge gate
	// 2026-11-19, TestHandoverTransfersAFreeWorkspaceThroughTheAdoptionRendezvous).
	elders := c.probeElders(fields)
	c.mu.Lock()
	was := c.joiningMode
	if was && len(elders) > 0 {
		c.eldersRunning = true
	}
	c.joiningMode = false
	if !was {
		owned := c.takeoverOwned
		if !owned {
			c.closeTookOverLocked()
		}
		c.unlock()
		c.log.Debug(opAdopt, "this daemon is already the incumbent; there is no takeover to run",
			dlog.Merge(fields, dlog.Context{"takeover_owned": owned}))
		return
	}
	c.takeoverOwned = true
	c.unlock()
	defer func() {
		c.mu.Lock()
		c.closeTookOverLocked()
		c.unlock()
	}()
	c.log.Info(opAdopt, "the outgoing daemon yielded; this daemon now serves every workspace it was not handed and no older daemon still serves",
		dlog.Merge(fields, dlog.Context{"state": "joining_mode", "before": true, "after": false}))
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
	// OLDER DAEMONS MAY STILL SERVE WHAT IS BUSY ON THEM (owner ruling,
	// 2026-11-16): the outgoing daemon yields at the end of its transfer pass
	// and keeps its holdouts until each is free. What only the last daemon
	// may do waits for every one of them to end (afterElders).
	if len(elders) == 0 {
		c.closeEldersGone()
		c.lastDaemonsDuties(promoted, &c.stragglerAdoptions, fields)
	} else {
		c.log.Info(opAdopt, "older daemons still serve workspaces busy on them; this daemon takes over now, and does what only the last daemon may once they have ended",
			dlog.Merge(fields, dlog.Context{"elders": elders}))
	}
	c.bounceStaleAdopted(fields)
	// THE QUEUES ARE READ AGAIN NOW THAT ONE DAEMON WRITES THEM. While both
	// daemons served, the outgoing one took queue actions too, and a resume it
	// applied kicked only its own pump: this daemon had recovered a merge
	// suspended under the pause and waited for a resume it never heard, so the
	// queue stood still until someone paused and resumed it again (2026-10-25,
	// unexpected-page-waits-land). Offered to this daemon's pump, each queue is
	// judged from its durable rows, whichever daemon wrote them.
	if err := c.deps.Merges.ReviewQueues(c.lifetime(context.Background())); err != nil {
		c.log.Error(opAdopt, "the merge queues could not be read again at the takeover; a queue change the outgoing daemon made waits for its queue's next change", dlog.WithCause(fields, err))
	}
	// THE WORKSPACES THE TAKEOVER MADE OWNED ARE TOLD TO THE MERGE QUEUE,
	// after they are owned: joining mode ended above, and with no elder the
	// stragglers were marked owned (lastDaemonsDuties), neither through an
	// adoption that tells the queue of its workspace (adopt's AdoptWorkspace).
	// A deploy mediation round the queue passed over while such a mediator
	// was not_yet_adopted here runs now. With elders, the stragglers are told
	// once they are owned (afterElders).
	c.deps.Merges.ServingWidened(c.lifetime(context.Background()))
	// THE CREATES THE OUTGOING DAEMON HELD FOR THE NETWORK ARE THIS DAEMON'S
	// NOW: it is gone, so none of them can run there, and each resumes here on
	// the network's restoration under its own key and pending ref.
	if len(elders) == 0 {
		c.recoverQueuedCreates(fields)
	}
	// THE LANDING REPORTS THE HANDOVER'S DEPLOY COVERED ARE PAID NOW, after
	// every adopted shim was judged: a stale one is bounced, or registered to
	// be once its work ends, and the prompt queue takes a registered bounce
	// before it delivers anything queued. So a report reaches its requester on
	// the build that carries the landing it reports.
	c.deps.Reports.PayOwed(c.lifetime(context.Background()), true, nil)
	c.settleTakeover(fields)
	if len(elders) > 0 {
		inflight.Standing(func() { c.afterElders(elders, promoted, fields) })
	}
}

// probeElders answers the daemons older than this one still running, nil
// when none does or none is wired. A probe that fails is recorded and read
// as no elders: the life locks are the only record of them, and the takeover
// then runs as it always has.
func (c *controller) probeElders(fields dlog.Context) []string {
	if c.deps.LiveElders == nil {
		return nil
	}
	elders, err := c.deps.LiveElders()
	if err != nil {
		c.log.Error(opAdopt, "could not read which older daemons still run; the takeover runs as if none did", dlog.WithCause(fields, err))
		return nil
	}
	return elders
}

// lastDaemonsDuties is what only the last daemon of a state root may do,
// run at the takeover when no older daemon runs and otherwise once the last
// of them has ended (afterElders): the roster mirror stops, the contract
// steps the join left run, the workspaces no daemon serves are adopted (on
// WG), and the shims nothing adopted are recovered.
func (c *controller) lastDaemonsDuties(promoted bool, wg *sync.WaitGroup, fields dlog.Context) {
	// THE HANDOVER IS OVER, AND THE MIRROR WITH IT: every row is this
	// daemon's own from here on.
	c.stopRosterMirror("no older daemon serves anything; this daemon took over")
	// THE CONTRACT STEPS THE JOIN LEFT run now: no older daemon runs, so no
	// running build reads what they remove (wsm.MigrationContract). A step
	// that fails leaves the file as it was, rolled back and recorded by the
	// store; this daemon serves on the shapes it reads either way.
	if promoted {
		if err := c.deps.DB.ApplyContracts(c.lifetime(context.Background())); err != nil {
			c.log.Error(opAdopt, "the contract steps the join left could not be applied; they are applied at the next boot", dlog.WithCause(fields, err))
		}
	}
	c.adoptStragglers(wg, fields)
	if promoted {
		c.recoverOrphans(fields)
	}
}

// recoverQueuedCreates takes up the creates an outgoing daemon held for the
// network: no older daemon runs, so none of them can run there, and each
// resumes here on the network's restoration under its own key and pending
// ref.
func (c *controller) recoverQueuedCreates(fields dlog.Context) {
	if err := c.deps.QueuedCreates.RecoverQueuedCreates(c.lifetime(context.Background())); err != nil {
		c.log.Error(opAdopt, "the creates the outgoing daemon held for the network could not be taken up; they wait until the next daemon takes them up", dlog.WithCause(fields, err))
	}
}

// afterElders waits for ELDERS, the older daemons still running at the
// takeover, to end, then does what only the last daemon may. Each of them
// moved here every workspace it held, so whatever is still unadopted when
// the last one ends is a straggler no daemon serves. A wait that fails is
// recorded, and nothing it guards runs: an older build may still be reading.
func (c *controller) afterElders(elders []string, promoted bool, fields dlog.Context) {
	lifetime := c.lifetime(context.Background())
	if err := c.deps.AwaitElders(lifetime, elders); err != nil {
		if lifetime.Err() != nil {
			c.log.Debug(opAdopt, "the daemon's lifetime ended while older daemons still ran", fields)
			return
		}
		c.log.Error(opAdopt, "could not wait for the older daemons to end; the contract steps, orphan recovery and held creates wait for the next daemon", dlog.WithCause(dlog.Merge(fields, dlog.Context{"elders": elders}), err))
		return
	}
	c.log.Info(opAdopt, "every older daemon has ended; this daemon now does what only the last daemon may",
		dlog.Merge(fields, dlog.Context{"elders": elders}))
	var stragglers sync.WaitGroup
	c.lastDaemonsDuties(promoted, &stragglers, fields)
	// THE STRAGGLERS JUST MARKED OWNED ARE TOLD TO THE MERGE QUEUE, as the
	// takeover tells it of the rest (becomeIncumbent): one whose adoption
	// fails never reaches its own AdoptWorkspace.
	c.deps.Merges.ServingWidened(lifetime)
	c.recoverQueuedCreates(fields)
	stragglers.Wait()
	c.mu.Lock()
	c.eldersRunning = false
	c.unlock()
	c.closeEldersGone()
}

// eldersGoneSignalLocked answers the channel that closes once no older
// daemon runs. Callers hold c.mu.
func (c *controller) eldersGoneSignalLocked() chan struct{} {
	if c.eldersGone == nil {
		c.eldersGone = make(chan struct{})
	}
	return c.eldersGone
}

// closeEldersGone closes the elders-gone signal unless it is already closed.
func (c *controller) closeEldersGone() {
	c.mu.Lock()
	defer c.unlock()
	signal := c.eldersGoneSignalLocked()
	select {
	case <-signal:
	default:
		close(signal)
	}
}

// eldersGoneSignal is eldersGoneSignalLocked for a caller that does not hold
// c.mu.
func (c *controller) eldersGoneSignal() <-chan struct{} {
	c.mu.Lock()
	defer c.unlock()
	return c.eldersGoneSignalLocked()
}

// enterJoiningLocked puts this daemon in joining mode, and with it opens the
// takeover-settled latch: a takeover is now coming, and nothing it will
// start has settled. It is the ONE writer of joining mode's start, so the
// latch becomeIncumbent closes is always the one opened here. The caller
// holds c.mu.
func (c *controller) enterJoiningLocked() {
	if !c.joiningMode {
		c.settled = make(chan struct{})
	}
	c.joiningMode = true
}

// settleTakeover closes the takeover-settled latch once every adoption and
// bring-up the takeover started has finished. Every Add to those counts was
// made above, on this goroutine, before the join below starts, so the join
// cannot race an Add.
func (c *controller) settleTakeover(fields dlog.Context) {
	c.mu.Lock()
	latch := c.settled
	c.unlock()
	inflight.Go(func() {
		c.stragglerAdoptions.Wait()
		c.bringUps.Wait()
		c.log.Info(opAdopt, "the takeover settled: every adoption and session bring-up it started has finished", fields)
		close(latch)
		c.deps.ServingSettled(c.lifetime(context.Background()))
	})
}

// awaitTakeoverSettled waits for the takeover-settled latch, or for ctx. A
// daemon that never joined answers at once. A handover asked while the
// takeover's own work is in flight waits for it, and says so at INFO.
func (c *controller) awaitTakeoverSettled(ctx context.Context, fields dlog.Context) error {
	c.mu.Lock()
	latch := c.settled
	c.unlock()
	select {
	case <-latch:
		return nil
	default:
	}
	c.log.Info(opRollOut, "a handover waits for this daemon's own takeover to settle before it plans anything", fields)
	select {
	case <-latch:
		c.log.Info(opRollOut, "the takeover settled; the handover proceeds", fields)
		return nil
	case <-ctx.Done():
		c.log.Info(opRollOut, "the handover's caller left before this daemon's takeover settled; nothing was started", dlog.WithCause(fields, ctx.Err()))
		return fmt.Errorf("rollout: handover: await the takeover: %w", ctx.Err())
	}
}

// promoteAtTakeover promotes the state handle to writing, reporting whether it
// writes. A refusal is ERROR and the orphan recovery is not attempted: it would
// adopt shims whose serving claims could not be written.
func (c *controller) promoteAtTakeover(fields dlog.Context) bool {
	if err := c.deps.DB.Promote(c.lifetime(context.Background())); err != nil {
		c.log.Error(opAdopt, "the state handle could not be promoted to writing at the takeover; the shims nothing adopted are not recovered", dlog.WithCause(fields, err))
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

// closeTookOverLocked closes the takeover signal unless it is already closed.
// Every close goes through here under mu, so the check and the close cannot
// interleave with another's. Callers hold c.mu.
func (c *controller) closeTookOverLocked() {
	signal := c.tookOverSignalLocked()
	select {
	case <-signal:
	default:
		close(signal)
	}
}

// TookOver answers the channel that closes when this daemon has taken over
// from the daemon it joined (becomeIncumbent). A daemon that never joined
// is not waiting on a takeover, and callers ask only after Joining.
func (c *controller) TookOver() <-chan struct{} {
	return c.tookOverSignal()
}

// tookOverSignal is tookOverSignalLocked for a caller that does not hold c.mu.
func (c *controller) tookOverSignal() <-chan struct{} {
	c.mu.Lock()
	defer c.unlock()
	return c.tookOverSignalLocked()
}

// adoptStragglers takes every workspace that was being handed over but never
// finished its rendezvous. The incumbent is gone, so nobody else can serve it:
// it is OWNED from this instant (a request for it gets this daemon's answer,
// never not_yet_adopted), and its running shim, if one survived, is adopted
// through the same path a headless workspace takes. An adoption that fails is
// that workspace's own logged error; the workspace stays owned either way.
//
// A WORKSPACE MID-ADOPTION AT THE TAKEOVER IS OWNED TOO. A carry look
// (followCarries) can claim a workspace for a headless adoption in the very
// instant the incumbent exits; that adoption marks it owned only when it
// finishes, so until then the workspace read not_yet_adopted with no other
// daemon left to serve it (TestAWorkspaceWhoseHandoverNeverFinishedIsOwnedAfterTheTakeover,
// under the merge queue's full run, 2026-10-31). Its own adoption carries on;
// no straggler adoption is started beside it.
func (c *controller) adoptStragglers(wg *sync.WaitGroup, fields dlog.Context) {
	c.mu.Lock()
	var stragglers []ids.WorkspaceID
	midAdoption := 0
	if c.owned == nil {
		c.owned = map[ids.WorkspaceID]bool{}
	}
	for ws := range c.joining {
		if c.owned[ws] {
			continue
		}
		c.owned[ws] = true
		if c.adopting[ws] {
			midAdoption++
			continue
		}
		stragglers = append(stragglers, ws)
	}
	c.unlock()
	if midAdoption > 0 {
		c.log.Info(opAdopt, "workspaces still mid-adoption at the takeover are owned from now; their own adoptions finish them",
			dlog.Merge(fields, dlog.Context{"workspaces": midAdoption}))
	}
	if len(stragglers) == 0 {
		return
	}
	lifetime := c.deps.Lifetime
	if lifetime == nil {
		lifetime = context.Background()
	}
	c.log.Info(opAdopt, "adopting the workspaces whose handover never finished",
		dlog.Merge(fields, dlog.Context{"workspaces": len(stragglers)}))
	for _, ws := range stragglers {
		wg.Add(1)
		inflight.Go(func() {
			defer wg.Done()
			if err := c.adopt(lifetime, ws, "straggler"); err != nil {
				c.log.Error(opAdopt, "a workspace whose handover never finished could not have its session adopted; it is served without one",
					dlog.WithCause(dlog.Merge(fields, dlog.Context{"workspace": string(ws)}), err))
			}
		})
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
	deferred, judged := c.judgeLiveShims(fields)
	if !judged {
		return
	}
	c.finishDeployStory(deferred, fields)
}

// SettleBoot ends a fresh boot: the shims it adopted reported their builds
// while it adopted them, each judged off the reporter's goroutine, so they are
// judged again here, synchronously, and only then is every owed landing
// report paid -- after the bounce each stale shim owes is registered.
func (c *controller) SettleBoot(ctx context.Context) {
	fields := dlog.Context{"why": "fresh_boot"}
	c.judgeLiveShims(fields)
	c.deps.Reports.PayOwed(ctx, false, nil)
	c.deps.ServingSettled(ctx)
}

// judgeLiveShims judges every live shim against the installed build, and
// answers the workspaces whose stale shim waits for its work to end, and
// whether the workspaces could be listed at all.
func (c *controller) judgeLiveShims(fields dlog.Context) (map[ids.WorkspaceID][]deployprogress.Note, bool) {
	lifetime := c.lifetime(context.Background())
	workspaces, err := c.deps.DB.ListWorkspaces(lifetime)
	if standdown.CutShort(lifetime, err) {
		c.log.Info(opStaleness, "the daemon stood down before the shims were judged against the installed build", dlog.WithCause(fields, err))
		return nil, false
	}
	if err != nil {
		c.log.Error(opStaleness, "could not list the workspaces to check for stale shims", dlog.WithCause(fields, err))
		return nil, false
	}
	checked := 0
	deferred := map[ids.WorkspaceID][]deployprogress.Note{}
	for _, ws := range workspaces {
		if _, live := c.deps.Shims.Client(ws.ID); !live {
			continue
		}
		checked++
		check, err := c.checkStale(lifetime, ws.ID, c.takeoverForce(ws.ID), judgedForSweep)
		if err != nil {
			c.log.Error(opStaleness, "an adopted shim could not be judged against the installed build",
				dlog.WithCause(dlog.Merge(fields, dlog.Context{"workspace": string(ws.ID)}), err))
			continue
		}
		if shimDeferred(check) {
			deferred[ws.ID] = append(deferred[ws.ID], deployprogress.ShimWhenIdle)
		}
	}
	c.log.Info(opStaleness, "judged every adopted shim against the installed build",
		dlog.Merge(fields, dlog.Context{"live_shims": checked}))
	return deferred, true
}

// retryAdvertise writes daemon.addr once the outgoing daemon's exit releases
// the boot claim, which it awaits in the kernel (Deps.AwaitBootClaim). A
// claim still held past advertiseRefusedBound is said once at ERROR, and the
// wait goes on: the incumbent may yet exit.
func (c *controller) retryAdvertise(ctx context.Context, fields dlog.Context) {
	claimed := make(chan error, 1)
	// inflight: joined: the function that starts it waits for it.
	go func() { claimed <- c.deps.AwaitBootClaim(ctx) }()
	select {
	case err := <-claimed:
		c.takeOverOnClaim(ctx, err, fields)
		return
	case <-c.deps.Clock.After(advertiseRefusedBound):
		c.log.Error(opAdopt, "the boot claim is still held long after the outgoing daemon should have exited; still waiting",
			dlog.Merge(fields, dlog.Context{"held_for": advertiseRefusedBound.String()}))
	case <-ctx.Done():
		c.log.Debug(opAdopt, "the daemon's lifetime ended before daemon.addr could be written", fields)
		return
	}
	c.takeOverOnClaim(ctx, <-claimed, fields)
}

// awaitIncumbentExit writes daemon.addr the moment the outgoing daemon lets go
// of the boot claim, and then takes over.
//
// The claim is released by the incumbent's EXIT, so holding it is the proof
// that no other daemon serves anything. It waits as long as the handover
// does -- a busy workspace can hold the incumbent for an hour -- so unlike
// retryAdvertise it never reports a claim held "too long": a long handover is
// ordinary. The wait is the kernel's (Deps.AwaitBootClaim), so a killed
// incumbent is taken over from at once, never at the next look of a backoff
// (TestTheTakeoversOrphanRecoveryReconcilesItsOpenTurn missed its bound under
// load behind a 2.4s poll, 2026-10-02).
func (c *controller) awaitIncumbentExit(ctx context.Context) {
	if c.deps.WriteDaemonAddr == nil {
		return
	}
	c.takeOverOnClaim(ctx, c.deps.AwaitBootClaim(ctx), nil)
}

// takeOverOnClaim is what follows the boot-claim wait: with the claim held,
// daemon.addr is written and this daemon takes over. A wait the daemon's
// lifetime ended is said at DEBUG; any other failure, and a write that fails
// under the held claim, is an ERROR and nothing is retried.
func (c *controller) takeOverOnClaim(ctx context.Context, awaited error, fields dlog.Context) {
	if awaited != nil {
		if ctx.Err() != nil {
			c.log.Debug(opAdopt, "the daemon's lifetime ended before the incumbent exited", fields)
			return
		}
		c.log.Error(opAdopt, "the boot claim could not be awaited; daemon.addr is not written", dlog.WithCause(fields, awaited))
		return
	}
	if err := c.awaitClaimedOwned(ctx, fields); err != nil {
		c.log.Debug(opAdopt, "the daemon's lifetime ended before the adoptions in flight were owned; daemon.addr is not written", dlog.WithCause(fields, err))
		return
	}
	wrote, err := c.writeDaemonAddrOnce(ctx, fields)
	if err != nil {
		if c.refusedStandingDown(err, fields) {
			return
		}
		c.log.Error(opAdopt, "daemon.addr could not be written under the held boot claim", dlog.WithCause(fields, err))
		return
	}
	if !wrote {
		return
	}
	c.log.Info(opAdopt, "the incumbent exited; wrote daemon.addr", fields)
	c.becomeIncumbent(fields)
}

// writeDaemonAddrOnce writes daemon.addr unless a write already succeeded,
// answering whether this call wrote it.
//
// daemon.addr IS WRITTEN ONCE (2026-11-24). The last joining adoption decides
// at its ownership that it completes the set, and writes later; the takeover
// writes once the incumbent exited and the adoptions in flight are owned. The
// two could interleave and both write (merge gate 2026-11-24,
// TestTheTakeoverDoesNotWaitForEveryRendezvous: two writes). The writes are
// serialized here, and the one after a success writes nothing; a failed write
// records nothing, so the other path still writes when it comes.
func (c *controller) writeDaemonAddrOnce(ctx context.Context, fields dlog.Context) (bool, error) {
	c.addrMu.Lock()
	defer c.addrMu.Unlock()
	if c.addrWritten {
		c.log.Debug(opAdopt, "daemon.addr is already written; nothing is written again", fields)
		return false, nil
	}
	if err := c.deps.WriteDaemonAddr(ctx); err != nil {
		return false, err
	}
	c.addrWritten = true
	return true, nil
}

// beginClaimedLocked counts one adoption past its serving claim. The caller
// holds c.mu.
func (c *controller) beginClaimedLocked() {
	if c.claimedAdoptions == 0 {
		c.claimedIdle = make(chan struct{})
	}
	c.claimedAdoptions++
}

// endClaimedLocked retires one claimed adoption, which either made its
// workspace owned or failed. The caller holds c.mu. An end with none counted
// is an accounting defect.
func (c *controller) endClaimedLocked() {
	if c.claimedAdoptions <= 0 {
		panic("rollout: a claimed adoption ended with none counted")
	}
	c.claimedAdoptions--
	if c.claimedAdoptions == 0 {
		close(c.claimedIdle)
	}
}

// awaitClaimedOwned waits until every adoption past its serving claim has
// made its workspace owned here (or failed), or for ctx. An adoption still
// waiting for the incumbent's release is not waited on: the incumbent is
// gone, and the takeover's straggler pass takes that workspace up.
func (c *controller) awaitClaimedOwned(ctx context.Context, fields dlog.Context) error {
	c.mu.Lock()
	n, idle := c.claimedAdoptions, c.claimedIdle
	c.unlock()
	if n == 0 {
		return nil
	}
	c.log.Info(opAdopt, "the incumbent exited while adoptions were still finishing; daemon.addr waits for them to be owned",
		dlog.Merge(fields, dlog.Context{"claimed_adoptions": n}))
	select {
	case <-idle:
		return nil
	case <-ctx.Done():
		return ctx.Err()
	}
}

// setRetrying marks whether a failed adoption's retry owns the workspace
// (entry.retrying).
func (c *controller) setRetrying(ws ids.WorkspaceID, retrying bool) {
	c.mu.Lock()
	defer c.unlock()
	if e, ok := c.rendezvous[ws]; ok {
		e.retrying = retrying
	}
}

// releaseAdoption undoes a headless claim whose adoption failed.
func (c *controller) releaseAdoption(ws ids.WorkspaceID) {
	c.mu.Lock()
	changed := false
	if e, ok := c.rendezvous[ws]; ok {
		changed = e.adoptionClaimed
		e.adoptionClaimed = false
	}
	c.unlock()
	if changed {
		c.logTransition(opJoin, ws, "adoption_claimed", true, false, nil)
	}
}

// startRosterMirror starts the joining window's subscription to the
// incumbent's roster (see Join's exception), for a daemon that was given an
// incumbent's address. The mirror runs until the handover ends, LIFETIME
// ending with this daemon.
func (c *controller) startRosterMirror(lifetime context.Context) {
	if c.deps.IncumbentAddress == "" {
		return
	}
	mirror, err := rostermirror.New(rostermirror.Deps{
		Log:       c.log,
		Incumbent: c.deps.IncumbentAddress,
		Dial:      c.deps.DialIncumbentRoster,
		NotYetAdopted: func(ws ids.WorkspaceID) bool {
			return c.Standing(ws) == StandingNotYetAdopted
		},
		Sink:            c.deps.IncumbentArms,
		NextPublication: c.deps.NextRosterPublication,
	})
	if err != nil {
		// New validated both deps beside IncumbentAddress, so a mirror that
		// cannot be built is a broken wiring.
		panic(fmt.Sprintf("rollout: the roster mirror could not be built: %v", err))
	}
	c.mu.Lock()
	c.mirror = mirror
	c.unlock()
	inflight.Standing(func() { mirror.Run(lifetime) })
}

// mirrorOwnershipChanged tells the mirror a workspace's standing moved.
func (c *controller) mirrorOwnershipChanged() {
	c.mu.Lock()
	mirror := c.mirror
	c.unlock()
	if mirror != nil {
		mirror.OwnershipChanged()
	}
}

// stopRosterMirror ends the joining window's mirror for REASON.
func (c *controller) stopRosterMirror(reason string) {
	c.mu.Lock()
	mirror := c.mirror
	c.unlock()
	if mirror != nil {
		mirror.Stop(reason)
	}
}

// armFromManifestBeforeTakeover arms every workspace the intent manifest names,
// for becomeIncumbent. No manifest arms nothing; an unreadable one is an
// error, recorded here, and its watcher records its own refusal.
func (c *controller) armFromManifestBeforeTakeover(fields dlog.Context) {
	m, found, err := ReadManifest(c.deps.IntentManifest)
	if err != nil {
		c.log.Error(opAdopt, "the intent manifest could not be read at the takeover; its workspaces are armed only when its watcher reads it",
			dlog.WithCause(dlog.Merge(fields, dlog.Context{"path": c.deps.IntentManifest}), err))
		return
	}
	if !found {
		return
	}
	// WHETHER THE HANDOVER IS A DEPLOY'S IS READ HERE TOO, for the same
	// reason: the takeover finishes the deploy's story (finishDeployStory)
	// right after joining mode ends, and the manifest's watcher records the
	// deploy on its own schedule. Read 0.7ms after the takeover had finished
	// its story, the successor never said `updated` (merge gate 2026-11-19).
	c.recordDeployTakeover(m)
	armed := c.armSessions(m.Daemon, m.WrittenAt, m.Sessions)
	c.log.Info(opAdopt, "armed the manifest's workspaces before ending joining mode, so none reads as owned before its adoption",
		dlog.Merge(fields, dlog.Context{"armed": armed, "outgoing_daemon": string(m.Daemon)}))
}
