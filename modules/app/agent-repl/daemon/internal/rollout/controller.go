package rollout

import (
	"sync"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

// The controller's operation names. Every logical branch records under one of
// them, per the logging contract.
const (
	opNew         = "daemon.rollout.new"
	opHandover    = "daemon.rollout.handover"
	opTransfer    = "daemon.rollout.transfer"
	opAdoption    = "daemon.rollout.adoption_window"
	opJoin        = "daemon.rollout.join"
	opAdoptHost   = "daemon.rollout.adopt_host"
	opAdoptWeb    = "daemon.rollout.adopt_web"
	opAdopt       = "daemon.rollout.adopt"
	opManifest    = "daemon.rollout.manifest"
	opReconcile   = "daemon.rollout.reconcile"
	opRelaunch    = "daemon.rollout.relaunch"
	opBounce      = "daemon.rollout.bounce"
	opStaleness   = "daemon.rollout.staleness"
	opReloadWebap = "daemon.rollout.reload_webapp"
)

// The rollout's default windows.
const (
	// DefaultExpectedOutage is the bounded outage the announcement states.
	DefaultExpectedOutage = 5 * time.Second
	// DefaultAdoptionWindow is how long the outgoing daemon gives an adoption.
	DefaultAdoptionWindow = 30 * time.Second
	// DefaultHoldoutWarnEvery is the cadence a handover waiting on busy
	// workspaces names them at, per the ruling: wait forever, name the holdout
	// every ten minutes.
	DefaultHoldoutWarnEvery = 10 * time.Minute
	// DefaultStandDownWindow is how long a gracefully killed shim has before
	// the force-kill.
	DefaultStandDownWindow = 30 * time.Second
)

// controller is the Controller implementation.
type controller struct {
	deps Deps
	log  dlog.Logger

	mu sync.Mutex
	// rendezvous is the per-workspace adopt ledger, armed either by this
	// daemon's own announcement (the outgoing side, so ExpectedParticipants can
	// answer) or by the intent manifest read at Join (the incoming side).
	rendezvous map[ids.WorkspaceID]*entry
	// owned is every workspace this joining daemon has finished adopting.
	owned map[ids.WorkspaceID]bool
	// joining is the set of workspaces the manifest named, so the daemon.addr
	// write happens exactly when the last one is owned.
	joining map[ids.WorkspaceID]bool
	// joiningMode reports that this daemon booted as a SUCCESSOR. It owns no
	// workspace until it adopts one, whatever the intent manifest says or has
	// not yet said.
	joiningMode bool
	// manifestSeen reports that the incumbent's intent manifest has actually
	// been READ. Until it has, this daemon does not know which workspaces are
	// being handed to it, so "not armed" says nothing: the manifest is written
	// AFTER the successor's address is announced, and a participant that dials
	// the announced address at once arrives ahead of it. Once it is read the
	// transfer set is known and a workspace it does not name genuinely has no
	// transfer announced.
	manifestSeen bool
	// transferred is every workspace this daemon handed to a successor, mapped
	// to the successor's address. It is what makes a per-workspace rpc refuse
	// with `transferring_away{address}` instead of serving a workspace this
	// daemon no longer owns.
	transferred map[ids.WorkspaceID]string
	// pendingDispositions are the bounce dispositions reconciled while the
	// state handle was still READ-ONLY. A joining successor reconciles the
	// outgoing daemon's manifest before it owns anything, and the accounting
	// is a WRITE: it is held here and written the moment the handle is
	// promoted, so the record is deferred rather than lost.
	pendingDispositions []pendingDisposition
	// successor is the address of the daemon taking over, empty while no
	// handover is in flight.
	successor string
	// handover is THE ONE SUCCESSOR SLOT: non-nil while a handover is in
	// flight, so a second one is REFUSED rather than started beside it (see
	// ErrAlreadyRollingOut). It is claimed before the successor is spawned, the
	// spawned successor lives in it, and it is emptied only by a handover that
	// failed AND whose successor was confirmed gone (abandonHandover). A
	// successor that would not stop keeps the slot claimed, which is what makes
	// two successors unrepresentable: nothing spawns while the slot is held.
	handover *handoverSlot
	// staleChecks counts the staleness judgements ShimReported dispatched off
	// its caller's lock, so a caller (a test, the orderly exit) joins them
	// rather than guessing with a delay. None of them WAITS on a workspace:
	// the bounce registry does the waiting.
	staleChecks sync.WaitGroup
	// handoverDone joins the one goroutine that follows a handover to its end
	// (the transfers, the adoption windows, the exit).
	handoverDone sync.WaitGroup
	// reported is the last build each live shim reported, "" for a shim that
	// reported none.
	reported map[ids.WorkspaceID]string
	// forcedTakeover is the handover the successor joined was FORCED, so the
	// stale shims it adopts are bounced at once too.
	forcedTakeover bool
	// deployTakeover is the handover the successor joined was a DEPLOY's, so
	// this daemon says `updated` on the footer once it has taken over.
	deployTakeover bool
	// stragglerAdoptions counts the adoptions becomeIncumbent started for
	// workspaces whose handover never finished, for the same reason.
	stragglerAdoptions sync.WaitGroup
	// tookOver closes once this daemon stops JOINING and becomes the only
	// daemon (becomeIncumbent). Made lazily under mu (tookOverSignal).
	tookOver chan struct{}
	// adopting marks the workspaces whose adoption is running right now.
	adopting map[ids.WorkspaceID]bool
	// bouncedStamp is the reported shim build each workspace was LAST bounced
	// for. It is what makes the build-staleness bounce fire ONCE per observed
	// build: a relaunched shim that comes back reporting the same build as the
	// one just stood down cannot be fixed by bouncing it again, and without
	// this the bounce re-triggers on every report forever, spawning a shim
	// each round.
	bouncedStamp map[ids.WorkspaceID]string
	// staleInFlight marks the workspaces whose stale-build bounce is
	// registered or running, from its claim until its Done. A report judged
	// meanwhile is the shim being replaced, not a relaunched one.
	staleInFlight map[ids.WorkspaceID]bool
}

// entry is one workspace's rendezvous state.
type entry struct {
	// outgoing is the incumbent whose serving claim must be released before
	// this successor may adopt. The nil serving row is the durable handover
	// latch: unlike process timing, it says the incumbent has passed freeness,
	// quiesced intake and detached from the shim.
	outgoing ids.InstanceID
	// expected is who owed an adoption call at announcement.
	expected Participants
	// hostCalled and webCalled record who has since called. The web slot is
	// satisfied by the FIRST AdoptWebWorkspace from any connection, because the
	// reloaded page — not a surviving stream — is the web participant.
	hostCalled bool
	webCalled  bool
	// headlessClaimed records that a manifest read has taken responsibility for
	// adopting this headless workspace, so a later read does not adopt it a
	// second time.
	headlessClaimed bool
	// adopted records that the workspace is owned, so a later call succeeds
	// immediately rather than re-adopting.
	adopted bool
	// adopting records that one caller is RUNNING the adoption. A caller that
	// arrives after the rendezvous was satisfied -- a reloaded page's own
	// AdoptWebWorkspace behind the one that completed it -- waits on done
	// rather than running a second adoption: two ran at once, both drained
	// the handover hold, and the second's release met "lease not found"
	// (e2e TestWebappLayerRestartHandover, 2026-09-24).
	adopting bool
	// done closes when the adoption completes. Every Adopt* call WAITS on it:
	// the participants call concurrently and "all calls succeed together" is
	// the rendezvous's whole meaning, so the caller that arrives first is not
	// told not_yet_adopted -- it waits for the one that completes it.
	done chan struct{}
	// failed carries an adoption failure to the waiters, so a caller that did
	// not run the adoption still learns why it did not happen.
	failed error
}

// settle closes an entry's completion channel exactly once, with the outcome.
func (e *entry) settle(err error) {
	if e.done == nil {
		return
	}
	select {
	case <-e.done:
		return
	default:
	}
	e.failed = err
	close(e.done)
}

// satisfied reports whether every expected participant has called.
func (e *entry) satisfied() bool {
	if e.expected.Host && !e.hostCalled {
		return false
	}
	if e.expected.Web && !e.webCalled {
		return false
	}
	return true
}

// ExpectedParticipants reports how many adoption calls a workspace's transfer
// owes. A headless workspace owes zero and transfers via WSM facts and the
// kernel lock alone.
func (c *controller) ExpectedParticipants(ws ids.WorkspaceID) int {
	c.mu.Lock()
	defer c.mu.Unlock()
	e, ok := c.rendezvous[ws]
	if !ok {
		return 0
	}
	return e.expected.Count()
}

// withCause stamps an error onto a record's context without mutating the
// caller's map.
func withCause(fields dlog.Context, err error) dlog.Context {
	out := merge(fields, nil)
	out["cause"] = err.Error()
	return out
}

// merge copies base and overlays extra, so no record shares a map with another.
func merge(base, extra dlog.Context) dlog.Context {
	out := make(dlog.Context, len(base)+len(extra)+1)
	for k, v := range base {
		out[k] = v
	}
	for k, v := range extra {
		out[k] = v
	}
	return out
}

// logTransition records one workspace-scoped rollout state change. The
// before/after pair makes the in-memory ledger reconstructible from a trace.
func (c *controller) logTransition(operation string, ws ids.WorkspaceID, state string, before, after any, extra dlog.Context) {
	fields := merge(dlog.Context{
		"state":  state,
		"before": before,
		"after":  after,
	}, extra)
	c.log.With(dlog.Context{"workspace": string(ws)}).Debug(operation,
		"rollout state changed", fields)
}

// milliseconds renders an instant as the epoch milliseconds the contract's
// *_ms fields carry.
func milliseconds(at time.Time) int64 { return at.UnixNano() / int64(time.Millisecond) }
