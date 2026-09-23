package rollout

import (
	"os"
	"sync"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

// The controller's operation names. Every logical branch records under one of
// them, per the logging contract.
const (
	opNew         = "daemon.rollout.new"
	opTrigger     = "daemon.rollout.trigger"
	opClassify    = "daemon.rollout.classify"
	opDeploy      = "daemon.rollout.deploy"
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
	opStaleness   = "daemon.rollout.staleness"
	opReloadWebap = "daemon.rollout.reload_webapp"
)

// DeployScriptEnv overrides bin/deploy-all.sh for the self-reload trigger. It
// is how a test asserts the trigger reached the deploy chain without running
// the real one.
const DeployScriptEnv = "AGENT_REPL_DEPLOY_SCRIPT"

// DefaultDeployScript is the ONE deploy chain, relative to the module root.
const DefaultDeployScript = "bin/deploy-all.sh"

// The rollout's default windows.
const (
	// DefaultExpectedOutage is the bounded outage the announcement states.
	DefaultExpectedOutage = 5 * time.Second
	// DefaultAdoptionWindow is how long the outgoing daemon gives an adoption.
	DefaultAdoptionWindow = 30 * time.Second
	// DefaultHoldoutWarnEvery is the never-free warning cadence, per the
	// ruling: wait forever, name the holdout every ten minutes.
	DefaultHoldoutWarnEvery = 10 * time.Minute
	// DefaultStandDownWindow is how long a gracefully killed shim has before
	// the force-kill.
	DefaultStandDownWindow = 30 * time.Second
)

// ResolveDeployScript answers the deploy script in force: DeployScriptEnv when
// it is set, else the wired value, else DefaultDeployScript.
func ResolveDeployScript(wired string) string {
	if fromEnv := os.Getenv(DeployScriptEnv); fromEnv != "" {
		return fromEnv
	}
	if wired != "" {
		return wired
	}
	return DefaultDeployScript
}

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
	// handingOver latches while a handover is in flight, so a second one is
	// REFUSED rather than started beside it (see ErrAlreadyRollingOut). It is
	// raised before the successor is spawned and lowered only by a handover
	// that failed before announcing anything.
	handingOver bool
	// staleBounces counts the per-workspace stale-shim relaunches a successor
	// started when it became the incumbent. Each waits for its own
	// workspace's freeness; the group is what lets a caller (a test) know the
	// checks have all run to their end rather than guessing with a delay.
	staleBounces sync.WaitGroup
	// bouncedStamp is the reported shim build each workspace was LAST bounced
	// for. It is what makes the build-staleness bounce fire ONCE per observed
	// stamp: a relaunched shim that comes back reporting the same sha as the
	// one just stood down is not stale again, and without this the bounce
	// re-triggers on every mount forever, spawning a shim each round.
	bouncedStamp map[ids.WorkspaceID]string
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
