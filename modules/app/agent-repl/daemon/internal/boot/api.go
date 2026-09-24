// Package boot is the boot sequence and adoption reconciliation.
//
// It probes the workspace locks to find shims that outlived a crashed daemon,
// ADOPTS them rather than killing and restarting them, reconciles the intent
// manifest an outgoing daemon left, restores the held prompts all-or-nothing,
// and closes the turns that never got a terminal. See ARCHITECTURE.md "boot".
//
// EVERY STEP IS LOUD. A boot that could not complete a step does not start
// degraded: it returns the error and the daemon exits, because a daemon that
// silently skipped its reconciliation would go on to answer for state it never
// read.
//
// THE SPLIT RECONCILIATION (ruled by the project lead): an ADOPTED workspace's
// in-flight turns are re-opened by the sessionwatcher the adoption installs —
// they are still running and nothing about them is orphaned. Only a
// CLIENT-LESS workspace, whose kernel lock reads free, has its turns closed as
// orphans here, in one transaction. A probe that could not tell is NEVER read
// as free, so such a workspace is neither adopted nor orphan-closed; it is
// recorded and left alone.
package boot

import (
	"context"
	"fmt"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/merge"
	"claude-repld/internal/promptqueue"
	"claude-repld/internal/rollout"
	"claude-repld/internal/sessionlock"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/shimsocket"
	"claude-repld/internal/startingshim"
	"claude-repld/internal/stateroot"
	"claude-repld/internal/wsm"
)

// DefaultAdoptBound is how long a surviving shim's adoption may take before
// the boot stops waiting on it. Every survivor is dialed CONCURRENTLY, so the
// bound is paid once for a whole boot rather than once per workspace.
//
// IT IS WHY THE DAEMON SERVES AT ALL. The listener is bound and daemon.addr is
// published BEFORE this reconciliation runs (cmd/claude-repld/run.go steps 5
// and 6) and `http.Server.Serve` is not reached until after it, so every
// instant this step spends is an instant the kernel is queueing client
// connections onto a socket nobody is accepting. An unbounded adoption is
// therefore not a slow boot: it is a daemon that listens forever and answers
// nothing, which is what pid 31984 did for ten hours with its accept queue at
// 128/128 — its lock read HELD for a shim whose socket path was gone, and
// `shimclient.bringUp` redials THAT forever by design.
//
// THE NUMBER HAS ONE AUTHOR, in the package whose operation it bounds: the
// fleet's own adoptions are bounded by the same one, and a boot and a revival
// that gave up at different instants would be two answers to one question.
// The sizing lives with the constant.
const DefaultAdoptBound = shimclient.DefaultAdoptBound

// Report is what one boot reconciled. It is returned rather than only logged
// so the daemon can answer for its own startup.
type Report struct {
	// Adopted are the workspaces whose surviving shims were reconnected —
	// EVERY adopted process, inert ones included, because the count answers
	// "how many shims did this boot take over".
	Adopted []ids.WorkspaceID
	// AdoptedSessions are the adopted shims that carry a SESSION: the ones
	// whose workspace lock read HELD. They are what the bounce accounting
	// judges, because they are the only adopted shims that had a session to
	// lose.
	AdoptedSessions []rollout.AdoptedSession
	// AdoptedInert are the adopted shims that carry NO session: reached
	// through a live listening socket while their workspace lock read FREE.
	// A shim takes that lock at StartSession, so a free lock behind a live
	// listener is the shim stating it has not started one. They are recorded
	// separately because they are neither a survivor to account for nor a
	// client-less workspace whose turns are orphans.
	AdoptedInert []ids.WorkspaceID
	// MissingDirClosed are the workspaces this boot CLOSED because their
	// directory no longer exists. They are counted separately from Orphaned
	// because closing a row is a registry decision about the workspace, while
	// an orphan close is about one workspace's unterminated turns.
	MissingDirClosed []ids.WorkspaceID
	// Orphaned are the turns closed because they had no terminal.
	Orphaned []ids.TurnID
	// HoldsRestored is how many held prompts came back.
	HoldsRestored int
	// MergesRecovered are the in-flight merges resumed.
	MergesRecovered []ids.WorkspaceID
	// PendingBringUp are the open, client-less workspaces whose sessions the
	// bring-up will start. THE RECONCILIATION ONLY NAMES THEM; it does not
	// start them, because starting them is what a session start costs and the
	// daemon does not answer a single unary until the reconciliation has
	// returned. The caller runs `BringUp` over this set once the listener is
	// accepting. See BringUp for the invariant.
	PendingBringUp []wsm.Workspace
	// Undetermined are the workspaces whose kernel lock probe could not tell.
	// They are neither adopted nor orphan-closed: "could not tell" is never
	// read as free, and this record is the only place that says so.
	Undetermined []ids.WorkspaceID
	// Dispositions is the intent manifest's reconciliation, one record per
	// session. PRESERVED, ROLLED, DIED and UNKNOWN are never collapsed.
	Dispositions []rollout.Disposition
}

// BringUpReport is what the bring-up step did. It is separate from Report
// because the step runs AFTER the reconciliation has been reported and the
// daemon is already serving: a boot report that carried these counts would be
// stating an outcome it could not yet know.
type BringUpReport struct {
	// BroughtUp are the open, client-less workspaces whose session was
	// STARTED. An open workspace is never session-less (owner ruling,
	// 2026-09-13), so a survivor that did not survive is brought back here.
	BroughtUp []ids.WorkspaceID
	// HibernatedLeft are the open, client-less workspaces left asleep.
	// Hibernation is the memory knob and it is deliberate: bringing one back
	// at boot would spend the ~500MB the sweep reclaimed, for a workspace
	// nobody has asked for.
	HibernatedLeft []ids.WorkspaceID
	// BringUpFailed are the workspaces whose start could not complete. A
	// failure is PER WORKSPACE — the next one is still started — and it raises
	// the same fault an open's failed start raises, so the failure is on every
	// surface and not only in this count.
	BringUpFailed []ids.WorkspaceID
	// StoodDown are the workspaces whose start ended because THIS DAEMON stood
	// their shim down under it -- the exit's drain force-stopping every
	// session while the bring-up was still running. It is counted apart from
	// BringUpFailed so `failed` still means a session that would not come up:
	// nothing about these workspaces is broken, and the next boot brings them
	// up again.
	StoodDown []ids.WorkspaceID
}

// Sequence runs the boot.
type Sequence interface {
	// Run performs the whole reconciliation: probe the workspace locks, adopt
	// surviving shims, reconcile the intent manifest, restore holds
	// all-or-nothing, close orphaned turns, and recover in-flight merges. Any
	// step that cannot complete fails the boot LOUDLY rather than starting
	// degraded.
	Run(ctx context.Context) (Report, error)
	// BringUp starts the session of every workspace in Report.PendingBringUp.
	//
	// THE LISTENER ANSWERS FIRST. The boot claim and daemon.addr are published
	// before the reconciliation runs and `http.Server.Serve` is not reached
	// until after it, so every instant a boot step spends is an instant no
	// unary is answered — and `DaemonHealth` is the unary Emacs recognizes a
	// replacement daemon by, under a 3s bound it does not lengthen. A bring-up
	// inside the reconciliation put N shim spawns in front of that bound and
	// the deploy reported the replacement as never observed, though the
	// daemon had come up and started both sessions (2026-09-13,
	// `elisp.services.runtime-await-failed elapsed=6.377`).
	//
	// So this is the one boot step that DOES NOT GATE READINESS: the caller
	// runs it on its own goroutine once the daemon is serving. It still logs
	// its per-workspace records and its one summary, and every failure is
	// still per workspace and still raises that workspace's own fault.
	BringUp(ctx context.Context, pending []wsm.Workspace) BringUpReport
	// Joining reports whether this daemon was started with -joining, in which
	// case it takes ownership workspace by workspace from the incumbent and
	// publishes daemon.addr only once it owns every one.
	Joining() bool
}

// Deps are the boot sequence's collaborators.
type Deps struct {
	// Layout names every path under the state root.
	Layout stateroot.Layout
	// DB is the durable state being reconciled.
	DB wsm.DB
	// Supervisor adopts the surviving shims.
	Supervisor shimclient.Supervisor
	// Queue restores the held prompts.
	Queue promptqueue.Queue
	// Merge recovers in-flight merges.
	Merge merge.Orchestrator
	// Rollout reconciles the intent manifest (persisting every disposition as
	// a fault) and, for a JOINING daemon, runs the join half of the handover.
	//
	// SEAM ADDITION (recorded): the skeleton named no rollout controller, and
	// neither the manifest reconciliation nor the joining path can be run
	// without one.
	Rollout rollout.Controller
	// BindViews binds every registered workspace whose directory exists on the
	// per-workspace resolvers, writing nothing (workspace.Verbs.BindViews). It
	// runs FIRST: every later step can raise or close a fault, a fault reaches
	// the footer, and a footer record for an unbound workspace is an invariant
	// violation. Required.
	BindViews func(ctx context.Context) error
	// RunDir is the kernel-lock directory the workspace locks are probed in.
	RunDir string
	// JoiningAddress is the incumbent's address when this daemon is a joining
	// successor, empty otherwise.
	JoiningAddress string
	// Probe derives and probes one workspace's shim-held kernel lock. It is
	// injected so a test drives the adopt-versus-orphan decision without a
	// real flock; nil means the production probe.
	Probe ProbeFunc
	// SocketProbe answers whether a shim is listening on a workspace's socket
	// path. It is a SECOND kernel fact beside the lock, because the lock says
	// a conversation is owned and only the socket says the owner is
	// reachable; injected so a test drives the decision without a real
	// listener, nil means the production probe.
	SocketProbe SocketProbeFunc
	// Adopted installs a client adopted from a surviving shim, so the session
	// fleet serves the workspace through the process that is already running.
	// It is a FUNCTION because the fleet sits beside boot rather than beneath
	// it. It is required: an adoption nothing installed would leave the daemon
	// believing it adopted a shim it cannot reach.
	Adopted AdoptFunc
	// StartSession brings ONE workspace's session up, through the same path
	// OpenWorkspace takes (workspace.Fleet.Start). It is a FUNCTION for the
	// same reason Adopted is: the session fleet sits beside boot rather than
	// beneath it. It is required — a boot that silently skipped it would
	// leave every unadopted workspace session-less, which is the state the
	// bring-up exists to abolish.
	StartSession StartFunc
	// AdoptBound bounds ONE surviving shim's adoption; zero means
	// DefaultAdoptBound. An adoption that overruns it is reported at ERROR and
	// the workspace is UNDETERMINED — neither adopted nor orphan-closed — which
	// is the state the sequence already has for "the kernel would not say".
	AdoptBound time.Duration
	// Now supplies the instant an orphan close is stamped with; nil means
	// time.Now.
	Now func() time.Time
	// Clock drives the wait for a PREDECESSOR'S STARTING SHIM to announce
	// itself on its socket; nil means startingshim.SystemClock. It is a field
	// so a test of that wait drives time rather than sleeping through it.
	Clock startingshim.Clock
	// ShimAlive reports whether a recorded spawned pid names a live process;
	// nil means startingshim.Alive, which is kill(pid, 0).
	ShimAlive func(pid int) bool
	// Log is the boot logger.
	Log dlog.Surfaces
}

// ProbeFunc probes ONE workspace's kernel lock, deriving the path from the run
// directory and the worktree. It takes the two inputs rather than a path so
// the whole derive-and-probe step is one injection point.
type ProbeFunc func(runDir, workspaceDir string) (sessionlock.State, error)

// SocketProbeFunc probes ONE workspace's shim socket path for a listener.
type SocketProbeFunc func(socketPath string) (shimsocket.State, error)

// AdoptFunc installs a client adopted from a surviving shim.
type AdoptFunc func(ctx context.Context, ws ids.WorkspaceID, client shimclient.Client) error

// StartFunc brings ONE workspace's session up. workspace.Fleet.Start
// satisfies it, which is exactly what OpenWorkspace calls.
type StartFunc func(ctx context.Context, ws ids.WorkspaceID) error

// probeWorkspaceLock builds the production probe over log. Every probe result
// — held, free, and could-not-tell — lands a record, because a lock probe is a
// diagnosis-critical event and a silent one defeats the boot report. Any error
// other than "held" is StateUnknown WITH the error, because an unreadable lock
// is never reported as free.
func probeWorkspaceLock(log dlog.Logger) ProbeFunc {
	log = log.With(dlog.Context{"component": "daemon.boot.probe_workspace_lock"})
	return func(runDir, workspaceDir string) (sessionlock.State, error) {
		path, err := sessionlock.WorkspaceLockPath(runDir, workspaceDir)
		if err != nil {
			log.Error("daemon.boot.probe_workspace_lock", "could not derive the workspace lock path",
				dlog.Context{"run_dir": runDir, "workspace_dir": workspaceDir, "error": err.Error()})
			return sessionlock.StateUnknown, fmt.Errorf("derive the workspace lock path: %w", err)
		}
		return sessionlock.ProbeWithLog(log, path)
	}
}

// probeShimSocket builds the production socket probe over log.
func probeShimSocket(log dlog.Logger) SocketProbeFunc {
	log = log.With(dlog.Context{"component": "daemon.boot.probe_shim_socket"})
	return func(socketPath string) (shimsocket.State, error) {
		return shimsocket.ProbeWithLog(log, socketPath)
	}
}

// New builds the boot sequence. Every collaborator is required: a boot that
// silently skipped a missing one would report a reconciliation it never ran.
func New(deps Deps) (Sequence, error) {
	missing := func(what string) error { return fmt.Errorf("boot: %s is required", what) }
	switch {
	case deps.DB == nil:
		return nil, missing("a state client")
	case deps.Supervisor == nil:
		return nil, missing("a shim supervisor")
	case deps.Queue == nil:
		return nil, missing("a prompt queue")
	case deps.Merge == nil:
		return nil, missing("a merge orchestrator")
	case deps.Rollout == nil:
		return nil, missing("a rollout controller")
	case deps.Adopted == nil:
		return nil, missing("an adoption installer")
	case deps.StartSession == nil:
		return nil, missing("a session starter")
	case deps.BindViews == nil:
		return nil, missing("a view binder")
	case deps.RunDir == "":
		return nil, missing("a kernel-lock run directory")
	case deps.Log == nil:
		return nil, missing("log surfaces")
	}
	probe := deps.Probe
	if probe == nil {
		probe = probeWorkspaceLock(deps.Log.Global())
	}
	socketProbe := deps.SocketProbe
	if socketProbe == nil {
		socketProbe = probeShimSocket(deps.Log.Global())
	}
	now := deps.Now
	if now == nil {
		now = time.Now
	}
	adoptBound := deps.AdoptBound
	if adoptBound <= 0 {
		adoptBound = DefaultAdoptBound
	}
	return &sequence{
		deps: deps, probe: probe, socketProbe: socketProbe, now: now, adoptBound: adoptBound,
		starting: startingshim.Waiter{Alive: deps.ShimAlive, Probe: socketProbe, Clock: deps.Clock},
	}, nil
}
