package shimclient

import (
	"context"
	"errors"
	"fmt"
	"io"
	"net"
	"os"
	"os/exec"
	"strings"
	"sync"
	"sync/atomic"
	"syscall"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"
	"agentrepl/proto/shim/v1/shimv1connect"

	"connectrpc.com/connect"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

// HealthyStandDownExit is the MEASURED time a healthy shim takes to leave on
// SIGTERM. Every bound below is derived from it rather than rounded to a
// pleasing number.
//
// MEASURED on this host by signalling a bound, serving shim's process group
// and waiting for the child's exit: the real Node shim (`dist/main.js`,
// `--fake`) 2.97ms min / 4.24ms p50 / 5.03ms p90 / 6.69ms max over 20 spawns,
// and the integration suite's fake shim 0.45ms min / 0.61ms p50 / 0.68ms p90 /
// 0.71ms max over 30. The Node shim's SIGTERM handler runs the SAME teardown
// its `KillSession` rpc runs, so this is the whole graceful stop, not a
// prefix of one.
const HealthyStandDownExit = 7 * time.Millisecond

// DefaultKillGrace is how long a SIGTERMed shim has to exit before the
// SIGKILL. Bounded, because a wedged shim must not wedge the daemon's
// shutdown.
//
// IT IS THE SHIM'S OWN SINGLE-STAGE LAST RESORT PLUS A MARGIN, not a round
// number and not a guess. The shim's SIGTERM stand-down runs the teardown
// whose per-stage bound is `WATCHER_CONCLUSION_BUDGET_MS` (1s,
// agent-shim/claude/shim/src/engine/session.ts), so a grace at or below 1s
// would SIGKILL a shim that was still legitimately concluding one tail; 250ms
// on top of that is the margin. Against the measurement above, 1250ms is ~187x
// the Node shim's observed maximum and ~1760x the fake shim's, so nothing
// healthy is anywhere near it.
//
// IT DOES NOT COVER FOUR STAGES BACK TO BACK, and that is deliberate rather
// than an oversight. Every graceful path in this daemon sends SIGTERM only
// AFTER the `KillSession` rpc has already run that teardown -- the drain's
// sweep, the relaunch engine's stand-down, and `Fleet.KillSession` itself all
// ask before they signal -- so the SIGTERM handler is re-entering a teardown
// that has already concluded. The one path that signals without asking first
// (`abandonBringUp`) has no session to tear down at all.
const DefaultKillGrace = 1250 * time.Millisecond

// EscalationBound is the room a caller must leave AFTER the grace for the
// SIGKILL to be delivered and its exit decode to land. SIGKILL is not
// negotiable and the reap that follows it is the kernel handing over a wait
// status already waiting to be read: measured sub-millisecond on every kill in
// this package's suite, so 250ms is two orders of magnitude of headroom.
const EscalationBound = 250 * time.Millisecond

// GracefulKillBound is the whole of a graceful Kill's worst case: the SIGTERM
// grace, then the SIGKILL and the reap after it. A caller whose own bound is
// smaller than this CANNOT observe the escalation -- it gives up at the exact
// moment the shim would have been SIGKILLed and reports a shim this daemon is
// still stopping as leaked. `drain.DefaultStandBound` is derived from it for
// that reason.
const GracefulKillBound = DefaultKillGrace + EscalationBound

// DefaultAdoptBound is how long ONE adoption of an already-running shim may
// take before the caller stops waiting on it.
//
// AN ADOPTION THAT IS NOT BOUNDED NEVER ENDS. `bringUp` is a dial ladder with
// no attempt limit, and for an ADOPTED client the only two ways out besides
// success are the caller's context and `witnessAdoptedDeath` -- which
// concludes death only when the socket is gone AND the workspace lock reads
// FREE. A lock that reads HELD for a shim whose socket path is gone satisfies
// neither, so the ladder redials that forever, by design, in silence at DEBUG.
//
// Two runs of this have now been paid for. The boot sequence hit it first (a
// daemon that listened for ten hours with its accept queue at 128/128 and
// answered nothing) and bounded its own adoption. The fleet's did not, and the
// same shape then stranded a prompt: realtest 7's parent workspace took a
// prompt at 14:35:43, the queue held it under `session_starting` and started
// the background revival, the revival called Adopt, and between
// "adopting a running shim" and the three-minute give-up the workspace's log
// carried nothing at all -- no "adopted a running shim", no "adoption failed".
// A held prompt writes no `turns` row, so the harness polling `turns` saw
// nothing, and the fork that needed the parent's conversation had none.
//
// Sized as a small multiple of a healthy adoption, which is a local AF_UNIX
// connect plus the shim's first pushed diagnostics frame -- healthy or not,
// because an unhealthy arm is an ANSWER and adopts (awaitDiagnostics);
// milliseconds, and `shimsocket.DialTimeout` already bounds the connect at 2s.
// 10s is ~5x that one bounded connect, so a shim that is merely busy is still
// adopted and one that is unreachable costs a bounded wait and a loud refusal
// instead of a caller that never returns.
const DefaultAdoptBound = 10 * time.Second

// client is one shim connection AND, when it spawned the process, its
// supervisor. Adopted clients have no cmd: they supervise the LINK only, and
// their death evidence is the socket plus the workspace lock.
type client struct {
	log     dlog.Logger
	ws      ids.WorkspaceID
	udsPath string
	rpc     shimv1connect.ShimClient
	back    backoff
	grace   time.Duration

	// lockProbe answers whether the workspace's kernel lock reads FREE. It is
	// injected because sessionlock is shimclient's PEER, not its dependency;
	// nil means the caller supplied no death witness for an adopted shim, and
	// such a client then redials forever — never guessing death from a count.
	lockProbe func(ids.WorkspaceID) (bool, error)

	cmd    *exec.Cmd
	pgid   int
	stderr *ring

	// release takes this client out of the supervisor's spawn registry. It is
	// armed by supervisor.hold at cmd.Start and fired exactly once, from the
	// two places supervision of the PROCESS ends: its death (publishExit) and
	// its handover to a successor (Detach). Nil for an adopted client, which
	// the supervisor never started and therefore never held.
	release     func()
	releaseOnce sync.Once

	link *linkFeed
	exit chan ExitInfo
	dead chan struct{}

	// standDown latches the moment THIS DAEMON asks this shim to go: a
	// KillSession, or a Kill of the process itself.
	//
	// BOTH VERBS ARM IT, because both are teardowns this daemon ordered. Only
	// the rpc did, so a teardown that went straight to the process — Forget
	// and Nuke stand a session down through `Fleet.Stop`, which calls Kill —
	// left every consumer reading an ordered departure as an unasked one.
	//
	// A SHIM ENDS ITS PROCESS ON KillSession -- the real one and the fake one
	// both do -- so the supervised liveness stream breaking afterwards is this
	// daemon's own act arriving back at it. Untold, the monitor read that
	// break as a transport fault: WARN "shim link broke; redialing", a redial
	// into a dying process, and a `link_severed` health fault raised against
	// an orderly stand-down. The session watcher is already told through
	// SessionEnding (internal/workspace/fleet_rollout.go); this is the same
	// courtesy for the SUPERVISOR's own stream, which nothing was telling.
	standDown atomic.Bool

	// daemonStandDown reads the SUPERVISOR's own stand-down latch, and it is
	// the second half of the same signal `standDown` above is the first half
	// of. A latch armed per CLIENT only ever covers the clients a teardown
	// walk can name, and an immediate shutdown reaches processes no walk names:
	//
	//   MEASURED, realtest run 2026-09-13T18:31:58. A bring-up whose
	//   StartSession refused left its spawned shim serving (that is the
	//   INERT SURVIVOR the next bring-up adopts), so ONE process ended up
	//   with TWO clients -- the supervisor's spawn record and the fleet's
	//   adopted one. `UpdateShutdownSchedule{now}` swept the spawn record,
	//   which armed ITS latch and recorded the exit at INFO; the adopted
	//   client, armed by nothing, witnessed the very same departure as
	//   `daemon.shimclient.exit` ERROR "shim died" plus
	//   `daemon.shimclient.redial` WARN "redial stopped", eight times over
	//   the run.
	//
	// So the question every consumer asks is not "was THIS client stood
	// down" but "did THIS DAEMON order the departure", and the supervisor's
	// latch is what answers it for every client it ever handed out. Nil for a
	// client built outside a supervisor, which orders nothing.
	daemonStandDown func() bool

	monitorCtx    context.Context
	cancelMonitor context.CancelFunc

	// sigMu serializes signalling against the reap. A kill may only reach the
	// process group WHILE the group's leader — our own child — is unreaped,
	// because the instant cmd.Wait returns the kernel may hand that pid, and
	// with it the group id, to a stranger. reaped is set under this lock the
	// moment Wait returns, so a signal and a reap can never interleave.
	sigMu  sync.Mutex
	reaped bool

	mu          sync.Mutex
	pid         int
	occupant    string
	detached    bool
	exited      bool
	attribution *KillAttribution
	exitInfo    *ExitInfo
}

// newClient builds an unstarted client for one shim socket.
//
// THE DAEMON-WIDE LATCH IS A CONSTRUCTOR ARGUMENT, not a field a caller
// remembers to set afterwards. Every client this daemon hands out — spawned or
// ADOPTED, on any path — must be able to answer "did this daemon order the
// departure", and a client that was handed the reader only on some of the
// paths that build one is exactly the adopted client whose ordered departure
// was recorded as ERROR "shim died" with `stand_down_asked: false`. A nil
// reader is a client built outside a supervisor, which orders nothing.
func newClient(log dlog.Logger, ws ids.WorkspaceID, udsPath string, back backoff, probe func(ids.WorkspaceID) (bool, error), daemonStandDown func() bool) *client {
	ctx, cancel := context.WithCancel(context.Background())
	return &client{
		daemonStandDown: daemonStandDown,
		log:             log,
		ws:              ws,
		udsPath:         udsPath,
		rpc:             shimv1connect.NewShimClient(newUDSClient(udsPath), udsBaseURL),
		back:            back,
		grace:           DefaultKillGrace,
		lockProbe:       probe,
		link:            newLinkFeed(),
		exit:            make(chan ExitInfo, 1),
		dead:            make(chan struct{}),
		monitorCtx:      ctx,
		cancelMonitor:   cancel,
	}
}

// ---- supervision ----

// PID is the supervised process's pid, or 0 when it is not known — an adopted
// shim whose lock holder yielded none.
func (c *client) PID() int {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.pid
}

// Exited yields exactly one ExitInfo when the process is gone, then closes.
func (c *client) Exited() <-chan ExitInfo { return c.exit }

// Reaped answers the decoded exit without consuming Exited.
func (c *client) Reaped() (ExitInfo, bool) {
	c.mu.Lock()
	defer c.mu.Unlock()
	if c.exitInfo == nil {
		return ExitInfo{}, false
	}
	return *c.exitInfo, true
}

// ErrStandDownOrdered marks a shim call that failed because THIS DAEMON had
// already asked the shim to stand down. It is the difference between a shim
// that broke and a shim that did what it was told: the call still fails and
// the error is still returned, but every consumer that reports the failure can
// tell which of the two it is looking at.
//
// MEASURED, realtest run 2026-09-13T16:20:34. A deploy's SIGTERM landed inside
// the boot's own bring-up, the drain force-stopped the workspace's shim, and
// the StartSession that was in flight to that shim came back `unavailable:
// unexpected EOF` -- recorded as an ERROR by the client, again by the fleet
// ("the StartSession call failed") and a third time by the boot ("an open
// workspace's session did not come up"), on three consecutive daemon
// generations, for a teardown the same process had ordered nine milliseconds
// earlier and recorded at info on the line above.
var ErrStandDownOrdered = errors.New("the shim was stood down by this daemon")

// StandingDown answers the stand-down latch. It is the shim's own record that
// THIS DAEMON asked it to end its session, and it is read by every consumer
// that must tell a teardown it ordered from one that happened to it.
//
// IT IS THE DAEMON'S LATCH TOO, not only this client's: see daemonStandDown.
// A daemon that has begun standing down is ending EVERY shim it holds, so a
// departure that lands after that moment is one it ordered whether or not the
// teardown walk reached this particular client.
func (c *client) StandingDown() bool {
	asked, daemon := c.standDownLatches()
	return asked || daemon
}

// standDownLatches reports the two halves of the stand-down signal separately:
// `asked` is this client's own record that a teardown was asked OF IT, and
// `daemon` is the supervisor's daemon-wide latch. Every record that reports a
// departure states BOTH, because "this daemon ordered it" and "the walk
// reached this client" are different facts and a reader that is given only the
// first cannot tell an unnamed client's ordered departure from a crash.
func (c *client) standDownLatches() (asked, daemon bool) {
	return c.standDown.Load(), c.daemonStandDown != nil && c.daemonStandDown()
}

// standDownFields states both latches on a record.
func (c *client) standDownFields(ctx dlog.Context) dlog.Context {
	asked, daemon := c.standDownLatches()
	ctx["stand_down_asked"] = asked
	ctx["daemon_stand_down"] = daemon
	return ctx
}

// StandDown arms the stand-down latch for a teardown this daemon is ordering,
// answering whether it was armed. See Client.StandDown.
func (c *client) StandDown() bool {
	c.mu.Lock()
	detached := c.detached
	c.mu.Unlock()
	if detached {
		return false
	}
	c.standDown.Store(true)
	return true
}

// Connectivity yields every link state change: dialing, connected, redialing,
// dead.
func (c *client) Connectivity() <-chan LinkState { return c.link.states() }

// Occupy takes the in-memory occupancy guard, returning the release function.
// A second holder is REFUSED, named against the current one.
func (c *client) Occupy(holder string) (func(), error) {
	if holder == "" {
		return nil, invalid("Occupy", "holder", "is empty")
	}
	c.mu.Lock()
	if c.occupant != "" {
		current := c.occupant
		c.mu.Unlock()
		c.log.Warn("daemon.shimclient.occupy", "occupancy refused", dlog.Context{
			"workspace_id": string(c.ws), "holder": holder, "occupant": current,
		})
		return nil, &OccupiedError{Holder: current, Requested: holder}
	}
	c.occupant = holder
	c.mu.Unlock()

	c.log.Debug("daemon.shimclient.occupy", "occupancy taken", dlog.Context{
		"workspace_id": string(c.ws), "holder": holder,
	})
	var once sync.Once
	return func() {
		once.Do(func() {
			c.mu.Lock()
			c.occupant = ""
			c.mu.Unlock()
			c.log.Debug("daemon.shimclient.occupy", "occupancy released", dlog.Context{
				"workspace_id": string(c.ws), "holder": holder,
			})
		})
	}, nil
}

// Kill stops the process group, recording who asked and why: SIGTERM, a
// bounded wait, then SIGKILL, and the reaper decodes the exit either way.
//
// AN ADOPTED SHIM IS STOPPED TOO, down its own path — see killAdopted. It used
// to be refused with ErrNoProcess, which made a successor daemon unable to
// stand down the very shims a handover had just given it.
//
// CTX BOUNDS THE WAITS, AND ONLY THE WAITS. This call took no context at all
// and blocked unconditionally on the reap, which made every bound its callers
// held a fiction: `drain`'s stand-down handed it a 5s context and then sat
// through the 5s grace plus the escalation plus the reap regardless. There are
// exactly two waits here and ctx now selects against both.
//
// A CONTEXT THAT EXPIRES MID-GRACE ESCALATES; IT DOES NOT ABANDON. The caller
// saying "your time is up" cannot mean "leave a SIGTERMed shim running", so the
// expiry converts the graceful stop into a forced one: the SIGKILL goes out
// before this returns, and the error names both the escalation and the cause.
//
// THE REAP ITSELF IS NOT CANCELLABLE, and that is a different thing from the
// WAIT for it. `cmd.Wait` runs on the client's own reaper goroutine, started at
// the spawn and owned by nothing a caller holds; ctx ends this function's wait
// for that goroutine's result, never the goroutine. So a caller whose bound
// expires after the SIGKILL gets its answer immediately AND the wait status is
// still collected, which is the only reason the daemon does not accumulate a
// zombie per abandoned kill.
func (c *client) Kill(ctx context.Context, attr KillAttribution) error {
	c.mu.Lock()
	if c.detached {
		c.mu.Unlock()
		return ErrDetached
	}
	// THE LATCH IS ARMED BEFORE ANYTHING IS ENDED, and by the PROCESS kill and
	// not only by KillSession.
	//
	// A kill is by construction a teardown THIS DAEMON ordered, so every side
	// that later sees the departure — the liveness monitor, the redialer, the
	// adopted-death witness — must read it as ordinary. Only KillSession armed
	// it, so a teardown that goes straight to the process armed nothing:
	// Forget stands a session down through `Fleet.Stop`, which calls Kill, and
	// forgetting a live ADOPTED workspace therefore recorded
	// `daemon.shimclient.exit` ERROR "adopted shim is gone" plus two
	// `daemon.shimclient.redial` WARNs for a stand-down the daemon itself
	// ordered and the shim performed exactly as asked.
	//
	// It is armed before the branches below because a shim that is already
	// gone, and an adopted one with no child handle, are both still departures
	// this daemon asked for.
	c.standDown.Store(true)
	switch {
	case c.exited:
		c.mu.Unlock()
		c.log.Debug("daemon.shimclient.kill", "process already gone", dlog.Context{
			"workspace_id": string(c.ws), "actor": attr.Actor,
		})
		return nil
	case c.cmd == nil:
		// ADOPTED: no child handle exists to signal, so the pid is read from
		// the socket's peer credential at the moment of the kill.
		c.attribution = &attr
		grace := c.grace
		c.mu.Unlock()
		return c.killAdopted(ctx, attr, grace)
	case c.pgid == 0:
		c.mu.Unlock()
		return ErrNoProcess
	}
	c.attribution = &attr
	pgid := c.pgid
	grace := c.grace
	c.mu.Unlock()

	c.log.Info("daemon.shimclient.kill", "stopping shim", dlog.Context{
		"workspace_id": string(c.ws), "pgid": pgid,
		"actor": attr.Actor, "reason": attr.Reason, "force": attr.Force,
	})

	var expired error
	if !attr.Force {
		gone, err := c.signalGroup(syscall.SIGTERM, pgid)
		if err != nil {
			c.log.Error("daemon.shimclient.kill", "SIGTERM failed", dlog.Context{
				"workspace_id": string(c.ws), "pgid": pgid, "error": err.Error(),
			})
			return fmt.Errorf("shimclient: SIGTERM %d: %w", pgid, err)
		}
		if gone {
			// GONE IS NOT REAPED. A group that has already left still owes this
			// daemon its exit decode, and until the reaper delivers it the
			// supervisor still holds the client -- so an immediate shutdown's
			// spawn sweep, which runs the instant the stand-down walk returns,
			// found a session it had just stood down and recorded "a spawn
			// this daemon never registered is being stood down" against it.
			// The wait is the same one the ordinary path takes below, and it
			// is already over whenever the reaper got there first.
			return c.awaitReap(ctx, pgid, "the process group was already gone")
		}
		timer := time.NewTimer(grace)
		defer timer.Stop()
		select {
		case <-c.dead:
			return nil
		case <-timer.C:
			c.log.Warn("daemon.shimclient.kill", "graceful stop timed out; escalating", dlog.Context{
				"workspace_id": string(c.ws), "pgid": pgid, "grace_ms": grace.Milliseconds(),
			})
		case <-ctx.Done():
			// THE CALLER'S BOUND EXPIRED INSIDE THE GRACE. Escalating anyway is
			// the only answer that leaves no shim behind: a return here would
			// hand back a process that has been asked to leave and given
			// nothing that makes it. The error below says so; the SIGKILL goes
			// out first.
			expired = ctx.Err()
			c.log.Warn("daemon.shimclient.kill", "the caller's bound expired inside the grace; escalating", dlog.Context{
				"workspace_id": string(c.ws), "pgid": pgid, "grace_ms": grace.Milliseconds(),
				"error": expired.Error(),
			})
		}
	}

	// THE REAP IS WHAT ENDS THIS CALL, whether or not the group had already
	// left: see the SIGTERM branch above. Kill's contract is that the process
	// is gone AND this daemon no longer owns one when it returns, and the
	// deregistration rides the exit decode.
	if _, err := c.signalGroup(syscall.SIGKILL, pgid); err != nil {
		c.log.Error("daemon.shimclient.kill", "SIGKILL failed", dlog.Context{
			"workspace_id": string(c.ws), "pgid": pgid, "error": err.Error(),
		})
		return fmt.Errorf("shimclient: SIGKILL %d: %w", pgid, err)
	}
	if expired != nil {
		// THE SIGKILL HAS LEFT, SO THE PROCESS IS GOING; the reaper will
		// collect its wait status whether or not anyone is still waiting here.
		// The caller asked for its answer by now and gets it, named as the
		// escalation it was rather than as a plain deadline.
		return fmt.Errorf("shimclient: pgid %d was SIGKILLed because the kill's context ended inside the %s grace; the reap continues: %w",
			pgid, grace, expired)
	}
	return c.awaitReap(ctx, pgid, "the process group was SIGKILLed")
}

// awaitReap waits for the reaper's exit decode, on the caller's bound.
//
// THE WAIT IS CANCELLABLE AND THE REAP IS NOT. `cmd.Wait` is already running on
// the client's own goroutine; ending this wait ends only the caller's interest
// in its result, so no abandoned kill can leave a zombie behind. The overrun is
// REPORTED rather than swallowed: a caller that never learns the exit landed is
// a caller that must treat the workspace as still occupied.
func (c *client) awaitReap(ctx context.Context, pgid int, why string) error {
	select {
	case <-c.dead:
		return nil
	case <-ctx.Done():
		c.log.Warn("daemon.shimclient.kill", "the caller's bound expired before the exit decode landed; the reap continues", dlog.Context{
			"workspace_id": string(c.ws), "pgid": pgid, "why": why, "error": ctx.Err().Error(),
		})
		return fmt.Errorf("shimclient: %s for pgid %d but its exit decode did not land inside the caller's bound; the reap continues: %w",
			why, pgid, ctx.Err())
	}
}

// adoptedGonePoll is how often an adopted process is re-checked for having
// gone. It is a poll because the daemon is NOT its parent and therefore has no
// wait(2) to block in; kill(pgid, 0) is the only observation available, and
// 25ms is the same interval the rollout's adoption look uses for the same kind
// of "somebody else's process changed state" question.
const adoptedGonePoll = 25 * time.Millisecond

// killAdopted stops a shim this daemon did not spawn: the successor's half of
// a handover, and a crash boot's surviving process.
//
// WHY IT EXISTS. Kill answered ErrNoProcess for every adopted client, because
// the only pid it knew was cmd.Process.Pid and an adopted client has no cmd.
// So a successor that took a handover could not stand down the shims the
// handover had just given it: Fleet.Stop returned the error, the immediate
// shutdown recorded it and exited anyway, and the shim ran on holding the
// workspace lock that refuses the next session plus ~95 MiB. Measured on the
// Emacs e2e layer's handover scenario: one node shim and two `shim-lock'
// holders outliving every daemon in the scenario. The same hole swallowed the
// idle sweeper's hibernation and the kill verb for any adopted workspace.
//
// THE SIGNAL IS THE ONLY WAY, and that is the shim's own contract rather than
// this package's preference: "SIGTERM is the ONE authorized process-level
// shutdown ... It cannot be an rpc-only path because the daemon may already be
// dead" (agent-shim/claude/shim/src/main.ts). KillSession ends the SESSION; it
// does not end the process.
//
// THE PID IS READ FROM THE KERNEL AT THE MOMENT OF THE KILL, off a fresh
// connection to the shim's own socket, so what is signalled is by construction
// the process serving that socket right now. A pid remembered from adoption
// could have been recycled in between; this one cannot be, because a recycled
// pid is not bound to the socket. The group is then required to be led by that
// same pid — the spawn contract sets Setpgid, so a shim always leads its own
// group — and a peer that does not is REFUSED rather than signalled, because
// signalling a group we cannot account for could reach the daemon's own.
func (c *client) killAdopted(ctx context.Context, attr KillAttribution, grace time.Duration) error {
	pid, err := socketPeerPID(c.udsPath)
	if err != nil {
		if isSocketGone(err) {
			// The socket refuses or is absent: the shim the caller asked to
			// stop is already gone. That is the state they asked for.
			c.log.Info("daemon.shimclient.kill", "the adopted shim's socket is gone; nothing to stop", dlog.Context{
				"workspace_id": string(c.ws), "uds": c.udsPath, "actor": attr.Actor,
			})
			c.publishExit(ExitInfo{
				PID:      c.PID(),
				Code:     -1,
				Inferred: true,
				Stderr:   "adopted shim: the socket was already gone when the daemon went to stop it",
			})
			return nil
		}
		c.log.Error("daemon.shimclient.kill", "could not learn the adopted shim's pid; it cannot be stopped", dlog.Context{
			"workspace_id": string(c.ws), "uds": c.udsPath, "error": err.Error(),
		})
		return fmt.Errorf("shimclient: stop the adopted shim for %q: %w", c.ws, err)
	}
	pgid, err := syscall.Getpgid(pid)
	if err != nil {
		c.log.Error("daemon.shimclient.kill", "could not read the adopted shim's process group", dlog.Context{
			"workspace_id": string(c.ws), "pid": pid, "error": err.Error(),
		})
		return fmt.Errorf("shimclient: process group of the adopted shim %d for %q: %w", pid, c.ws, err)
	}
	if pgid != pid {
		c.log.Error("daemon.shimclient.kill", "refused to signal an adopted shim that does not lead its own process group", dlog.Context{
			"workspace_id": string(c.ws), "pid": pid, "pgid": pgid,
		})
		return fmt.Errorf("shimclient: adopted shim %d for %q leads no process group of its own (pgid %d)", pid, c.ws, pgid)
	}
	// AND IT IS NEVER THE DAEMON'S OWN GROUP. The two checks together make
	// signalling ourselves unrepresentable rather than merely unlikely: a peer
	// that is this process passes the leadership check only when this process
	// leads its group, and that is exactly the case this refuses. A daemon
	// that SIGKILLs its own group takes down every shim on the machine and
	// itself, so the answer is a loud refusal, never a signal sent hopefully.
	if pgid == syscall.Getpgrp() {
		c.log.Error("daemon.shimclient.kill", "refused to signal the daemon's own process group as an adopted shim", dlog.Context{
			"workspace_id": string(c.ws), "pid": pid, "pgid": pgid, "self": os.Getpid(),
		})
		return fmt.Errorf("shimclient: the peer of %q's socket (pid %d) is in this daemon's own process group %d", c.ws, pid, pgid)
	}

	c.mu.Lock()
	c.pid = pid
	c.mu.Unlock()

	c.log.Info("daemon.shimclient.kill", "stopping an adopted shim", dlog.Context{
		"workspace_id": string(c.ws), "pid": pid, "pgid": pgid,
		"actor": attr.Actor, "reason": attr.Reason, "force": attr.Force,
	})

	signal := syscall.SIGKILL
	if !attr.Force {
		signal = syscall.SIGTERM
	}
	if err := c.signalAdoptedGroup(signal, pgid); err != nil {
		return err
	}
	if c.awaitAdoptedGone(ctx, pgid, grace) {
		c.publishAdoptedKill(pid, signal)
		return nil
	}
	if signal == syscall.SIGKILL {
		// SIGKILL is not negotiable, so a group still standing after the
		// grace is a process the kernel is holding (an uninterruptible wait),
		// not one that declined. It is REPORTED: the caller's record is the
		// only thing that will ever say the workspace lock is still held.
		return fmt.Errorf("shimclient: adopted shim %d for %q did not go down within %s of SIGKILL", pid, c.ws, grace)
	}
	c.log.Warn("daemon.shimclient.kill", "the adopted shim ignored SIGTERM; escalating", dlog.Context{
		"workspace_id": string(c.ws), "pid": pid, "pgid": pgid, "grace_ms": grace.Milliseconds(),
	})
	if err := c.signalAdoptedGroup(syscall.SIGKILL, pgid); err != nil {
		return err
	}
	// THE SIGKILL IS SENT WHATEVER THE CALLER'S BOUND SAYS, exactly as on the
	// supervised path: a caller running out of time cannot mean a SIGTERMed
	// shim is left standing. Only the wait that FOLLOWS it is the caller's to
	// bound, and an adopted process has no reap of ours to leak — the kernel
	// gave its wait status to init the moment we were not its parent.
	if !c.awaitAdoptedGone(ctx, pgid, grace) {
		return fmt.Errorf("shimclient: adopted shim %d for %q did not go down within %s of SIGKILL", pid, c.ws, grace)
	}
	c.publishAdoptedKill(pid, syscall.SIGKILL)
	return nil
}

// signalAdoptedGroup sends one signal to an adopted shim's process group. A
// group that is already gone is SUCCESS — the caller asked for it to stop —
// and every other errno is returned.
func (c *client) signalAdoptedGroup(sig syscall.Signal, pgid int) error {
	if err := syscall.Kill(-pgid, sig); err != nil {
		if errors.Is(err, syscall.ESRCH) {
			return nil
		}
		c.log.Error("daemon.shimclient.kill", "signalling the adopted shim's process group failed", dlog.Context{
			"workspace_id": string(c.ws), "pgid": pgid, "signal": sig.String(), "error": err.Error(),
		})
		return fmt.Errorf("shimclient: %s the adopted process group %d for %q: %w", sig, pgid, c.ws, err)
	}
	return nil
}

// awaitAdoptedGone polls until the process group holds nothing, or the bound
// expires, or the caller's context ends. It reports whether the group is gone.
//
// THE CALLER'S CONTEXT ENDS THE POLL, not the kill: whoever called has already
// sent the signal by the time this runs, so giving up here abandons an
// OBSERVATION and never a process.
func (c *client) awaitAdoptedGone(ctx context.Context, pgid int, bound time.Duration) bool {
	deadline := time.Now().Add(bound)
	poll := time.NewTicker(adoptedGonePoll)
	defer poll.Stop()
	for {
		if err := syscall.Kill(-pgid, 0); errors.Is(err, syscall.ESRCH) {
			return true
		}
		if !time.Now().Before(deadline) {
			return false
		}
		select {
		case <-poll.C:
		case <-ctx.Done():
			return false
		}
	}
}

// publishAdoptedKill records the death of a shim the daemon stopped but never
// parented. There is no wait status to decode — the kernel gave it to init —
// so the SIGNAL this daemon sent is the whole of the evidence, and the exit
// says exactly that rather than inventing a code.
func (c *client) publishAdoptedKill(pid int, sig syscall.Signal) {
	c.publishExit(ExitInfo{
		PID:    pid,
		Code:   -1,
		Signal: sig.String(),
		Stderr: "adopted shim: stopped by this daemon; no wait status, the process was never its child",
	})
}

// signalGroup signals the supervised child's process group, and can only ever
// reach OUR OWN child's group. Two things make that structural: the group was
// created by us (the spawn sets Setpgid, so the leader is the child itself, and
// a group id cannot be recycled while its leader is unreaped), and the signal
// is delivered under sigMu, which the reap takes the instant cmd.Wait returns.
// A child that is already gone is SUCCESS, never a kill failure — including the
// kernel's EPERM, which on a recycled pid means the process is not ours.
func (c *client) signalGroup(sig syscall.Signal, pgid int) (gone bool, err error) {
	c.mu.Lock()
	proc := (*os.Process)(nil)
	if c.cmd != nil {
		proc = c.cmd.Process
	}
	c.mu.Unlock()

	// A pid we never owned is refused LOUDLY, without signalling anything: an
	// adopted shim, or a pgid that does not belong to the retained handle.
	if proc == nil || proc.Pid != pgid {
		held := 0
		if proc != nil {
			held = proc.Pid
		}
		c.log.Error("daemon.shimclient.kill", "refused to signal a pid we do not own", dlog.Context{
			"workspace_id": string(c.ws), "pgid": pgid, "held_pid": held,
		})
		return false, ErrNoProcess
	}

	c.sigMu.Lock()
	defer c.sigMu.Unlock()
	if c.reaped {
		c.log.Debug("daemon.shimclient.kill", "child already reaped; nothing signaled", dlog.Context{
			"workspace_id": string(c.ws), "pgid": pgid, "signal": sig.String(),
		})
		return true, nil
	}
	if err := syscall.Kill(-pgid, sig); err != nil {
		if errors.Is(err, syscall.ESRCH) || errors.Is(err, syscall.EPERM) {
			c.log.Debug("daemon.shimclient.kill", "process group already gone", dlog.Context{
				"workspace_id": string(c.ws), "pgid": pgid,
				"signal": sig.String(), "errno": err.Error(),
			})
			return true, nil
		}
		return false, err
	}
	return false, nil
}

// markReaped records, under the signal lock, that cmd.Wait has returned and the
// child's pid is therefore free for reuse. No signal may follow it.
func (c *client) markReaped() {
	c.sigMu.Lock()
	c.reaped = true
	c.sigMu.Unlock()
}

// Detach stops supervising while LEAVING THE PROCESS RUNNING — the handover's
// per-workspace transfer. Nothing is signaled and no exit is ever published.
func (c *client) Detach() {
	c.mu.Lock()
	if c.detached {
		c.mu.Unlock()
		return
	}
	c.detached = true
	pid := c.pid
	c.mu.Unlock()

	c.cancelMonitor()
	c.link.close()
	// THE HANDOVER LEAVES THE SUPERVISOR'S REGISTRY TOO. This process is now
	// the successor's to adopt, and a `now` shutdown's sweep must not find it:
	// the registry is what tells a transferred shim from an in-flight spawn.
	c.releaseHold()
	c.log.Info("daemon.shimclient.detach", "supervision handed over; process left running", dlog.Context{
		"workspace_id": string(c.ws), "pid": pid,
	})
}

// reap waits for the spawned process, decodes its exit, and publishes the
// evidence. It is the ONLY place a spawned shim's death is decided.
func (c *client) reap() {
	err := c.cmd.Wait()
	c.markReaped()

	c.mu.Lock()
	if c.detached {
		c.mu.Unlock()
		return
	}
	c.mu.Unlock()

	info := ExitInfo{PID: c.PID(), Stderr: c.stderr.String()}
	switch {
	case err == nil:
		info.Code = 0
	default:
		var exitErr *exec.ExitError
		if errors.As(err, &exitErr) {
			if status, ok := exitErr.Sys().(syscall.WaitStatus); ok {
				if status.Signaled() {
					info.Signal = status.Signal().String()
					info.Code = -1
				} else {
					info.Code = status.ExitStatus()
				}
			} else {
				info.Code = exitErr.ExitCode()
			}
		} else {
			info.Code = -1
			info.Stderr = info.Stderr + "\nwait failed: " + err.Error()
		}
	}
	c.publishExit(info)
}

// publishExit records the decoded exit once: the link goes dead, the exit
// channel yields it and closes, and every redial stops because the EVIDENCE
// says so.
func (c *client) publishExit(info ExitInfo) {
	c.mu.Lock()
	if c.exited || c.detached {
		c.mu.Unlock()
		return
	}
	c.exited = true
	info.Attribution = c.attribution
	c.exitInfo = &info
	c.mu.Unlock()

	ctx := c.standDownFields(dlog.Context{
		"workspace_id": string(c.ws), "pid": info.PID, "code": info.Code,
		"signal": info.Signal, "stderr": info.Stderr,
	})
	if info.Attribution != nil {
		ctx["actor"] = info.Attribution.Actor
		ctx["reason"] = info.Attribution.Reason
		c.log.Info("daemon.shimclient.exit", "supervised shim stopped as asked", ctx)
	} else if c.StandingDown() && (info.Inferred || (info.Code == 0 && info.Signal == "")) {
		// THE SHIM ENDS ITS OWN PROCESS ON KillSession. Every graceful
		// stand-down in this daemon asks before it signals, so the ordinary
		// case is that the shim is already gone by the time anything would
		// have killed it -- there is no `Kill` and therefore no attribution,
		// and the exit arrives here unexplained. Recorded as a death, an
		// orderly relaunch bounce cost every realtest run an ERROR for a
		// teardown the daemon itself ordered and the shim performed exactly
		// as asked.
		//
		// THE CONDITIONS ARE ALL REQUIRED. A shim that was never asked to
		// stand down, one that exits nonzero, and one that was signalled all
		// reach the loud branch below unchanged: those are deaths however the
		// teardown was ordered, and the whole point is telling them apart.
		//
		// AN INFERRED DEPARTURE HAS NO STATUS TO JUDGE. An adopted shim is not
		// this daemon's child, so `Code` carries the -1 sentinel rather than a
		// wait status, and reading that sentinel as a signalled death reported
		// every ordered adopted teardown as a crash. `Inferred` says the
		// evidence is a vanished socket and nothing else; with no ask behind
		// it, it is still a death and still loud.
		c.log.Info("daemon.shimclient.exit", "the shim left after the stand-down it was asked for", ctx)
	} else {
		c.log.Error("daemon.shimclient.exit", "shim died", ctx)
	}

	// THE SUPERVISOR LETS GO BEFORE ANY WAITER IS WOKEN. `Kill` returns on
	// `dead`, and its contract is that the process is gone AND this daemon no
	// longer owns one; released afterwards, the registry still held this
	// client for as long as the reaper goroutine took to reach the next line,
	// and an immediate shutdown's spawn sweep -- which runs the instant the
	// stand-down walk returns -- swept a session it had just stood down and
	// recorded "a spawn this daemon never registered is being stood down"
	// against it (4 of 6 runs of
	// TestUpdateShutdownScheduleNowLeavesNoShimBehindEvenAtAPermissionGate).
	c.releaseHold()
	close(c.dead)
	c.link.publish(LinkDead)
	c.link.close()
	c.exit <- info
	close(c.exit)
	c.cancelMonitor()
}

// releaseHold fires the supervisor's deregistration exactly once. A client
// that was never held (an adopted one) has nothing to fire.
func (c *client) releaseHold() {
	if c.release == nil {
		return
	}
	c.releaseOnce.Do(c.release)
}

// killWithin forces the process down and waits for the reaper, on a bound.
//
// THE BOUND IS STATED TWICE, and both statements are needed. It is handed to
// Kill, which selects its waits against it; and this ALSO waits on it from the
// outside, because Kill's non-waiting steps are not all cancellable -- a
// `socketPeerPID` dial or a signal syscall does not consult a context -- and a
// child stopped in the kernel (a ptrace stop, an uninterruptible D state) must
// not turn an immediate shutdown into a daemon that never leaves. So the kill
// runs on its own goroutine and this waits on bound, whose value the caller
// states; the goroutine's channel is buffered, so a kill that lands after the
// bound has passed still completes and never leaks a blocked writer.
func (c *client) killWithin(ctx context.Context, bound time.Duration, attr KillAttribution) error {
	within, cancel := context.WithTimeout(ctx, bound)
	defer cancel()

	done := make(chan error, 1)
	go func() { done <- c.Kill(within, attr) }()

	select {
	case err := <-done:
		return err
	case <-within.Done():
		return fmt.Errorf("shimclient: pid %d did not go down within %s: %w", c.PID(), bound, within.Err())
	}
}

// exitedAlready reports whether death has already been decided.
func (c *client) exitedAlready() bool {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.exited
}

// ---- bring-up and the redial loop ----

// openSupervisedSession opens the client's own session stream — the link's
// liveness evidence — with the open SELECTED against process death, because
// Connect's server-stream open blocks until the shim's first frame and a dead
// process must end the wait at once.
func (c *client) openSupervisedSession(parent context.Context) (Stream[*shimv1.WatchSessionResponse], error) {
	type opened struct {
		stream Stream[*shimv1.WatchSessionResponse]
		err    error
	}
	result := make(chan opened, 1)
	// The stream is opened against the CLIENT's own supervision lifetime, not
	// the caller's: it is the link's liveness evidence and it outlives whoever
	// asked for the spawn (an OpenWorkspace rpc, a create, the relaunch
	// engine). Bound to the caller instead, the link "breaks" the instant that
	// verb answers and the redial ladder runs for no reason. The WAIT below is
	// still selected on the caller, so an abandoned bring-up returns at once.
	//
	// A refused open on this ladder is an ORDINARY branch, not a warning: the
	// shim's socket does not exist yet on the first attempt of every spawn.
	streamCtx := context.WithValue(c.monitorCtx, quietOpenKey{}, true)
	go func() {
		stream, err := c.watchSession(streamCtx)
		result <- opened{stream: stream, err: err}
	}()

	// An abandoned open still has to be closed when it eventually lands, or the
	// stream it opened would outlive the supervisor that abandoned it.
	abandon := func() {
		go func() {
			if r := <-result; r.err == nil {
				r.stream.Close()
			}
		}()
	}

	select {
	case <-parent.Done():
		abandon()
		return nil, parent.Err()
	case <-c.dead:
		abandon()
		return nil, c.deathError()
	case r := <-result:
		if r.err != nil {
			return nil, r.err
		}
		return r.stream, nil
	}
}

// redial re-establishes the link to a still-running shim, FOREVER with capped
// backoff. It stops only when the evidence says the process is gone or the
// supervision context ends.
func (c *client) redial(ctx context.Context) (Stream[*shimv1.WatchSessionResponse], error) {
	c.link.publish(LinkRedialing)
	for attempt := 0; ; attempt++ {
		if err := c.deathOrContext(ctx); err != nil {
			return nil, err
		}
		stream, err := c.openSupervisedSession(ctx)
		if err == nil {
			c.log.Info("daemon.shimclient.redial", "shim link re-established", dlog.Context{
				"uds": c.udsPath, "attempt": attempt,
			})
			c.link.publish(LinkConnected)
			return stream, nil
		}
		if stop := c.afterFailedDial(ctx, err, attempt); stop != nil {
			return nil, stop
		}
	}
}

// awaitDiagnostics consumes session frames until the FIRST diagnostics arm,
// whichever it is. A shim that pushes diagnostics has ANSWERED, and answering
// is what bring-up waits for: a shim standing on a fault it will not clear
// answers in milliseconds, and treating that answer as silence burned the
// caller's whole bound and left the workspace with no client at all — which is
// strictly worse than a client whose session carries faults, because a fault
// the daemon holds a client for is one the frontend gets to see.
//
// THE FAULTS ARE NOT SWALLOWED. They reach the workspace health path the same
// way an already-adopted shim's later verdict does: every WatchSession the
// shim serves opens with its current diagnostics, so the watcher the fleet
// attaches on install folds this same verdict into the session's faults.
//
// The bound the caller holds is therefore back to guarding the one condition
// it can: a shim that sends no diagnostics frame at all.
//
// The frames come from the ONE receive loop the stream has; a second loop on
// the same stream would be two concurrent receivers.
func (c *client) awaitDiagnostics(ctx context.Context, frames <-chan *shimv1.WatchSessionResponse, errs <-chan error) error {
	for {
		select {
		case <-ctx.Done():
			return ctx.Err()
		case <-c.dead:
			return c.deathError()
		case err := <-errs:
			return fmt.Errorf("shimclient: session stream ended during bring-up: %w", err)
		case frame := <-frames:
			diagnostics := frame.GetUpdate().GetDiagnostics()
			if diagnostics == nil {
				continue
			}
			if diagnostics.GetHealthy() != nil {
				c.log.Info("daemon.shimclient.ready", "shim reported healthy", dlog.Context{
					"workspace_id": string(c.ws), "uds": c.udsPath,
				})
				return nil
			}
			faults := diagnostics.GetUnhealthy().GetFaults()
			c.log.Warn("daemon.shimclient.ready", "shim reported unhealthy", dlog.Context{
				"workspace_id": string(c.ws), "faults": len(faults),
			})
			c.log.Info("daemon.shimclient.ready", "adopted an unhealthy shim; faults reported", dlog.Context{
				"workspace_id": string(c.ws), "uds": c.udsPath,
				"faults": len(faults), "fault_kinds": FaultKinds(faults),
			})
			return nil
		}
	}
}

// FaultKinds names the kind arm of every fault, comma-joined, so a record
// carries WHAT the shim is standing on rather than only how many things it is.
func FaultKinds(faults []*conversationv1.SessionFault) string {
	kinds := make([]string, 0, len(faults))
	for _, fault := range faults {
		kinds = append(kinds, FaultKind(fault))
	}
	return strings.Join(kinds, ",")
}

// FaultKind names the SessionFault kind arm the shim set. It is the daemon's
// ONE spelling of those arm names: the health reporter keeps it as a recorded
// fault's evidence and the adoption record logs it, and the two must never
// drift. An arm this build does not know is named rather than dropped, so a
// kind list and a fault count always agree.
func FaultKind(fault *conversationv1.SessionFault) string {
	switch fault.GetKind().(type) {
	case *conversationv1.SessionFault_StoreUnreachable:
		return "store_unreachable"
	case *conversationv1.SessionFault_ConverterDefect:
		return "converter_defect"
	case *conversationv1.SessionFault_LogSinkPoisoned:
		return "log_sink_poisoned"
	case *conversationv1.SessionFault_KeepaliveFailed:
		return "keepalive_failed"
	case *conversationv1.SessionFault_VendorQueryFailed:
		return "vendor_query_failed"
	default:
		return "unclassified"
	}
}

// monitor holds the session stream as the link's liveness evidence. A break
// while the process still lives is a REDIAL, forever, with capped backoff; a
// break with the process gone stops, because the evidence decided.
func (c *client) monitor(stream Stream[*shimv1.WatchSessionResponse], frames <-chan *shimv1.WatchSessionResponse, errs <-chan error) {
	ctx := c.monitorCtx
	for {
		var broke error
	consume:
		for {
			select {
			case <-ctx.Done():
				stream.Close()
				return
			case <-c.dead:
				stream.Close()
				return
			case err := <-errs:
				broke = err
				break consume
			case <-frames:
				// The link is alive. Session facts reach their consumers on
				// their OWN WatchSession; this stream is the liveness evidence.
			}
		}
		stream.Close()
		if c.exitedAlready() || ctx.Err() != nil {
			return
		}
		if c.StandingDown() {
			// THE DAEMON ASKED FOR THIS. A stand-down was requested of this
			// shim, so the liveness stream ending is the answer to it and not
			// a fault: redialing here reaches a process that is on its way
			// out, and publishing `redialing` raises a `link_severed` health
			// fault against a teardown the daemon itself ordered.
			c.log.Debug("daemon.shimclient.redial", "the liveness stream ended after a stand-down was asked of this shim; not redialing", c.standDownFields(dlog.Context{
				"uds": c.udsPath, "error": errText(broke),
			}))
			c.awaitAdoptedExit(ctx)
			return
		}
		c.log.Warn("daemon.shimclient.redial", "shim link broke; redialing", c.standDownFields(dlog.Context{
			"uds": c.udsPath, "error": errText(broke),
		}))
		next, err := c.redial(ctx)
		if err != nil {
			// THE LATCH IS RE-READ HERE, and that is not the same read as the
			// one above. A stand-down asked WHILE this dial ladder was already
			// climbing arms the latch after the gate has been passed, so the
			// ladder ends on a process the daemon itself just ended; without
			// this second read that ordered ending is a WARN, and the realtest
			// harvest fails a run on every one of them.
			if c.StandingDown() {
				c.log.Debug("daemon.shimclient.redial", "the redial ladder ended after a stand-down was asked of this shim", c.standDownFields(dlog.Context{
					"uds": c.udsPath, "error": err.Error(),
				}))
				c.awaitAdoptedExit(ctx)
				return
			}
			c.log.Warn("daemon.shimclient.redial", "redial stopped", c.standDownFields(dlog.Context{
				"uds": c.udsPath, "error": err.Error(),
			}))
			return
		}
		stream = next
		frames, errs = recvLoop(stream, ctx.Done())
	}
}

// awaitAdoptedExit decides an ADOPTED shim's exit once it has been asked to
// stand down: it polls the shim's process until the kernel says it is gone,
// then publishes the exit. A spawned client returns at once, because its reap
// decides its exit.
//
// WITHOUT IT AN ADOPTED SHIM'S EXIT WAS NEVER SEEN. The daemon is not its
// parent, so nothing waits on it, and once a stand-down was asked the monitor
// stopped redialing -- the only other path that could witness the death. The
// relaunch engine's reap gate then read `Exited` for the whole stand-down
// window and force-killed a shim that had left 29 seconds earlier (live
// handover 2026-09-24T18:06: 30s of held prompts for three workspaces).
//
// THE EVIDENCE IS THE PROCESS, not the lock. The shim releases its kernel
// locks as its session ends, before the process leaves, and the gate this
// serves promises the old process is gone. By the spawn contract the shim
// leads its own process group, so the GROUP being empty also covers its
// `shim-lock` holders; a peer that leads no group is watched by its pid alone.
// A recycled pid can only make a dead shim look alive, never the reverse, so
// the error is on the side of waiting, and the caller's window still bounds it.
//
// It ends with ctx (the client's supervision lifetime: a detach, or an exit
// decided elsewhere, such as the relaunch's force-kill).
func (c *client) awaitAdoptedExit(ctx context.Context) {
	c.mu.Lock()
	spawned := c.cmd != nil
	pid := c.pid
	c.mu.Unlock()
	if spawned {
		return
	}
	if pid <= 0 {
		c.log.Info("daemon.shimclient.exit", "the adopted shim's pid is unknown; its exit is decided by the socket or a kill", dlog.Context{
			"workspace_id": string(c.ws), "uds": c.udsPath,
		})
		return
	}
	target := pid
	if pgid, err := syscall.Getpgid(pid); err == nil && pgid == pid {
		target = -pid
	}
	poll := time.NewTicker(adoptedGonePoll)
	defer poll.Stop()
	for {
		if err := syscall.Kill(target, 0); errors.Is(err, syscall.ESRCH) {
			c.log.Debug("daemon.shimclient.exit", "the adopted shim's process is gone after the stand-down it was asked for", dlog.Context{
				"workspace_id": string(c.ws), "pid": pid, "group": target < 0,
			})
			c.publishExit(ExitInfo{
				PID:      pid,
				Code:     -1,
				Inferred: true,
				Stderr:   "adopted shim: its process is gone; no wait status, the process was never this daemon's child",
			})
			return
		}
		select {
		case <-poll.C:
		case <-ctx.Done():
			return
		case <-c.dead:
			return
		}
	}
}

// witnessAdoptedDeath reports whether a dial failure is EVIDENCE that an
// adopted shim is gone: the socket refuses or is absent AND the workspace
// lock reads FREE. Without the injected witness nothing is concluded.
func (c *client) witnessAdoptedDeath(dialErr error) bool {
	c.mu.Lock()
	spawned := c.cmd != nil
	c.mu.Unlock()
	if spawned || c.lockProbe == nil || !isSocketGone(dialErr) {
		return false
	}
	free, err := c.lockProbe(c.ws)
	if err != nil {
		c.log.Warn("daemon.shimclient.redial", "lock probe could not tell; still redialing", dlog.Context{
			"workspace_id": string(c.ws), "error": err.Error(),
		})
		return false
	}
	if !free {
		return false
	}
	// A DEPARTURE THIS DAEMON ORDERED IS AN ORDINARY EVENT HERE TOO. The
	// stand-down latch is the one signal every side reads, and this witness
	// read none of it: a forget or a kill of a live adopted workspace ends the
	// very socket this dial is failing on, and the failure was recorded as a
	// shim that went missing.
	evidence := c.standDownFields(dlog.Context{
		"workspace_id": string(c.ws), "uds": c.udsPath, "error": dialErr.Error(),
	})
	if c.StandingDown() {
		c.log.Debug("daemon.shimclient.exit", "the adopted shim's socket is gone after the stand-down it was asked for", evidence)
	} else {
		c.log.Error("daemon.shimclient.exit", "adopted shim is gone: socket refused and workspace lock free", evidence)
	}
	c.publishExit(ExitInfo{
		PID:      c.PID(),
		Code:     -1,
		Inferred: true,
		Stderr:   "adopted shim: no exit observed; socket refused and the workspace lock read free",
	})
	return true
}

// deathOrContext answers with the reason to stop before dialing again.
func (c *client) deathOrContext(ctx context.Context) error {
	select {
	case <-ctx.Done():
		return ctx.Err()
	case <-c.dead:
		return c.deathError()
	default:
		return nil
	}
}

// deathError is the bring-up-ending error a dead process produces, carrying
// its exit decoding and stderr ring.
func (c *client) deathError() error {
	c.mu.Lock()
	info := c.exitInfo
	c.mu.Unlock()
	if info == nil {
		return errProcessDead
	}
	return &BringUpDeathError{Exit: *info}
}

// recvLoop pumps one stream into a frame channel and a one-shot error channel,
// so a blocking Recv can be SELECTED against process death.
func recvLoop[T any](stream Stream[T], done <-chan struct{}) (<-chan T, <-chan error) {
	frames := make(chan T)
	errs := make(chan error, 1)
	go func() {
		for {
			frame, err := stream.Recv()
			if err != nil {
				errs <- err
				return
			}
			select {
			case frames <- frame:
			case <-done:
				return
			}
		}
	}()
	return frames, errs
}

// isSocketGone reports whether an error means the process serving the unix
// socket is not there: ECONNREFUSED or ENOENT on the dial, and ENOTCONN on a
// connection that was made and then lost before anything could be read off it.
//
// ENOTCONN IS THE SAME EVIDENCE ARRIVING ONE INSTANT LATER. A dial to a shim
// that is on its way out can win the race with the shim's exit and hand back a
// connected socket whose peer is already gone; the kernel then answers every
// question about that peer -- `LOCAL_PEERPID' on Darwin, a read on either
// platform -- with ENOTCONN. Reading that as a hard failure made `killAdopted'
// report "could not learn the adopted shim's pid; it cannot be stopped" about a
// shim that had just stopped itself, which is the one thing a caller asking for
// a stop cannot act on. There is no other way for a socket handed back by a
// successful `net.Dial' to be unconnected, so the arm is not broader than the
// evidence it names.
func isSocketGone(err error) bool {
	if err == nil {
		return false
	}
	if errors.Is(err, syscall.ECONNREFUSED) || errors.Is(err, syscall.ENOENT) || errors.Is(err, os.ErrNotExist) || errors.Is(err, syscall.ENOTCONN) {
		return true
	}
	var opErr *net.OpError
	if errors.As(err, &opErr) {
		return errors.Is(opErr.Err, syscall.ECONNREFUSED) || errors.Is(opErr.Err, syscall.ENOENT) || errors.Is(opErr.Err, syscall.ENOTCONN)
	}
	return false
}

// errText renders a stream break for a log record; a producer-side end is
// io.EOF and says so.
func errText(err error) string {
	if err == nil {
		return ""
	}
	if errors.Is(err, io.EOF) {
		return "producer ended the stream"
	}
	return err.Error()
}

// ---- the verbs, 1:1 over the generated client ----

// StartSession starts or resumes the session. Session facts travel only here.
func (c *client) StartSession(ctx context.Context, req *shimv1.StartSessionRequest) (*shimv1.StartSessionResponse, error) {
	return unary(ctx, c, "start_session", req, validateStartSessionRequest, c.rpc.StartSession)
}

// WatchSession opens the session update stream.
func (c *client) WatchSession(ctx context.Context) (Stream[*shimv1.WatchSessionResponse], error) {
	return c.watchSession(ctx)
}

// watchSession is the one place a session stream is opened — the verb and the
// client's own liveness stream share it.
func (c *client) watchSession(ctx context.Context) (Stream[*shimv1.WatchSessionResponse], error) {
	return openStream(ctx, c, "watch_session", &shimv1.WatchSessionRequest{}, nil, c.rpc.WatchSession,
		func(resp *shimv1.WatchSessionResponse) (*shimv1.WatchSessionResponse, error) {
			// THE FRAME ONEOF IS VALIDATED, never guessed: an unset frame is
			// illegal on the wire and is raised rather than read as an empty
			// update. The ARMS are handed on whole — the session watcher is
			// what tells an update from the landing-7 re-announcement.
			if resp.GetFrame() == nil {
				return nil, invalid("WatchSessionResponse", "WatchSessionResponse.frame", "oneof is unset on a pushed frame")
			}
			return resp, nil
		})
}

// SetSessionModel switches the session's model; the cold arm is an answer.
func (c *client) SetSessionModel(ctx context.Context, req *shimv1.SetSessionModelRequest) (*shimv1.SetSessionModelResponse, error) {
	return unary(ctx, c, "set_session_model", req, validateSetSessionModelRequest, c.rpc.SetSessionModel)
}

// SetSessionPermissionMode switches the session's permission mode.
func (c *client) SetSessionPermissionMode(ctx context.Context, req *shimv1.SetSessionPermissionModeRequest) (*shimv1.SetSessionPermissionModeResponse, error) {
	return unary(ctx, c, "set_session_permission_mode", req, validateSetSessionPermissionModeRequest, c.rpc.SetSessionPermissionMode)
}

// Hibernate stands the session down for the idle sweep.
func (c *client) Hibernate(ctx context.Context, req *shimv1.HibernateRequest) (*shimv1.HibernateResponse, error) {
	return unary(ctx, c, "hibernate", req, validateHibernateRequest, c.rpc.Hibernate)
}

// KillSession ends the session, gracefully unless forced.
//
// The stand-down is latched BEFORE the verb goes, not after it answers: the
// shim ends its process as it answers, so a monitor told only afterwards has
// already read the break as a fault.
func (c *client) KillSession(ctx context.Context, req *shimv1.KillSessionRequest) (*shimv1.KillSessionResponse, error) {
	c.standDown.Store(true)
	return unary(ctx, c, "kill_session", req, validateKillSessionRequest, c.rpc.KillSession)
}

// StartTurn opens a turn with the daemon's minted TurnId and its origin.
func (c *client) StartTurn(ctx context.Context, req *shimv1.StartTurnRequest) (*shimv1.StartTurnResponse, error) {
	return unary(ctx, c, "start_turn", req, validateStartTurnRequest, c.rpc.StartTurn)
}

// WatchAgent opens one agent's frame stream, opening with a catch-up page.
func (c *client) WatchAgent(ctx context.Context, req *shimv1.WatchAgentRequest) (Stream[*shimv1.WatchAgentResponse], error) {
	return openStream(ctx, c, "watch_agent", req, validateWatchAgentRequest, c.rpc.WatchAgent,
		func(resp *shimv1.WatchAgentResponse) (*shimv1.WatchAgentResponse, error) {
			if resp.GetFrame() == nil {
				return nil, invalid("WatchAgentResponse", "WatchAgentResponse.frame", "oneof is unset on a pushed frame")
			}
			return resp, nil
		})
}

// UpdateAgent delivers an answer, a consent, a prompt or a stop to an agent.
func (c *client) UpdateAgent(ctx context.Context, req *shimv1.UpdateAgentRequest) (*shimv1.UpdateAgentResponse, error) {
	return unary(ctx, c, "update_agent", req, validateUpdateAgentRequest, c.rpc.UpdateAgent)
}

// KillTurn interrupts the open turn.
func (c *client) KillTurn(ctx context.Context, req *shimv1.KillTurnRequest) (*shimv1.KillTurnResponse, error) {
	return unary(ctx, c, "kill_turn", req, validateKillTurnRequest, c.rpc.KillTurn)
}

// WatchBash opens one detached shell's stream.
func (c *client) WatchBash(ctx context.Context, work *conversationv1.DetachedWorkId) (Stream[*conversationv1.AgentBash], error) {
	if err := validateDetachedWorkID("WatchBashRequest.work", work); err != nil {
		c.log.Error("daemon.shimclient.watch_bash", "invalid request", dlog.Context{
			"workspace_id": string(c.ws), "error": err.Error(),
		})
		return nil, err
	}
	return openStream(ctx, c, "watch_bash", &shimv1.WatchBashRequest{Work: work}, nil, c.rpc.WatchBash,
		func(resp *shimv1.WatchBashResponse) (*conversationv1.AgentBash, error) {
			bash := resp.GetBash()
			if bash == nil {
				return nil, invalid("WatchBashResponse", "WatchBashResponse.bash", "is unset on a pushed frame")
			}
			return bash, nil
		})
}

// StopBash stops one detached shell.
func (c *client) StopBash(ctx context.Context, req *shimv1.StopBashRequest) (*shimv1.StopBashResponse, error) {
	return unary(ctx, c, "stop_bash", req, validateStopBashRequest, c.rpc.StopBash)
}

// DetachForeground detaches a running foreground unit.
func (c *client) DetachForeground(ctx context.Context, req *shimv1.DetachForegroundRequest) (*shimv1.DetachForegroundResponse, error) {
	return unary(ctx, c, "detach_foreground", req, validateDetachForegroundRequest, c.rpc.DetachForeground)
}

// ReadHistory pages an agent's history without opening a watch.
func (c *client) ReadHistory(ctx context.Context, req *shimv1.ReadHistoryRequest) (*shimv1.ReadHistoryResponse, error) {
	return unary(ctx, c, "read_history", req, validateReadHistoryRequest, c.rpc.ReadHistory)
}

// ReadTranscripts lists the conversations filed under the shim's directory.
func (c *client) ReadTranscripts(ctx context.Context, req *shimv1.ReadTranscriptsRequest) (*shimv1.ReadTranscriptsResponse, error) {
	return unary(ctx, c, "read_transcripts", req, validateReadTranscriptsRequest, c.rpc.ReadTranscripts)
}

// GatherTitleDigest reads the transcript for a synthesized title's material.
func (c *client) GatherTitleDigest(ctx context.Context, req *shimv1.GatherTitleDigestRequest) (*shimv1.GatherTitleDigestResponse, error) {
	return unary(ctx, c, "gather_title_digest", req, validateGatherTitleDigestRequest, c.rpc.GatherTitleDigest)
}

// unary is every unary verb's body: validate through the message's base
// function, log the branch, call the generated client, log the outcome.
func unary[Req any, Resp any](
	ctx context.Context,
	c *client,
	verb string,
	req *Req,
	validate func(*Req) error,
	call func(context.Context, *connect.Request[Req]) (*connect.Response[Resp], error),
) (*Resp, error) {
	operation := "daemon.shimclient." + verb
	if err := validate(req); err != nil {
		c.log.Error(operation, "invalid request", dlog.Context{
			"workspace_id": string(c.ws), "error": err.Error(),
		})
		return nil, err
	}
	c.log.Debug(operation, "calling shim", dlog.Context{"workspace_id": string(c.ws)})
	resp, err := call(ctx, connect.NewRequest(req))
	if err != nil {
		// A CALL THAT DIED IN A TEARDOWN THIS DAEMON ORDERED IS NOT A FAULT.
		// The latch is the shim's own record that a KillSession or a Kill was
		// asked of it, so a call still in flight to that shim comes back
		// `unavailable` for the plainest of reasons: the daemon killed the
		// peer it was talking to. The error is unchanged in substance -- it is
		// returned, wrapped so a caller can tell the two apart -- and only the
		// record's level and wording change.
		fields := dlog.Context{
			"workspace_id": string(c.ws), "error": err.Error(),
			"connect_code": connect.CodeOf(err).String(),
		}
		if c.StandingDown() {
			c.log.Info(operation, "the shim call ended in a stand-down this daemon ordered", fields)
			return nil, fmt.Errorf("%w: %w", ErrStandDownOrdered, err)
		}
		c.log.Error(operation, "shim call failed", fields)
		return nil, err
	}
	c.log.Debug(operation, "shim answered", dlog.Context{"workspace_id": string(c.ws)})
	return resp.Msg, nil
}

// quietOpenKey marks a context whose stream open is part of a RETRY LADDER,
// where a refusal is an ordinary branch rather than a warning. The error is
// still returned; only the record's level changes.
type quietOpenKey struct{}

// refusedOpen records a refused stream open at the level the refusal calls
// for.
//
// A SEMANTIC REFUSAL IS AN ANSWER, NOT A FAULT. not_found and
// failed_precondition are a serving shim saying it holds no such handle (yet):
// the transport is fine, and only the CALLER knows whether the handle was
// expected. The session watcher rules on exactly that (openRefusedLocked: INFO
// for an expected handle it will re-open, WARN plus a lifecycle fault for one
// nothing announced), so recording the same refusal at ERROR here reported an
// ordinary branch as a failure. It is recorded at INFO, and the error is still
// returned whole. Every other code is a failed open and stays at ERROR.
func (c *client) refusedOpen(ctx context.Context, operation string, err error, fields dlog.Context) {
	if quiet, _ := ctx.Value(quietOpenKey{}).(bool); quiet {
		c.log.Debug(operation, "shim stream refused", fields)
		return
	}
	switch connect.CodeOf(err) {
	case connect.CodeNotFound, connect.CodeFailedPrecondition:
		c.log.Info(operation, "the shim refused the stream open: it holds no such handle; the caller rules on whether that was expected", fields)
		return
	}
	c.log.Error(operation, "shim stream refused", fields)
}

// openStream is every watch verb's body. A Connect error on the OPEN is
// returned as an error from the call — never a stream that fails later.
func openStream[Req any, W any, T any](
	ctx context.Context,
	c *client,
	verb string,
	req *Req,
	validate func(*Req) error,
	open func(context.Context, *connect.Request[Req]) (*connect.ServerStreamForClient[W], error),
	project func(*W) (T, error),
) (Stream[T], error) {
	operation := "daemon.shimclient." + verb
	if validate != nil {
		if err := validate(req); err != nil {
			c.log.Error(operation, "invalid request", dlog.Context{
				"workspace_id": string(c.ws), "error": err.Error(),
			})
			return nil, err
		}
	}
	c.log.Debug(operation, "opening shim stream", dlog.Context{"workspace_id": string(c.ws)})
	streamCtx, cancel := context.WithCancel(ctx)
	stream, err := open(streamCtx, connect.NewRequest(req))
	if err != nil {
		cancel()
		c.refusedOpen(ctx, operation, err, dlog.Context{
			"workspace_id": string(c.ws), "error": err.Error(),
		})
		return nil, &StreamOpenError{Procedure: verb, Err: err}
	}
	// Connect defers the open to the first Receive, so the refusal is only
	// visible once a frame is asked for: take the first frame here, so a
	// refused open IS an error from this call.
	if !stream.Receive() {
		err := stream.Err()
		cancel()
		_ = stream.Close()
		if err == nil {
			err = io.EOF
		}
		c.refusedOpen(ctx, operation, err, dlog.Context{
			"workspace_id": string(c.ws), "error": err.Error(),
		})
		return nil, &StreamOpenError{Procedure: verb, Err: err}
	}
	first, err := project(stream.Msg())
	if err != nil {
		cancel()
		_ = stream.Close()
		c.log.Error(operation, "shim stream opened with an illegal frame", dlog.Context{
			"workspace_id": string(c.ws), "error": err.Error(),
		})
		return nil, &StreamOpenError{Procedure: verb, Err: err}
	}
	c.log.Debug(operation, "shim stream opened", dlog.Context{"workspace_id": string(c.ws)})
	return &firstFrameStream[W, T]{
		first: first,
		inner: &mappedStream[W, T]{procedure: verb, stream: stream, project: project, cancel: cancel},
	}, nil
}

// firstFrameStream re-serves the frame the open consumed, so a refused open is
// an error from the Watch call without the consumer losing the opening frame.
type firstFrameStream[W any, T any] struct {
	once  sync.Once
	first T
	inner *mappedStream[W, T]
}

// Recv yields the opening frame first, then the stream's own.
func (s *firstFrameStream[W, T]) Recv() (T, error) {
	served := false
	s.once.Do(func() { served = true })
	if served {
		return s.first, nil
	}
	return s.inner.Recv()
}

// Close ends the stream from this side.
func (s *firstFrameStream[W, T]) Close() { s.inner.Close() }
