package shimclient

import (
	"context"
	"errors"
	"fmt"
	"io"
	"net"
	"os"
	"os/exec"
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

// defaultKillGrace is how long a SIGTERMed shim has to exit before the
// SIGKILL. Bounded, because a wedged shim must not wedge the daemon's
// shutdown.
const defaultKillGrace = 5 * time.Second

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

	// standDown latches the moment a KillSession is asked of this shim.
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
func newClient(log dlog.Logger, ws ids.WorkspaceID, udsPath string, back backoff, probe func(ids.WorkspaceID) (bool, error)) *client {
	ctx, cancel := context.WithCancel(context.Background())
	return &client{
		log:           log,
		ws:            ws,
		udsPath:       udsPath,
		rpc:           shimv1connect.NewShimClient(newUDSClient(udsPath), udsBaseURL),
		back:          back,
		grace:         defaultKillGrace,
		lockProbe:     probe,
		link:          newLinkFeed(),
		exit:          make(chan ExitInfo, 1),
		dead:          make(chan struct{}),
		monitorCtx:    ctx,
		cancelMonitor: cancel,
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
func (c *client) Kill(attr KillAttribution) error {
	c.mu.Lock()
	switch {
	case c.detached:
		c.mu.Unlock()
		return ErrDetached
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
		return c.killAdopted(attr, grace)
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
			<-c.dead
			return nil
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
	<-c.dead
	return nil
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
func (c *client) killAdopted(attr KillAttribution, grace time.Duration) error {
	pid, err := socketPeerPID(c.udsPath)
	if err != nil {
		if isSocketGone(err) {
			// The socket refuses or is absent: the shim the caller asked to
			// stop is already gone. That is the state they asked for.
			c.log.Info("daemon.shimclient.kill", "the adopted shim's socket is gone; nothing to stop", dlog.Context{
				"workspace_id": string(c.ws), "uds": c.udsPath, "actor": attr.Actor,
			})
			c.publishExit(ExitInfo{
				Code:   -1,
				Stderr: "adopted shim: the socket was already gone when the daemon went to stop it",
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
	if c.awaitAdoptedGone(pgid, grace) {
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
	if !c.awaitAdoptedGone(pgid, grace) {
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
// expires. It reports whether the group is gone.
func (c *client) awaitAdoptedGone(pgid int, bound time.Duration) bool {
	deadline := time.Now().Add(bound)
	for {
		if err := syscall.Kill(-pgid, 0); errors.Is(err, syscall.ESRCH) {
			return true
		}
		if !time.Now().Before(deadline) {
			return false
		}
		time.Sleep(adoptedGonePoll)
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

	ctx := dlog.Context{
		"workspace_id": string(c.ws), "pid": info.PID, "code": info.Code,
		"signal": info.Signal, "stderr": info.Stderr,
	}
	if info.Attribution != nil {
		ctx["actor"] = info.Attribution.Actor
		ctx["reason"] = info.Attribution.Reason
		c.log.Info("daemon.shimclient.exit", "supervised shim stopped as asked", ctx)
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
// Kill's own last step is an UNBOUNDED wait on the reap, which is correct for
// its callers -- the process has been SIGKILLed and the kernel does not
// negotiate -- but the immediate shutdown cannot stake the whole exit on that:
// a child stopped in the kernel (a ptrace stop, an uninterruptible D state)
// does not reap, and an exit that waited on one is a daemon that never leaves.
// So the kill runs on its own goroutine and this waits on bound, whose value
// the caller states; the goroutine's channel is buffered, so a kill that lands
// after the bound has passed still completes and never leaks a blocked writer.
func (c *client) killWithin(ctx context.Context, bound time.Duration, attr KillAttribution) error {
	done := make(chan error, 1)
	go func() { done <- c.Kill(attr) }()

	within, cancel := context.WithTimeout(ctx, bound)
	defer cancel()
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

// awaitHealthy consumes session frames until the first diagnostics arm says
// healthy. Unhealthy is an ANSWER, not readiness: the client keeps waiting.
// The frames come from the ONE receive loop the stream has; a second loop on
// the same stream would be two concurrent receivers.
func (c *client) awaitHealthy(ctx context.Context, frames <-chan *shimv1.WatchSessionResponse, errs <-chan error) error {
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
			c.log.Warn("daemon.shimclient.ready", "shim reported unhealthy; still waiting", dlog.Context{
				"workspace_id": string(c.ws), "faults": len(diagnostics.GetUnhealthy().GetFaults()),
			})
		}
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
		if c.standDown.Load() {
			// THE DAEMON ASKED FOR THIS. A stand-down was requested of this
			// shim, so the liveness stream ending is the answer to it and not
			// a fault: redialing here reaches a process that is on its way
			// out, and publishing `redialing` raises a `link_severed` health
			// fault against a teardown the daemon itself ordered.
			c.log.Debug("daemon.shimclient.redial", "the liveness stream ended after a stand-down was asked of this shim; not redialing", dlog.Context{
				"uds": c.udsPath, "error": errText(broke),
			})
			return
		}
		c.log.Warn("daemon.shimclient.redial", "shim link broke; redialing", dlog.Context{
			"uds": c.udsPath, "error": errText(broke),
		})
		next, err := c.redial(ctx)
		if err != nil {
			c.log.Warn("daemon.shimclient.redial", "redial stopped", dlog.Context{
				"uds": c.udsPath, "error": err.Error(),
			})
			return
		}
		stream = next
		frames, errs = recvLoop(stream, ctx.Done())
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
	c.log.Error("daemon.shimclient.exit", "adopted shim is gone: socket refused and workspace lock free", dlog.Context{
		"workspace_id": string(c.ws), "uds": c.udsPath, "error": dialErr.Error(),
	})
	c.publishExit(ExitInfo{
		PID:    c.PID(),
		Code:   -1,
		Stderr: "adopted shim: no exit observed; socket refused and the workspace lock read free",
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

// isSocketGone reports whether a dial error means the listener is not there:
// ECONNREFUSED or ENOENT on the unix socket.
func isSocketGone(err error) bool {
	if err == nil {
		return false
	}
	if errors.Is(err, syscall.ECONNREFUSED) || errors.Is(err, syscall.ENOENT) || errors.Is(err, os.ErrNotExist) {
		return true
	}
	var opErr *net.OpError
	if errors.As(err, &opErr) {
		return errors.Is(opErr.Err, syscall.ECONNREFUSED) || errors.Is(opErr.Err, syscall.ENOENT)
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
		c.log.Error(operation, "shim call failed", dlog.Context{
			"workspace_id": string(c.ws), "error": err.Error(),
			"connect_code": connect.CodeOf(err).String(),
		})
		return nil, err
	}
	c.log.Debug(operation, "shim answered", dlog.Context{"workspace_id": string(c.ws)})
	return resp.Msg, nil
}

// quietOpenKey marks a context whose stream open is part of a RETRY LADDER,
// where a refusal is an ordinary branch rather than a warning. The error is
// still returned; only the record's level changes.
type quietOpenKey struct{}

// refusedOpen records a refused stream open at the level the context calls for.
func (c *client) refusedOpen(ctx context.Context, operation, message string, fields dlog.Context) {
	if quiet, _ := ctx.Value(quietOpenKey{}).(bool); quiet {
		c.log.Debug(operation, message, fields)
		return
	}
	c.log.Error(operation, message, fields)
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
		c.refusedOpen(ctx, operation, "shim stream refused", dlog.Context{
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
		c.refusedOpen(ctx, operation, "shim stream refused", dlog.Context{
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
