// Package startingshim answers ONE question two daemons' boot paths both ask:
// is the shim that a previous daemon spawned for this workspace merely STILL
// STARTING, rather than absent?
//
// THE TWO KERNEL FACTS DO NOT COVER THE STARTING WINDOW. A shim's workspace
// lock is taken inside StartSession and its socket is bound by Node several
// tens of milliseconds after the fork returns, so between those two instants a
// spawned shim holds NO lock and answers NO dial: it looks exactly like a
// workspace nothing ever served. A daemon that reads it that way spawns a
// SECOND shim onto the one session socket, and the shim itself refuses the
// bind and dies -- measured 2026-09-13 as `shim.main.fatal: already has a live
// listener; refusing to start a second shim on one session socket`, with the
// first daemon killed 60ms after its spawn, the successor spawning at T+80ms,
// the survivor binding at T+110ms and the newcomer dying at T+190ms.
//
// The missing fact is durable in the registry: wsm.Workspace.SpawnedShim is
// written at the instant the fork returns, as the child's pid AND its kernel
// start stamp, and retired once the shim announces itself. This package reads
// it, asks the kernel whether that very process is alive, and -- when it is -- waits, bounded,
// for the socket to appear. A survivor that announces itself is then taken
// through the ORDINARY inert-survivor adoption; a pid that is dead or absent
// leaves the caller on its ordinary spawn path.
package startingshim

import (
	"context"
	"fmt"
	"path/filepath"
	"time"

	"agent-repld/internal/clock"
	"agent-repld/internal/ids"
	"agent-repld/internal/kernelevent"
	"agent-repld/internal/shimsocket"
)

// Clock is this package's view of time; see internal/clock.
type Clock = clock.Clock

// SystemClock is the production Clock.
type SystemClock = clock.System

// Outcome is what a probe of the recorded spawn concluded.
type Outcome int

const (
	// OutcomeNoSpawn means no pid is recorded: no daemon has a spawn
	// outstanding for this workspace, so the caller's ordinary spawn path is
	// right.
	OutcomeNoSpawn Outcome = iota
	// OutcomeSpawnDead means a pid is recorded and the process is gone. The
	// spawn died before it announced itself, so again the caller spawns.
	OutcomeSpawnDead
	// OutcomeAnnounced means the recorded process is alive and its socket went
	// LIVE within the bound. The caller adopts it as the inert survivor it is.
	OutcomeAnnounced
	// OutcomeUndetermined means the recorded process was alive and its socket
	// never went live within the bound. It is NEVER read as absent: a live
	// process that may bind the path at any instant is exactly what a spawn
	// must not race, so the caller leaves the workspace undetermined and says
	// so at ERROR.
	OutcomeUndetermined
)

// String names the outcome for a log record.
func (o Outcome) String() string {
	switch o {
	case OutcomeNoSpawn:
		return "no_spawn_recorded"
	case OutcomeSpawnDead:
		return "spawn_dead"
	case OutcomeAnnounced:
		return "announced"
	default:
		return "undetermined"
	}
}

// Identify reads the identity of the process PID names now: the record a
// spawn makes of its child at the fork. The child is this daemon's own and
// unreaped, so its pid cannot have been handed to another process yet.
func Identify(pid int) (ids.ProcessIdentity, error) {
	if pid <= 0 {
		return ids.ProcessIdentity{}, fmt.Errorf("startingshim: pid %d names no one process", pid)
	}
	stamp, found, err := startStamp(pid)
	if err != nil {
		return ids.ProcessIdentity{}, err
	}
	if !found {
		return ids.ProcessIdentity{}, fmt.Errorf("startingshim: pid %d names no process", pid)
	}
	return ids.ProcessIdentity{PID: pid, StartUnixMicros: stamp}, nil
}

// Alive reports whether RECORDED still names a running process: its pid
// names a live process AND that process's kernel start stamp is the one
// recorded.
//
// THE PID ALONE IS NOT THE PROCESS. kill(pid, 0) answers for whichever
// process holds the number now, and reads a process of another user as alive
// (EPERM): a shim's pid recorded at 19:33 named AppleCredentialManagerDaemon,
// started at 05:02 the next day, and the workspace waited out its adoption
// bound for it every 10s (2026-11-21). A process whose stamp differs started
// after the recorded one, so the recorded one is gone.
//
// An error is a stamp the kernel would not answer for a pid that exists; the
// caller leaves the workspace undetermined, as for any unreadable fact.
func Alive(recorded ids.ProcessIdentity) (bool, error) {
	return aliveBy(startStamp, recorded)
}

// aliveBy is Alive over the kernel read STAMP.
func aliveBy(stamp func(pid int) (int64, bool, error), recorded ids.ProcessIdentity) (bool, error) {
	if recorded.PID <= 0 {
		return false, nil
	}
	now, found, err := stamp(recorded.PID)
	if err != nil {
		return false, err
	}
	return found && now == recorded.StartUnixMicros, nil
}

// Waiter waits for a recorded spawn to announce itself on its socket.
//
// THE WAIT IS THE KERNEL'S, NEVER A POLL. The socket is probed again at each
// change of its directory -- the shim binds a temporary name, listens, and
// links it into place, so the socket's name appears only once it accepts --
// and the wait ends at once on the recorded process's exit event. The bound is
// a failure bound on the whole wait.
//
// Probe and Alive are injected so the caller's OWN probe decides liveness --
// one resolution cannot disagree with the probe the same caller then reports
// -- and so a test drives the kernel's answers directly.
type Waiter struct {
	// Alive reports whether the recorded spawn's process still runs. Nil
	// means this package's own Alive.
	Alive func(recorded ids.ProcessIdentity) (bool, error)
	// Probe is the socket probe, handed to shimsocket.NewestLive so every
	// generation on disk is considered.
	Probe func(string) (shimsocket.State, error)
	// Clock times the bound. Nil means SystemClock.
	Clock Clock
	// Changes follows the socket's directory; nil means kernelevent.Follow.
	Changes func(ctx context.Context, dir string) (<-chan error, func())
	// Exit waits for the recorded process's exit; nil means
	// kernelevent.WaitExit.
	Exit func(ctx context.Context, pid int) error
}

// Running answers whether RECORDED's process still runs, by the injected
// Alive or this package's own. It is the one liveness question every caller
// asks before it waits, so the answer it logs is the answer Await acts on.
func (w Waiter) Running(recorded ids.ProcessIdentity) (bool, error) {
	if w.Alive == nil {
		return Alive(recorded)
	}
	return w.Alive(recorded)
}

// Await answers what became of the spawn recorded for a workspace whose lock
// reads FREE and whose socket is not live.
//
// It answers the socket PATH it settled on along with the outcome, because a
// survivor that announced itself may have bound a relaunch generation rather
// than the base path, and the caller adopts the path that answered.
//
// A CANCELLED CONTEXT IS NOT A DEAD SPAWN. The caller's boot is being
// abandoned, and answering OutcomeSpawnDead would send it off to spawn a
// second shim on its way out; it answers undetermined, which spawns nothing.
// Neither is a watch that fails: the process may still bind at any instant.
func (w Waiter) Await(ctx context.Context, recorded *ids.ProcessIdentity, base string, bound time.Duration) (string, Outcome) {
	if recorded == nil {
		return base, OutcomeNoSpawn
	}
	running, err := w.Running(*recorded)
	if err != nil {
		return base, OutcomeUndetermined
	}
	if !running {
		return base, OutcomeSpawnDead
	}
	clock := w.Clock
	if clock == nil {
		clock = SystemClock{}
	}
	follow := w.Changes
	if follow == nil {
		follow = func(ctx context.Context, dir string) (<-chan error, func()) { return kernelevent.Follow(ctx, dir) }
	}
	exit := w.Exit
	if exit == nil {
		exit = kernelevent.WaitExit
	}
	// THE WATCHES ARE TAKEN BEFORE THE FIRST PROBE, so a bind or an exit in
	// between is reported.
	waitCtx, cancel := context.WithCancel(ctx)
	defer cancel()
	changes, stop := follow(waitCtx, filepath.Dir(base))
	defer stop()
	exited := make(chan error, 1)
	// inflight: joined: the function that starts it waits for it.
	go func() { exited <- exit(waitCtx, recorded.PID) }()
	expired := clock.After(bound)
	for {
		if path, state, _ := shimsocket.NewestLive(w.Probe, base); state == shimsocket.StateLive {
			return path, OutcomeAnnounced
		}
		select {
		case <-ctx.Done():
			return base, OutcomeUndetermined
		case err := <-exited:
			if err != nil {
				return base, OutcomeUndetermined
			}
			// A SOCKET LINKED IN JUST BEFORE THE EXIT is still the
			// survivor's last word: the probe above is asked once more.
			if path, state, _ := shimsocket.NewestLive(w.Probe, base); state == shimsocket.StateLive {
				return path, OutcomeAnnounced
			}
			return base, OutcomeSpawnDead
		case err := <-changes:
			if err != nil {
				return base, OutcomeUndetermined
			}
		case <-expired:
			return base, OutcomeUndetermined
		}
	}
}
