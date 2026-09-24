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
// The missing fact is durable in the registry: wsm.Workspace.SpawnedShimPID is
// written at the instant the fork returns. This package reads it, asks the
// kernel whether that process is alive, and -- when it is -- waits, bounded,
// for the socket to appear. A survivor that announces itself is then taken
// through the ORDINARY inert-survivor adoption; a pid that is dead or absent
// leaves the caller on its ordinary spawn path.
package startingshim

import (
	"context"
	"errors"
	"syscall"
	"time"

	"claude-repld/internal/clock"
	"claude-repld/internal/shimsocket"
)

// Clock is this package's view of time; see internal/clock.
type Clock = clock.Clock

// SystemClock is the production Clock.
type SystemClock = clock.System

// DefaultPoll is how often the wait re-probes the socket. The fact lives in
// the KERNEL and cannot announce itself, so a poll is the floor under it; the
// whole wait is bounded by the caller's adoption bound.
const DefaultPoll = 20 * time.Millisecond

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

// Alive reports whether pid names a process this daemon can signal. It is
// kill(pid, 0): ESRCH is the one answer that means gone, and EPERM means the
// process is there and owned by somebody else -- alive either way, and never
// read as absent.
func Alive(pid int) bool {
	if pid <= 0 {
		return false
	}
	err := syscall.Kill(pid, 0)
	return !errors.Is(err, syscall.ESRCH)
}

// Waiter waits for a recorded spawn to announce itself on its socket.
//
// Probe and Alive are injected so the caller's OWN probe decides liveness --
// one resolution cannot disagree with the probe the same caller then reports
// -- and so a test drives the kernel's two answers directly.
type Waiter struct {
	// Alive reports whether the recorded pid names a live process. Nil means
	// this package's own kill(pid, 0).
	Alive func(pid int) bool
	// Probe is the socket probe, handed to shimsocket.NewestLive so every
	// generation on disk is considered.
	Probe func(string) (shimsocket.State, error)
	// Clock drives the poll. Nil means SystemClock.
	Clock Clock
	// Poll is the re-probe cadence. Zero means DefaultPoll.
	Poll time.Duration
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
func (w Waiter) Await(ctx context.Context, recorded *int, base string, bound time.Duration) (string, Outcome) {
	if recorded == nil {
		return base, OutcomeNoSpawn
	}
	alive := w.Alive
	if alive == nil {
		alive = Alive
	}
	if !alive(*recorded) {
		return base, OutcomeSpawnDead
	}
	clock := w.Clock
	if clock == nil {
		clock = SystemClock{}
	}
	poll := w.Poll
	if poll <= 0 {
		poll = DefaultPoll
	}
	deadline := clock.Now().Add(bound)
	for {
		path, state, _ := shimsocket.NewestLive(w.Probe, base)
		if state == shimsocket.StateLive {
			return path, OutcomeAnnounced
		}
		// THE PROCESS IS RE-ASKED EVERY PASS. A spawn that dies mid-wait is
		// the ordinary "the shim would not come up" case, and holding the boot
		// to the full bound for a process that is already gone is a listener
		// nobody accepts on.
		if !alive(*recorded) {
			return base, OutcomeSpawnDead
		}
		if !clock.Now().Before(deadline) {
			return base, OutcomeUndetermined
		}
		select {
		case <-clock.After(poll):
		case <-ctx.Done():
			return base, OutcomeUndetermined
		}
	}
}
