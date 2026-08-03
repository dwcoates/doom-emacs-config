package shimclient

import (
	"errors"
	"fmt"
)

// ErrShimDiedBeforeConnect is returned by AwaitReady when the shim process the
// daemon launched for this session exited without ever dialling in.
//
// It is a distinct sentinel from a deadline: "the process is gone" and "the
// process has not finished coming up" are different failures with different
// repairs, and a bring-up that reports the second when the first happened
// spends its whole deadline proving nothing.
var ErrShimDiedBeforeConnect = errors.New("shimclient: the daemon-spawned shim exited before it ever connected")

// ErrShimDiedAfterConnect is returned when the daemon-owned process behind a
// session that has connected is reaped while the session controller remains
// live. A transport reconnect cannot repair a process that no longer exists,
// so Run returns this error instead of waiting forever for that process to
// dial again.
var ErrShimDiedAfterConnect = errors.New("shimclient: the daemon-spawned shim exited after connecting")

// ShimExit is the process evidence captured at reap time. It crosses the
// optional process-observer seam separately from transport errors because a
// closed socket alone cannot distinguish a dead shim from a live shim that is
// reattaching after a daemon-side link loss.
type ShimExit struct {
	Description string
	ExitCode    int
	StderrTail  string
}

// ShimExits reports a daemon-spawned process death after that process has
// connected. A nil channel means this daemon does not own a process handle,
// as with a shim that survived a daemon restart.
type ShimExits interface {
	DiedAfterConnect(sessionID string) <-chan ShimExit
}

// ShimSpawnState is what the daemon knows about a session's spawned shim
// process at one instant. It is the evidence a stalled bring-up attaches to its
// own error, so the reader learns whether the process is alive, dead, or was
// never this daemon's to begin with.
type ShimSpawnState struct {
	// Spawned reports that THIS daemon launched a shim process for the session.
	// False for a shim that outlived a previous daemon and dialled back in —
	// there is no process handle for it, so its silence has no local evidence.
	Spawned bool
	// Alive reports that the spawned process has not been reaped yet.
	Alive bool
	// StderrTail is the bounded tail of that process's stderr, empty when it
	// has written nothing (or nothing was captured).
	StderrTail string
	// Failure is non-nil once the process exited WITHOUT ever connecting. It
	// carries the exit status and the stderr tail.
	Failure error
}

// ShimDeaths reports the death of a daemon-spawned shim to the client that is
// waiting for that shim to dial in.
//
// Without it the only bound on a shim that dies during bring-up is the
// caller's deadline, so an immediate `node: cannot find module` and a shim that
// is merely slow are indistinguishable for thirty seconds and neither carries
// the process's own explanation.
//
// Implemented daemon-side by the shim spawn watch (server package); optional,
// because a client driven by a shim this daemon never spawned has no process
// to watch.
type ShimDeaths interface {
	// DiedBeforeConnect returns a channel closed when the session's spawned
	// shim process exits without having connected. A nil channel never fires,
	// which is the correct answer for a session with no spawned process.
	DiedBeforeConnect(sessionID string) <-chan struct{}
	// SpawnState reports the current process evidence for the session.
	SpawnState(sessionID string) ShimSpawnState
}

func (c *Client) afterConnectDeathError(exit ShimExit) error {
	description := exit.Description
	if description == "" {
		description = "process exited"
	}
	tail := " (the process wrote nothing to stderr)"
	if exit.StderrTail != "" {
		tail = "; shim stderr tail: " + exit.StderrTail
	}
	return fmt.Errorf("shimclient: session %s: %w: %s (exit code %d)%s",
		c.cfg.SessionID, ErrShimDiedAfterConnect, description, exit.ExitCode, tail)
}

// spawnDeathError builds the fast bring-up failure for a shim that exited
// before connecting, preferring the watch's own recorded cause (which carries
// exit status and stderr) and falling back to the bare sentinel so a death
// with no recorded detail is still reported as a death.
func (c *Client) spawnDeathError() error {
	state := c.cfg.ShimDeaths.SpawnState(c.cfg.SessionID)
	tail := " (the process wrote nothing to stderr)"
	if state.StderrTail != "" {
		tail = "; shim stderr tail: " + state.StderrTail
	}
	if state.Failure != nil {
		return fmt.Errorf("shimclient: bring-up for session %s FAILED: %w%s", c.cfg.SessionID, state.Failure, tail)
	}
	return fmt.Errorf("shimclient: bring-up for session %s FAILED: %w (no exit detail was recorded)%s",
		c.cfg.SessionID, ErrShimDiedBeforeConnect, tail)
}

// spawnEvidence renders what is known about the spawned process as a suffix for
// a timeout error: whether it is still running (so the diagnosis is "alive but
// never dialled" rather than "crashed"), plus whatever it printed.
//
// Empty when this daemon spawned nothing for the session — there is then no
// process claim to make, and inventing one would be worse than saying nothing.
func (c *Client) spawnEvidence() string {
	if c.cfg.ShimDeaths == nil {
		return ""
	}
	state := c.cfg.ShimDeaths.SpawnState(c.cfg.SessionID)
	if !state.Spawned {
		return ""
	}
	var evidence string
	switch {
	case state.Alive:
		evidence = " — the shim process this daemon spawned is STILL ALIVE but never dialled in"
	case state.Failure != nil:
		evidence = fmt.Sprintf(" — the shim process this daemon spawned had already exited without connecting: %v", state.Failure)
	default:
		evidence = " — the shim process this daemon spawned has exited"
	}
	if state.StderrTail != "" {
		evidence += "; shim stderr tail: " + state.StderrTail
	}
	return evidence
}
