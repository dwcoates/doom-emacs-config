// shimspawnwatch.go makes the gap between "we exec'd a shim" and "a shim
// dialled in" observable.
//
// THE GAP IS WHERE SHIMS DIE SILENTLY. The spawn is logged, the connection
// never arrives, and the only later record is the bring-up's own deadline
// expiring thirty seconds on — no exit code, no stderr, no statement of which
// failure it even was. The watch closes that gap by holding the two facts the
// exec knows and the connection wait does not: whether the process is still
// running, and what it said before it stopped.
package server

import (
	"fmt"
	"sync"

	"claude-repld/internal/shim"
	"claude-repld/internal/shimclient"
)

// ShimSpawnWatch records the fate of every shim process THIS daemon spawned,
// keyed by session, and publishes a death that happened before the shim ever
// connected to whoever is waiting for that connection.
//
// One entry per session, replaced on each spawn: a fresh spawn's wait must
// never be aborted by the previous generation's corpse.
type ShimSpawnWatch struct {
	// connected is a second, independent read of "has this session's shim
	// dialled in" — the listener's own parked-connection probe. It backs up the
	// watch's own Connected mark rather than replacing it: the listener only
	// knows about connections nobody has claimed yet, and the mark only about
	// connections that were claimed, so a shim is proven connected by either.
	connected ConnectedFunc
	logf      func(string, ...any)

	mu       sync.Mutex
	sessions map[string]*spawnedShim
}

// spawnedShim is one generation of one session's daemon-spawned shim process.
type spawnedShim struct {
	// stderrTail reads the process's bounded stderr tail live, so evidence is
	// available while it still runs and not only after it is reaped.
	stderrTail func() string
	// died is closed when the process is reaped WITHOUT having connected.
	died chan struct{}
	// connected latches the moment the daemon took this session's shim
	// connection.
	connected bool
	// exited latches the reap; failure carries its cause when it was a death
	// before connecting.
	exited  bool
	failure error
}

var _ shimclient.ShimDeaths = (*ShimSpawnWatch)(nil)

// NewShimSpawnWatch builds a watch. connected may be nil (the watch then relies
// on its own Connected mark alone); a nil logf discards.
func NewShimSpawnWatch(connected ConnectedFunc, logf func(string, ...any)) *ShimSpawnWatch {
	if logf == nil {
		logf = func(string, ...any) {}
	}
	return &ShimSpawnWatch{connected: connected, logf: logf, sessions: map[string]*spawnedShim{}}
}

// Spawned arms the watch for a freshly exec'd shim, discarding any previous
// generation's record for the session. stderrTail reads that process's bounded
// stderr tail; a nil one reports no evidence rather than panicking, because a
// launcher that cannot capture stderr must still be able to report the exit.
func (w *ShimSpawnWatch) Spawned(sessionID string, stderrTail func() string) {
	if stderrTail == nil {
		stderrTail = func() string { return "" }
	}
	w.mu.Lock()
	w.sessions[sessionID] = &spawnedShim{stderrTail: stderrTail, died: make(chan struct{})}
	w.mu.Unlock()
	w.logf("server: session %s: shim spawn watch armed — an exit before this shim connects will fail the bring-up immediately", sessionID)
}

// Connected marks that the daemon took this session's shim connection, which is
// what makes a later exit an ordinary death rather than a spawn failure.
func (w *ShimSpawnWatch) Connected(sessionID string) {
	w.mu.Lock()
	defer w.mu.Unlock()
	sh, ok := w.sessions[sessionID]
	if !ok {
		return // a shim this daemon never spawned: nothing to mark
	}
	sh.connected = true
}

// Exited records the reaped process and returns the spawn failure when it died
// before ever connecting (nil otherwise, including for an ordinary exit after a
// connection). The returned error is the caller's to log alongside its own
// workspace-bound record; the watch logs the loud line itself so no death can
// pass unrecorded even if a caller ignores the return.
func (w *ShimSpawnWatch) Exited(sessionID string, waitErr error) error {
	w.mu.Lock()
	sh, ok := w.sessions[sessionID]
	if !ok {
		w.mu.Unlock()
		w.logf("server: session %s: a shim exited (%s) that this daemon has no spawn record for", sessionID, shim.ExitDescription(waitErr))
		return nil
	}
	alreadyConnected := sh.connected
	tail := sh.stderrTail()
	w.mu.Unlock()

	// The listener's own probe is consulted only when the mark says nothing:
	// its failure is reported and treated as "not proven connected", because
	// reporting a death with evidence is strictly better than losing it to an
	// unanswerable probe.
	if !alreadyConnected && w.connected != nil {
		probed, err := w.connected(sessionID)
		if err != nil {
			w.logf("server: session %s: probing whether the exiting shim had connected FAILED: %v — treating it as never connected", sessionID, err)
		}
		alreadyConnected = probed
	}

	w.mu.Lock()
	defer w.mu.Unlock()
	// A newer generation may have replaced this entry between the two locks;
	// its channel and its liveness are not this corpse's to publish.
	if current, ok := w.sessions[sessionID]; !ok || current != sh {
		w.logf("server: session %s: a superseded shim generation exited (%s)", sessionID, shim.ExitDescription(waitErr))
		return nil
	}
	if sh.exited {
		return sh.failure // already recorded; never publish an exit twice
	}
	sh.exited = true
	if alreadyConnected {
		// NOT published as a death: `died` means "never connected", and a shim
		// that connected and later exited is the reconnect loop's business, not
		// a spawn failure. Firing here would make a reattach report a bring-up
		// that never failed.
		w.logf("server: session %s: the daemon-spawned shim exited (%s) AFTER connecting", sessionID, shim.ExitDescription(waitErr))
		return nil
	}
	// The stderr tail is deliberately NOT folded into this error: it is carried
	// alongside on ShimSpawnState, and the one composer that renders both (the
	// waiting client, and the loud line below) would otherwise print it twice.
	sh.failure = fmt.Errorf("server: session %s: %w: %s (exit code %d)",
		sessionID, shimclient.ErrShimDiedBeforeConnect, shim.ExitDescription(waitErr), shim.ExitCode(waitErr))
	w.logf("server: session %s: SHIM SPAWN FAILURE — %v; shim stderr tail: %s", sessionID, sh.failure, tailOrNone(tail))
	close(sh.died)
	return sh.failure
}

// DiedBeforeConnect implements shimclient.ShimDeaths.
func (w *ShimSpawnWatch) DiedBeforeConnect(sessionID string) <-chan struct{} {
	w.mu.Lock()
	defer w.mu.Unlock()
	sh, ok := w.sessions[sessionID]
	if !ok {
		return nil // never spawned here: nothing can fire
	}
	return sh.died
}

// SpawnState implements shimclient.ShimDeaths.
func (w *ShimSpawnWatch) SpawnState(sessionID string) shimclient.ShimSpawnState {
	w.mu.Lock()
	sh, ok := w.sessions[sessionID]
	if !ok {
		w.mu.Unlock()
		return shimclient.ShimSpawnState{}
	}
	state := shimclient.ShimSpawnState{Spawned: true, Alive: !sh.exited, Failure: sh.failure}
	tail := sh.stderrTail
	w.mu.Unlock()
	state.StderrTail = tail()
	return state
}

// tailOrNone renders an empty stderr tail as a statement rather than a blank,
// so a spawn failure never reads as if the evidence were merely missing from
// the message.
func tailOrNone(tail string) string {
	if tail == "" {
		return "(the process wrote nothing to stderr)"
	}
	return tail
}
