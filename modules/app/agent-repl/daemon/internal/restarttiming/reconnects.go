package restarttiming

import (
	"sync"
	"time"

	"agent-repld/internal/ids"
)

// WorkspaceReconnect is ONE workspace's client connection coming back after
// a restart: from the restart's start until the workspace's Emacs host
// stream or webview stream opened on this daemon.
const WorkspaceReconnect Restart = "workspace_reconnect"

// ReconnectPhase is which client connection came back, the `phase`.
type ReconnectPhase string

// The reconnect phases.
const (
	// ReconnectHost is the workspace's Emacs host stream (WatchHostWorkspace).
	ReconnectHost ReconnectPhase = "host"
	// ReconnectWeb is the workspace's webview stream (WatchWebWorkspace).
	ReconnectWeb ReconnectPhase = "web"
)

// ReconnectCause is the restart the connection came back from, the `cause`.
type ReconnectCause string

// The reconnect causes.
const (
	// ReconnectCold is a daemon's ordinary boot, timed from the process's
	// start, expecting every open workspace the roster shows.
	ReconnectCold ReconnectCause = "cold"
	// ReconnectSuccessor is a handover, timed from the outgoing daemon's
	// manifest, expecting the participants it named.
	ReconnectSuccessor ReconnectCause = "successor"
	// ReconnectWebappReload is a deploy's webview reload, timed from the
	// reload's push, expecting each webview it was pushed to.
	ReconnectWebappReload ReconnectCause = "webapp_reload"
)

// OfReconnect is one workspace's connection coming back.
func OfReconnect(ws ids.WorkspaceID, p ReconnectPhase, cause ReconnectCause, d time.Duration) Observation {
	return observation(WorkspaceReconnect, string(p), string(cause), OK, ws, d)
}

type reconnectKey struct {
	ws    ids.WorkspaceID
	phase ReconnectPhase
}

type expectation struct {
	began time.Time
	cause ReconnectCause
}

// Reconnects times each expected workspace's connection coming back after a
// restart. A restart states what it expects (Expect); the stream handlers say
// when each connection opens (Connected), and the first one an expectation
// meets is timed and ends it. A connection nothing expected is not timed: an
// ordinary switch or a page opened later is not a reconnect.
//
// The time every workspace took to come back is the slowest workspace's:
// the largest agentrepl_workspace_restart_last_seconds of the restart.
type Reconnects struct {
	recorder Recorder
	now      func() time.Time

	mu       sync.Mutex
	expected map[reconnectKey]expectation
}

// NewReconnects answers a tracker recording through recorder on the clock now.
func NewReconnects(recorder Recorder, now func() time.Time) *Reconnects {
	return &Reconnects{recorder: recorder, now: now, expected: map[reconnectKey]expectation{}}
}

// Expect states that each of workspaces' phase connection is to come back
// after a restart of cause that began at began. A later expectation for the
// same workspace and phase replaces an earlier one.
func (r *Reconnects) Expect(cause ReconnectCause, began time.Time, phase ReconnectPhase, workspaces ...ids.WorkspaceID) {
	r.mu.Lock()
	defer r.mu.Unlock()
	for _, ws := range workspaces {
		r.expected[reconnectKey{ws, phase}] = expectation{began: began, cause: cause}
	}
}

// Connected says ws's phase connection opened now, timing it when a restart
// expected it.
func (r *Reconnects) Connected(ws ids.WorkspaceID, phase ReconnectPhase) {
	key := reconnectKey{ws, phase}
	r.mu.Lock()
	e, ok := r.expected[key]
	delete(r.expected, key)
	r.mu.Unlock()
	if !ok {
		return
	}
	// A START FROM ANOTHER PROCESS IS ON THE WALL CLOCK (a handover's
	// manifest), and a clock step between the two instants can put it after
	// now; that span is stated as zero rather than as a negative duration.
	took := max(r.now().Sub(e.began), 0)
	r.recorder.ObserveRestart(OfReconnect(ws, phase, e.cause, took))
}
