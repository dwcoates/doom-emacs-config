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

// ReconnectWave is ONE restart's expected connections of one phase coming
// back together: when the first came back, when the last did, and the spread
// between them, the span the sidebar shows its rows loading in. Its phase is
// the connection phase and its mark (`host_first`, `web_spread`, ...).
const ReconnectWave Restart = "reconnect_wave"

// WaveMark is which instant of a wave an observation states.
type WaveMark string

// The wave marks.
const (
	// WaveFirst is the first expected connection back, from the restart's
	// start.
	WaveFirst WaveMark = "first"
	// WaveLast is the last expected connection back, from the restart's
	// start: the time every workspace took to come back.
	WaveLast WaveMark = "last"
	// WaveSpread is the time from the first expected connection back to the
	// last.
	WaveSpread WaveMark = "spread"
)

// OfReconnectWave is one mark of a restart's wave of p connections.
func OfReconnectWave(p ReconnectPhase, mark WaveMark, cause ReconnectCause, d time.Duration) Observation {
	return observation(ReconnectWave, string(p)+"_"+string(mark), string(cause), OK, "", d)
}

// wave is the connections one Expect stated: how many are still to come back,
// and when the first did. A wave one of whose members was expected again by a
// later restart is broken: its last connection is never timed, since the
// member it lost will not come back as part of it.
type wave struct {
	cause   ReconnectCause
	phase   ReconnectPhase
	began   time.Time
	pending int
	first   time.Time
	broken  bool
	// closeWindow ends the wave's resource window (Windows), once every
	// member came back or the wave broke.
	closeWindow func()
}

// The resource windows a reconnect wave opens are bounded at about three
// times the p99 of a workspace's reconnect over the 7 days to 2026-11-20:
// 60s for an Emacs host stream, 233s for a webview stream.
const (
	hostWaveWindowBound = 180 * time.Second
	webWaveWindowBound  = 600 * time.Second
)

// waveWatches are the processes a wave of each phase brings back: Emacs
// registers its hosts again, and the webviews' WebKit processes load the
// webapp again.
var waveWatches = map[ReconnectPhase][]Watched{
	ReconnectHost: {{Process: ProcessEmacs}},
	ReconnectWeb:  {{Process: ProcessWebKitWebContent}, {Process: ProcessWebKitNetworking}, {Process: ProcessWebKitGPU}},
}

// waveWindowBounds are each phase's resource window bound.
var waveWindowBounds = map[ReconnectPhase]time.Duration{
	ReconnectHost: hostWaveWindowBound,
	ReconnectWeb:  webWaveWindowBound,
}

type reconnectKey struct {
	ws    ids.WorkspaceID
	phase ReconnectPhase
}

type expectation struct {
	began time.Time
	cause ReconnectCause
	wave  *wave
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
	windows  Windows
	now      func() time.Time

	mu       sync.Mutex
	expected map[reconnectKey]expectation
}

// NewReconnects answers a tracker recording through recorder on the clock
// now, measuring each wave's processes through windows.
func NewReconnects(recorder Recorder, windows Windows, now func() time.Time) *Reconnects {
	return &Reconnects{recorder: recorder, windows: windows, now: now, expected: map[reconnectKey]expectation{}}
}

// Expect states that each of workspaces' phase connection is to come back
// after a restart of cause that began at began, together as one wave. A later
// expectation for the same workspace and phase replaces an earlier one, and
// breaks the earlier one's wave.
func (r *Reconnects) Expect(cause ReconnectCause, began time.Time, phase ReconnectPhase, workspaces ...ids.WorkspaceID) {
	r.mu.Lock()
	defer r.mu.Unlock()
	w := &wave{cause: cause, phase: phase, began: began}
	for _, ws := range workspaces {
		key := reconnectKey{ws, phase}
		earlier, ok := r.expected[key]
		if ok && earlier.wave == w {
			continue
		}
		if ok {
			earlier.wave.broken = true
			earlier.wave.pending--
			earlier.wave.closeWindow()
		}
		w.pending++
		r.expected[key] = expectation{began: began, cause: cause, wave: w}
	}
	// ONE RESTART'S EXPECTATION OF A PHASE IS ONE WAVE, so the wave's member
	// count is the figure of what the restart affected. An empty expectation
	// is no restart that expects connections back, so it counts nothing.
	if w.pending > 0 {
		r.recorder.ObserveAffected(OfAffected(AffectedKind(phase), cause, w.pending))
		w.closeWindow = r.windows.OpenWindow(ReconnectWave, string(phase), waveWindowBounds[phase], waveWatches[phase]...)
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
	now := r.now()
	took := max(now.Sub(e.began), 0)
	r.recorder.ObserveRestart(OfReconnect(ws, phase, e.cause, took))
	r.waveConnected(e.wave, now)
}

// waveConnected counts one of W's connections back at NOW, timing the wave's
// first, and its last and spread once every member came back.
func (r *Reconnects) waveConnected(w *wave, now time.Time) {
	r.mu.Lock()
	w.pending--
	first := w.first.IsZero()
	if first {
		w.first = now
	}
	last := w.pending == 0 && !w.broken
	r.mu.Unlock()
	if last {
		w.closeWindow()
	}
	if first {
		r.recorder.ObserveRestart(OfReconnectWave(w.phase, WaveFirst, w.cause, max(now.Sub(w.began), 0)))
	}
	if last {
		r.recorder.ObserveRestart(OfReconnectWave(w.phase, WaveLast, w.cause, max(now.Sub(w.began), 0)))
		r.recorder.ObserveRestart(OfReconnectWave(w.phase, WaveSpread, w.cause, now.Sub(w.first)))
	}
}
