package topbar

import (
	"fmt"

	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/shimclient"
)

// The link states the render-colors topbar_connectivity table is keyed by.
// They are the DAEMON's states rather than the shim client's spellings: the
// topbar reports the route to the session, so a redial in progress and a
// severed link are one condition to a reader.
const (
	// linkNoSession is "there is no session to have a route to".
	linkNoSession = "no_session"
	// linkConnecting is a route being established.
	linkConnecting = "connecting"
	// linkConnected is a serving route.
	linkConnected = "connected"
	// linkSevered is a broken route being retried.
	linkSevered = "severed"
	// linkDead is a route whose process is gone.
	linkDead = "dead"
)

// connectivityKey maps the THREE HOPS of connectivity truth onto the
// vocabulary's key. An UNOBSERVED link is `no_session` and not `connecting`:
// nothing has been asked to connect yet, and drawing it as in progress would
// claim work nobody started.
//
// peers is whether BOTH client hops — WatchHostWorkspace and
// WatchWebWorkspace — are held. Per daemon.md invariant 11 a workspace is
// connected only while all three hops are live, so a serving shim link with a
// client hop down is `severed` and never `connected`: the route to the reader
// is broken even though the route to the session is not.
//
// parked is the idle sweep's deliberate stand-down. A PARKED SESSION IS IDLE,
// NOT BROKEN (resolve/sidebar/status.go), so the route being gone is not a
// fault to draw: `no_session` — "there is no session to have a route to" — is
// literally the state, and it is the only key the vocabulary already carries
// for it. Drawing `dead` there hollowed the indicator ("the session's process
// is gone") for a session the daemon parked on purpose.
func connectivityKey(seen bool, link shimclient.LinkState, peers, parked bool) string {
	if !seen {
		return linkNoSession
	}
	if parked && link == shimclient.LinkDead {
		return linkNoSession
	}
	switch link {
	case shimclient.LinkDialing:
		return linkConnecting
	case shimclient.LinkConnected:
		if !peers {
			return linkSevered
		}
		return linkConnected
	case shimclient.LinkRedialing:
		return linkSevered
	case shimclient.LinkDead:
		return linkDead
	default:
		return linkNoSession
	}
}

// connectivityGlyph is the literal character the indicator draws. Geometric
// forms rather than pictographs: the strip is one line tall and the shapes read
// at that size in every terminal and browser.
func connectivityGlyph(key string) string {
	switch key {
	case linkConnected:
		return "●"
	case linkConnecting:
		return "◍"
	case linkSevered:
		return "◌"
	case linkDead:
		return "○"
	default:
		return "·"
	}
}

// connectivityTitle is the tooltip, verbatim.
func connectivityTitle(key string) string {
	switch key {
	case linkConnected:
		return "connected to the session"
	case linkConnecting:
		return "connecting to the session"
	case linkSevered:
		return "the route to the session was severed and is being retried"
	case linkDead:
		return "the session's process is gone"
	default:
		return "no session is running"
	}
}

// connectivity resolves the whole indicator: the vocabulary's tone, the glyph
// and the tooltip. The client holds no table and maps nothing itself.
//
// The tone is ASSERTED against the vocabulary's closed tone set. A tone the
// file does not declare is a divergence between the daemon and the two
// renderers that consume the same file, and it fails loudly here rather than
// painting a state no renderer has a class for.
func (r *resolver) connectivity(key string) (*frontendv1.TopbarConnectivity, error) {
	tone, ok := r.colors.TopbarConnectivity[key]
	if !ok {
		return nil, fmt.Errorf(
			"render-colors topbar_connectivity has no row for link state %q; the daemon refuses to serve an unpainted state", key)
	}
	if !r.toneDeclared(tone) {
		return nil, fmt.Errorf(
			"render-colors topbar_connectivity maps %q to tone %q, which is not in topbar_tones", key, tone)
	}
	return &frontendv1.TopbarConnectivity{
		Tone:  tone,
		Glyph: connectivityGlyph(key),
		Title: connectivityTitle(key),
	}, nil
}

// toneDeclared reports whether the vocabulary declares the tone.
func (r *resolver) toneDeclared(tone string) bool {
	for _, declared := range r.colors.TopbarTones {
		if declared == tone {
			return true
		}
	}
	return false
}
