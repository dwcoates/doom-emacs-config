// Package viewquiet is THE ONE SWITCH that stops the daemon drawing views
// nobody can see: while persistent wifi's lid-closed quiet state stands
// (agentrepl.v1 PersistentWifiState.quiet), the view resolvers (sidebar,
// footer, topbar, feed tails) stop rendering and publishing to the local
// pages, and when it ends each brings every view it skipped to current state
// in ONE publication, never a replay of the states it missed.
//
// THE GATE STOPS VIEWS, NEVER FACTS. A resolver still takes every fact while
// quiet, and still derives whatever an agent depends on (a workspace's
// status, the durable turn result and status-change instant); only the view
// built from those facts for a page waits. So session work, the merge queue,
// tests, notices and remote delivery never see the gate.
//
// A remote-control reader is never behind the gate: claude.ai and the Claude
// app read the vendor session through the vendor, not these views.
package viewquiet

import (
	"fmt"
	"sync"
	"sync/atomic"
)

// Telemetry counts the renders the gate skipped, by view. The metrics surface
// is the production one; nil counts nothing.
type Telemetry interface {
	CountViewRenderSkipped(view string)
}

// Gate is the switch. The zero value is not quiet, follows nobody and counts
// nothing; a nil *Gate is a gate that is never quiet, so a resolver built
// without one draws as it always has.
type Gate struct {
	quiet atomic.Bool
	tel   Telemetry

	// mu serializes Set, so followers hear the transitions in the order they
	// happened and never two at once.
	mu        sync.Mutex
	followers []follower
}

type follower struct {
	view string
	fn   func(quiet bool)
}

// New builds a gate that counts skipped renders on tel (nil counts nothing).
func New(tel Telemetry) *Gate { return &Gate{tel: tel} }

// Quiet answers whether views are stopped now. It is one atomic load, so a
// resolver asks it on every fact.
func (g *Gate) Quiet() bool { return g != nil && g.quiet.Load() }

// Follow registers fn, which is told every transition (true entering, false
// leaving), under the name view. A resolver registers once, at construction,
// and resyncs its views when told false. Following a nil gate registers
// nothing: that gate never moves.
func (g *Gate) Follow(view string, fn func(quiet bool)) {
	if g == nil {
		return
	}
	if view == "" || fn == nil {
		panic(fmt.Sprintf("viewquiet: Follow needs a view name and a function (view=%q, fn nil=%v)", view, fn == nil))
	}
	g.mu.Lock()
	defer g.mu.Unlock()
	g.followers = append(g.followers, follower{view: view, fn: fn})
}

// Set moves the gate and tells every follower, in registration order, when it
// moved. It answers whether it moved. Leaving tells the followers AFTER the
// gate reads not quiet, so a resync renders under a gate that lets it
// publish; entering tells them after the gate reads quiet, so nothing a
// follower renders from then is published.
func (g *Gate) Set(quiet bool) bool {
	g.mu.Lock()
	defer g.mu.Unlock()
	if g.quiet.Load() == quiet {
		return false
	}
	g.quiet.Store(quiet)
	for _, f := range g.followers {
		f.fn(quiet)
	}
	return true
}

// Skipped counts one render the gate stopped for view. A resolver calls it
// exactly where it declined to render, so the counter is the work saved.
func (g *Gate) Skipped(view string) {
	if g == nil || g.tel == nil {
		return
	}
	g.tel.CountViewRenderSkipped(view)
}
