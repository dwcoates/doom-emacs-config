package main

import (
	"context"
	"errors"
	"sync"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/health"
	"claude-repld/internal/ids"
	"claude-repld/internal/promptqueue"
	"claude-repld/internal/resolve/feed"
	"claude-repld/internal/rollout"
	"claude-repld/internal/workspace"
)

// LATE BINDING, AND WHY THERE IS ANY.
//
// Two edges of the component graph point backwards. The rollout controller
// pushes onto the server's per-workspace and daemon-level streams, and the
// workspace verbs push onto the same relay — but `server.New` takes the
// rollout controller and the verbs as dependencies, so the server cannot exist
// before them.
//
// The cycle is broken with FORWARDERS rather than by inventing behavior: each
// one implements the consumer's interface, holds the real target once it
// exists, and does exactly nothing else. A forwarder with no target yet is a
// programming error in the boot order, so it records the drop rather than
// pretending the push happened — but nothing reaches these before `server.New`
// returns, because nothing serves until then.

// serverForwarder carries the rollout's and the drain's pushes to the server,
// which is built after both of them.
type serverForwarder struct {
	mu     sync.RWMutex
	target serverPushTarget
	// dropped counts pushes that arrived before the server existed. It is a
	// boot-order defect, and the count is what makes it visible.
	dropped int
}

// serverPushTarget is exactly the slice of the server the two controllers
// push through.
type serverPushTarget interface {
	rollout.WorkspacePusher
	rollout.ParticipantSource
	ShutdownAnnounced(push *agentreplv1.DaemonShutdownAnnounced)
	DrainScheduled(push *agentreplv1.DaemonDrainScheduled)
	DrainCancelled(push *agentreplv1.DaemonDrainCancelled)
	SeedFeedTextScale(scale float64)
}

// bind installs the real server. It is called once, immediately after
// server.New returns and before anything is served.
func (f *serverForwarder) bind(target serverPushTarget) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.target = target
}

// bound answers the target, false while the server does not exist yet.
func (f *serverForwarder) bound() (serverPushTarget, bool) {
	f.mu.RLock()
	target := f.target
	f.mu.RUnlock()
	if target == nil {
		f.mu.Lock()
		f.dropped++
		f.mu.Unlock()
		return nil, false
	}
	return target, true
}

// Dropped is how many pushes arrived before the server existed.
func (f *serverForwarder) Dropped() int {
	f.mu.RLock()
	defer f.mu.RUnlock()
	return f.dropped
}

func (f *serverForwarder) PushTransferred(ws ids.WorkspaceID, successorAddress string) {
	if target, ok := f.bound(); ok {
		target.PushTransferred(ws, successorAddress)
	}
}

func (f *serverForwarder) PushReloadWebapp(ws ids.WorkspaceID) {
	if target, ok := f.bound(); ok {
		target.PushReloadWebapp(ws)
	}
}

// Participants answers an EMPTY snapshot while the server does not exist: no
// stream can be held before anything is served, so "nobody holds one" is the
// truth rather than a stand-in for it.
func (f *serverForwarder) Participants(ws ids.WorkspaceID) rollout.Participants {
	if target, ok := f.bound(); ok {
		return target.Participants(ws)
	}
	return rollout.Participants{}
}

func (f *serverForwarder) ShutdownAnnounced(push *agentreplv1.DaemonShutdownAnnounced) {
	if target, ok := f.bound(); ok {
		target.ShutdownAnnounced(push)
	}
}

func (f *serverForwarder) DrainScheduled(push *agentreplv1.DaemonDrainScheduled) {
	if target, ok := f.bound(); ok {
		target.DrainScheduled(push)
	}
}

func (f *serverForwarder) DrainCancelled(push *agentreplv1.DaemonDrainCancelled) {
	if target, ok := f.bound(); ok {
		target.DrainCancelled(push)
	}
}

func (f *serverForwarder) SeedFeedTextScale(scale float64) {
	if target, ok := f.bound(); ok {
		target.SeedFeedTextScale(scale)
	}
}

// relayForwarder carries the workspace verbs' host pushes to the server's
// relay, which is built after the verbs.
type relayForwarder struct {
	mu     sync.RWMutex
	target workspace.HostRelay
}

func (f *relayForwarder) bind(target workspace.HostRelay) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.target = target
}

func (f *relayForwarder) relay() (workspace.HostRelay, bool) {
	f.mu.RLock()
	defer f.mu.RUnlock()
	return f.target, f.target != nil
}

func (f *relayForwarder) OpenInEditor(ws ids.WorkspaceID, path string, line *uint32) {
	if target, ok := f.relay(); ok {
		target.OpenInEditor(ws, path, line)
	}
}

func (f *relayForwarder) ReloadWebapp(ws ids.WorkspaceID) {
	if target, ok := f.relay(); ok {
		target.ReloadWebapp(ws)
	}
}

func (f *relayForwarder) PublishHostWorkspace(ws ids.WorkspaceID) {
	if target, ok := f.relay(); ok {
		target.PublishHostWorkspace(ws)
	}
}

func (f *relayForwarder) ReturnFeedToTail(ws ids.WorkspaceID) {
	if target, ok := f.relay(); ok {
		target.ReturnFeedToTail(ws)
	}
}

// verbsForwarder carries a reference to the workspace verbs for the lifecycle
// hooks the fleet is wired with before the verbs exist.
type verbsForwarder struct {
	mu     sync.RWMutex
	target workspace.Verbs
}

func (f *verbsForwarder) bind(target workspace.Verbs) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.target = target
}

func (f *verbsForwarder) verbs() (workspace.Verbs, bool) {
	f.mu.RLock()
	defer f.mu.RUnlock()
	return f.target, f.target != nil
}

// healthForwarder carries the session watcher's diagnostics verdict to the
// health reporter, which is built after the fleet the watchers live in.
type healthForwarder struct {
	mu     sync.RWMutex
	target health.Reporter
}

func (f *healthForwarder) bind(target health.Reporter) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.target = target
}

func (f *healthForwarder) reporter() (health.Reporter, bool) {
	f.mu.RLock()
	defer f.mu.RUnlock()
	return f.target, f.target != nil
}

// historyForwarder is the feed resolver's history source, which is the session
// fleet's: the fleet is built after the feed resolver (it routes into it), so
// the source is bound once the fleet exists.
type historyForwarder struct {
	mu    sync.RWMutex
	fleet feed.HistorySource
}

// bind installs the fleet.
func (h *historyForwarder) bind(fleet feed.HistorySource) {
	h.mu.Lock()
	defer h.mu.Unlock()
	h.fleet = fleet
}

// ReadHistory reads through the fleet. A read before the fleet exists is a
// boot-order defect and fails loudly rather than reading as "no session".
func (h *historyForwarder) ReadHistory(ctx context.Context, ws ids.WorkspaceID, target *conversationv1.AgentId, after *conversationv1.HistoryPointer) (*conversationv1.HistoryPage, error) {
	h.mu.RLock()
	fleet := h.fleet
	h.mu.RUnlock()
	if fleet == nil {
		return nil, errors.New("claude-repld: history was read before the session fleet was built")
	}
	return fleet.ReadHistory(ctx, ws, target, after)
}

// vendorServesForwarder carries the footer's vendor-serves edge
// (footer.WithVendorServes) to the prompt queue, which is built after the
// footer. An edge before the queue is bound is a wiring defect, said at ERROR:
// the prompts held after reconnect would wait for the next edge.
type vendorServesForwarder struct {
	mu    sync.RWMutex
	queue promptqueue.Queue
	log   dlog.Logger
}

func (f *vendorServesForwarder) bind(queue promptqueue.Queue) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.queue = queue
}

// VendorServes tells the queue the workspace's vendor serves again.
func (f *vendorServesForwarder) VendorServes(ws ids.WorkspaceID, was string) {
	f.mu.RLock()
	queue := f.queue
	f.mu.RUnlock()
	if queue == nil {
		f.log.Error("daemon.cmd.vendor_serves", "the vendor served again before the prompt queue was wired; its after-reconnect holds were not released", dlog.Context{
			"workspace": string(ws), "vendor_block": was, "invariant_violation": "the queue is bound before any footer edge",
		})
		return
	}
	queue.OnVendorServes(ws)
}
