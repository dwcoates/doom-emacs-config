package main

import (
	"context"
	"errors"
	"fmt"
	"sync"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/drain"
	"agent-repld/internal/health"
	"agent-repld/internal/ids"
	"agent-repld/internal/merge"
	"agent-repld/internal/mergeworkspace"
	"agent-repld/internal/promptqueue"
	"agent-repld/internal/resolve/feed"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/rollout"
	"agent-repld/internal/workspace"
	"agent-repld/internal/wsm"
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

func (f *serverForwarder) RetractTransferred(ws ids.WorkspaceID) {
	if target, ok := f.bound(); ok {
		target.RetractTransferred(ws)
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

func (f *relayForwarder) OpenCommitLog(ws ids.WorkspaceID, log *agentreplv1.OpenInEditorCommitLog) {
	if target, ok := f.relay(); ok {
		target.OpenCommitLog(ws, log)
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

// bound answers the workspace verbs for a call made through the forwarder. One
// made before they are bound is a wiring defect, answered as an error naming
// WHAT happened that early.
func (f *verbsForwarder) bound(what string) (workspace.Verbs, error) {
	verbs, ok := f.verbs()
	if !ok {
		return nil, fmt.Errorf("agent-repld: %s before the workspace verbs existed", what)
	}
	return verbs, nil
}

// publishRegistry is the roster's durable-half republish for a component built
// BEFORE the verbs, which own it. The republish reads the verbs out of the
// forwarder when it runs; one asked for before they are bound is a wiring
// defect, answered as an error naming WHAT happened that early.
func (f *verbsForwarder) publishRegistry(what string) func(context.Context) error {
	return func(ctx context.Context) error {
		verbs, err := f.bound(what)
		if err != nil {
			return err
		}
		return verbs.PublishRegistry(ctx)
	}
}

// ClaimMergeName is the merge orchestrator's name check (merge.Names), read
// out of the verbs, which are built after the orchestrator. One asked for
// before they are bound is a wiring defect, answered as an error.
func (f *verbsForwarder) ClaimMergeName(ctx context.Context, repoDir, name string) (string, func(), bool, error) {
	verbs, err := f.bound("an async merge's name was checked")
	if err != nil {
		return "", nil, false, err
	}
	return verbs.ClaimMergeName(ctx, repoDir, name)
}

// interruptTurn is the merge orchestrator's turn stop (merge.InterruptTurnFunc):
// the user's own turn interrupt, confirmed, so the agent's detached work stops
// with it. It reads the verbs out of the forwarder when it runs.
func (f *verbsForwarder) interruptTurn(ctx context.Context, ws ids.WorkspaceID) (bool, error) {
	verbs, err := f.bound("a merge tab's agent was interrupted")
	if err != nil {
		return false, err
	}
	out, err := verbs.Interrupt(ctx, ws, workspace.InterruptTarget{Turn: true}, true)
	if err != nil {
		return false, err
	}
	return out.Turn, nil
}

// status is a workspace subscription's status read (workspacesubs.Deps.Status),
// read out of the verbs, which are built after the notifier.
func (f *verbsForwarder) status(ctx context.Context, ws ids.WorkspaceID) (*agentreplv1.WorkspaceStatus, error) {
	verbs, err := f.bound("a followed workspace's status was read")
	if err != nil {
		return nil, err
	}
	return verbs.Status(ctx, ws)
}

// carryOutSelfClose is the turn-end edge's self-close (workspace.SelfCloses),
// read out of the verbs, which are built after the edges.
func (f *verbsForwarder) carryOutSelfClose(ctx context.Context, ws ids.WorkspaceID) error {
	verbs, err := f.bound("a turn's end carried a self-close out")
	if err != nil {
		return err
	}
	return verbs.CarryOutSelfClose(ctx, ws)
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
	peers feed.PeerDeliverySource
	tasks feed.VendorTaskSource
}

// historyAndPeers is what the fleet serves the feed: its history, the ends of
// its cross-session messages, and the agents its task ids name.
type historyAndPeers interface {
	feed.HistorySource
	feed.PeerDeliverySource
	feed.VendorTaskSource
}

// bind installs the fleet.
func (h *historyForwarder) bind(fleet historyAndPeers) {
	h.mu.Lock()
	defer h.mu.Unlock()
	h.fleet = fleet
	h.peers = fleet
	h.tasks = fleet
}

// ResolveVendorTask asks through the fleet which agent of a session one vendor
// task id names. A lookup before the fleet exists is a boot-order defect and
// fails loudly.
func (h *historyForwarder) ResolveVendorTask(ctx context.Context, ws ids.WorkspaceID, session, vendorTaskID string) (feed.VendorTaskAgent, error) {
	h.mu.RLock()
	fleet := h.tasks
	h.mu.RUnlock()
	if fleet == nil {
		return feed.VendorTaskAgent{}, errors.New("agent-repld: a vendor task was resolved before the session fleet was built")
	}
	return fleet.ResolveVendorTask(ctx, ws, session, vendorTaskID)
}

// ReadHistory reads through the fleet. A read before the fleet exists is a
// boot-order defect and fails loudly rather than reading as "no session".
func (h *historyForwarder) ReadHistory(ctx context.Context, ws ids.WorkspaceID, target *conversationv1.AgentId, after *conversationv1.HistoryPointer) (*conversationv1.HistoryPage, error) {
	h.mu.RLock()
	fleet := h.fleet
	h.mu.RUnlock()
	if fleet == nil {
		return nil, errors.New("agent-repld: history was read before the session fleet was built")
	}
	return fleet.ReadHistory(ctx, ws, target, after)
}

// ReadInherited reads a fork's inherited past through the fleet. A read before
// the fleet exists is a boot-order defect and fails loudly.
func (h *historyForwarder) ReadInherited(ctx context.Context, ws ids.WorkspaceID, book *conversationv1.AgentId, after *conversationv1.HistoryPointer, throughMs int64) (*conversationv1.HistoryPage, error) {
	h.mu.RLock()
	fleet := h.fleet
	h.mu.RUnlock()
	if fleet == nil {
		return nil, errors.New("agent-repld: history was read before the session fleet was built")
	}
	return fleet.ReadInherited(ctx, ws, book, after, throughMs)
}

// ResolvePeerDelivery asks through the fleet which sessions hold the two ends
// of one cross-session message. A lookup before the fleet exists is a
// boot-order defect and fails loudly.
func (h *historyForwarder) ResolvePeerDelivery(ctx context.Context, ws ids.WorkspaceID, deliveryID string) (feed.PeerDeliveryEnds, error) {
	h.mu.RLock()
	fleet := h.peers
	h.mu.RUnlock()
	if fleet == nil {
		return feed.PeerDeliveryEnds{}, errors.New("agent-repld: a peer delivery was resolved before the session fleet was built")
	}
	return fleet.ResolvePeerDelivery(ctx, ws, deliveryID)
}

// deliveryForwarder carries the fleet's fork to the prompt queue's
// Exclusively: the fleet is built before the queue, and a fork is made only
// at a bring-up, after both exist. A fork asked for before the queue is bound
// is a boot-order defect and fails loudly rather than forking a parent whose
// deliveries nothing holds.
type deliveryForwarder struct {
	mu    sync.RWMutex
	queue promptqueue.Queue
}

func (d *deliveryForwarder) bind(queue promptqueue.Queue) {
	d.mu.Lock()
	defer d.mu.Unlock()
	d.queue = queue
}

// Exclusively runs PERFORM while the queue delivers nothing to WS.
func (d *deliveryForwarder) Exclusively(ctx context.Context, ws ids.WorkspaceID, what string, perform func(context.Context) error) error {
	d.mu.RLock()
	queue := d.queue
	d.mu.RUnlock()
	if queue == nil {
		return errors.New("agent-repld: a fork was made before the prompt queue was built")
	}
	return queue.Exclusively(ctx, ws, what, perform)
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

// statusChangedForwarder carries the footer's status edge to the roster,
// which is built after the footer because it takes the footer as its status
// source.
type statusChangedForwarder struct {
	mu     sync.RWMutex
	roster sidebar.Resolver
	log    dlog.Logger
}

func (f *statusChangedForwarder) bind(roster sidebar.Resolver) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.roster = roster
}

// StatusChanged tells the roster the workspace's footer status moved.
func (f *statusChangedForwarder) StatusChanged(ws ids.WorkspaceID) {
	f.mu.RLock()
	roster := f.roster
	f.mu.RUnlock()
	if roster == nil {
		f.log.Error("daemon.cmd.status_changed", "the footer's status moved before the roster was wired; the row did not re-project it", dlog.Context{
			"workspace": string(ws), "invariant_violation": "the roster is bound before any footer edge",
		})
		return
	}
	roster.FooterStatusChanged(ws)
}

// mergeTarget is what the merge forwarder hands on to: the orchestrator's
// handover moves and its drain.
type mergeTarget interface {
	rollout.MergeMover
	drain.MergeSuspension
}

// mergeForwarder carries a handover's merge moves and the immediate
// shutdown's merge drain to the merge orchestrator, which is built after the
// rollout controller (it is told of a landing through it) and after the drain
// controller. Nothing transfers or adopts before the graph is whole; a move
// asked of an unbound forwarder is a wiring defect, answered as the error it
// is rather than as a move that happened. A drain asked of an unbound
// forwarder is the same defect, and the drain has no error to answer it with,
// so it panics.
type mergeForwarder struct {
	mu     sync.RWMutex
	target mergeTarget
}

var (
	_ rollout.MergeMover    = (*mergeForwarder)(nil)
	_ drain.MergeSuspension = (*mergeForwarder)(nil)
)

func (f *mergeForwarder) bind(target mergeTarget) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.target = target
}

func (f *mergeForwarder) mover() (mergeTarget, error) {
	f.mu.RLock()
	defer f.mu.RUnlock()
	if f.target == nil {
		return nil, errMergeMoverUnbound
	}
	return f.target, nil
}

func (f *mergeForwarder) SuspendForTransfer(ctx context.Context, ws ids.WorkspaceID) error {
	m, err := f.mover()
	if err != nil {
		return err
	}
	return m.SuspendForTransfer(ctx, ws)
}

func (f *mergeForwarder) AdoptWorkspace(ctx context.Context, ws ids.WorkspaceID) error {
	m, err := f.mover()
	if err != nil {
		return err
	}
	return m.AdoptWorkspace(ctx, ws)
}

func (f *mergeForwarder) Drain(ctx context.Context) {
	m, err := f.mover()
	if err != nil {
		panic(err)
	}
	m.Drain(ctx)
}

// errMergeMoverUnbound is a merge move or drain asked before the orchestrator
// exists.
var errMergeMoverUnbound = errors.New("agent-repld: the merge orchestrator is not bound; a workspace's merges cannot be moved")

// detachedForwarder carries the detached-merge service to the merge
// orchestrator, which is built before the workspace verbs and the prompt
// handler the service is made of. A detached merge reaching it before the
// service is bound is a defect in the boot order, answered as an error.
type detachedForwarder struct {
	mu     sync.RWMutex
	target *mergeworkspace.Service
}

func (f *detachedForwarder) bind(target *mergeworkspace.Service) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.target = target
}

func (f *detachedForwarder) service() (*mergeworkspace.Service, error) {
	f.mu.RLock()
	defer f.mu.RUnlock()
	if f.target == nil {
		return nil, fmt.Errorf("agent-repld: a detached merge reached the merge orchestrator before the detached-merge service existed")
	}
	return f.target, nil
}

func (f *detachedForwarder) Fork(ctx context.Context, requester wsm.Workspace, origin wsm.MergeOrigin, name, base string) (wsm.Workspace, error) {
	target, err := f.service()
	if err != nil {
		return wsm.Workspace{}, err
	}
	return target.Fork(ctx, requester, origin, name, base)
}

func (f *detachedForwarder) Reveal(ctx context.Context, mergeWS ids.WorkspaceID) (bool, error) {
	target, err := f.service()
	if err != nil {
		return false, err
	}
	return target.Reveal(ctx, mergeWS)
}

// CoverOwed reaches the detached-merge service, which records its own
// failures; a deploy before that service exists is answered with the error.
func (f *detachedForwarder) CoverOwed(ctx context.Context) error {
	target, err := f.service()
	if err != nil {
		return err
	}
	return target.CoverOwed(ctx)
}

// PayOwed reaches the detached-merge service. A deploy or a takeover cannot
// end before the graph that built the service has, so a payment that finds
// none is a defect in the graph's order, and panics.
func (f *detachedForwarder) PayOwed(ctx context.Context, coveredOnly bool, deployErr error) {
	target, err := f.service()
	if err != nil {
		panic(err)
	}
	target.PayOwed(ctx, coveredOnly, deployErr)
}

func (f *detachedForwarder) HandBack(ctx context.Context, requester ids.WorkspaceID, mergeWS wsm.Workspace, out merge.DetachedOutcome) error {
	target, err := f.service()
	if err != nil {
		return err
	}
	return target.HandBack(ctx, requester, mergeWS, out)
}
