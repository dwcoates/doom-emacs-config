package sessionwatcher

import (
	"context"
	"errors"
	"sort"
	"sync"
	"sync/atomic"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
	"claude-repld/internal/shimclient"
)

// openingPageSize is the budget every watch's opening catch-up page is opened
// with. One value for every watch: the page is a catch-up bounded by a
// known_through pointer in the ordinary case, and a cold subagent bubble pages
// older history through ReadHistory rather than through a bigger first page.
const openingPageSize = 200

// mainWatchKey is the known_through map's key for the MAIN agent's watch,
// which is addressed by an UNSET target and therefore has no AgentId to key
// on. Empty is safe: a real AgentId.value is never empty on the wire.
const mainWatchKey = ""

// agentWatch is one open WatchAgent stream and everything needed to re-open it
// after a transport failure. The main agent's watch has a nil id (its request
// leaves the target unset) and a nil work.
type agentWatch struct {
	// id is the watched agent, nil for the main agent's watch.
	id *conversationv1.AgentId
	// work is the detached-work handle whose announcement opened this watch,
	// nil for the main agent's watch. It is what the live-work set reports.
	work *conversationv1.DetachedWorkId
	// stream is the open stream, nil between a teardown and its re-open.
	stream shimclient.Stream[*shimv1.WatchAgentResponse]
	// done marks a watch REAPED at its terminal, so its goroutine reads the
	// end of its own stream as the reaping rather than a transport failure.
	done bool
}

// shellWatch is one open WatchBash stream for one detached shell.
type shellWatch struct {
	work   *conversationv1.DetachedWorkId
	stream shimclient.Stream[*conversationv1.AgentBash]
	// done marks a watch REAPED at its terminal, as on agentWatch.
	done bool
}

// watcher is one live workspace's watch fleet: every shim watch the session
// owns, the daemon-to-shim hop of connectivity truth, and the routing of every
// frame into the resolvers' sinks.
//
// SERIALIZATION: mu guards every field AND is held across every sink call, so
// the resolvers observe frames in stream order per agent however many streams
// are being consumed at once. Each stream has its own goroutine; the only work
// done outside mu is blocking on Recv.
type watcher struct {
	ws     ids.WorkspaceID
	client shimclient.Client
	sinks  Sinks
	log    dlog.Logger

	ctx    context.Context
	cancel context.CancelFunc

	mu sync.Mutex

	// gen is the fleet's generation. A re-open bumps it, so a goroutine whose
	// stream was torn down under it recognizes its own error as stale and
	// exits silently instead of reporting a transport failure.
	gen    uint64
	closed bool
	// sessionEnded records that the session itself is over (query_died), which
	// is the one way a stream may legally end without a terminal.
	sessionEnded bool
	// degraded records that a standing stream is actually DOWN -- a stream
	// that ended while the session was live, or a watch that could not be
	// opened. It, and not the link state, is what a re-open answers: the
	// client's connectivity feed replays the bring-up transitions (dialing,
	// then connected) to a watcher that was handed a connected link, and a
	// re-open on that history would tear down the very streams bring-up had
	// just established. A link that comes back with every stream still
	// standing has nothing to re-open.
	degraded bool

	link LinkState
	// linkNow mirrors link for the lock-free readers; every write to link
	// writes it under mu, so the mirror can never lead the truth.
	linkNow atomic.Int32
	addr    OutputAddress

	turn      *ids.TurnID
	mainAgent *conversationv1.AgentId

	// freeWaiters are the standing AwaitFree calls. They are answered by the
	// stream edges — a turn end and a live-work change — never by a poll.
	freeWaiters []chan error
	// turnWaiters are the standing AwaitTurnEnd calls, keyed by the turn.
	turnWaiters map[ids.TurnID][]chan turnEnd
	// pendingTurnEnds are the turn ends recorded under mu and not yet handed
	// to the lifecycle sink, which is told only once mu is released.
	pendingTurnEnds []endedTurn
	// closedTurns remembers how the last few turns ended, so a wait that
	// arrives after the terminal is still answered; closedTurnOrder is its
	// eviction order.
	closedTurns     map[ids.TurnID]TurnClose
	closedTurnOrder []ids.TurnID

	sessionStream shimclient.Stream[*conversationv1.SessionUpdate]
	main          *agentWatch

	// agents is one entry per LIVE DETACHED SUBAGENT, keyed by AgentId.value.
	// The main watch is not in it: the open set here IS the live subagent set.
	agents map[string]*agentWatch
	// shells is one entry per live detached shell, keyed by DetachedWorkId.value.
	shells map[string]*shellWatch
	// monitors is every live background watcher, keyed by DetachedWorkId.value.
	// A monitor opens NO stream — the contract gives it none — so its liveness
	// is tracked from the announcement and dropped at the monitor activity's
	// own terminal.
	monitors map[string]*conversationv1.DetachedWorkId

	// known is the newest pointer served on each watch, keyed by AgentId.value
	// with mainWatchKey for the main agent's. It is what a re-open passes as
	// known_through so the opening page is a catch-up, not a repaint.
	known map[string]*conversationv1.HistoryPointer

	// facts is what the watcher learned from the activities it routed, keyed
	// by AgentActivityId.value: the detachable kind (so a `detached`-origin
	// announcement resolves to a kind), the created agent id (the WatchAgent
	// address of a spawned subagent) and the call's tool name (what a
	// permission notification names). NOT ANCESTRY: nothing here says who
	// spawned whom, and no placement is ever derived from it.
	//
	// KEYED BY ACTIVITY ID, DELIBERATELY. A subagent's AgentId is minted from
	// the spawning call's tool_use_id (and the main agent's from the original
	// vendor session id), so the bytes of a spawn unit's identity and its
	// created agent's identity COINCIDE. They are still two different things,
	// and this map keys on the UNIT so nothing here ever derives a spawn
	// unit's identity from an AgentId.
	facts map[string]*activityFact
}

// activityFact is what one routed activity taught the watcher about the unit
// it addressed.
type activityFact struct {
	// kind is the detachable kind, unset when the unit cannot detach.
	kind detachedKind
	// agent is the agent a subagent spawn created, nil for every other kind.
	agent *conversationv1.AgentId
	// tool is the call's tool name, empty when the unit is not a tool call.
	tool string
	// work is the detached-work handle once this unit detached, nil before.
	work *conversationv1.DetachedWorkId
}

// detachedKind is the closed set of work kinds that can leave a turn, as
// DetachableWork names them.
type detachedKind int

const (
	// kindUnknown is a unit whose kind the watcher could not determine.
	kindUnknown detachedKind = iota
	// kindSubagent is a detached subagent, watched with WatchAgent.
	kindSubagent
	// kindBash is a detached shell, watched with WatchBash.
	kindBash
	// kindMonitor is a background watcher. FOOTER-ONLY: no stream exists.
	kindMonitor
	// kindWorkflow is a workflow run. KICKED, never watched.
	kindWorkflow
)

// String names the kind for a log record.
func (k detachedKind) String() string {
	switch k {
	case kindSubagent:
		return "subagent"
	case kindBash:
		return "bash"
	case kindMonitor:
		return "monitor"
	case kindWorkflow:
		return "workflow"
	default:
		return "unknown"
	}
}

// start builds the fleet, opens every watch the session's opening level calls
// for, and begins consuming the client's connectivity.
func start(ctx context.Context, ws ids.WorkspaceID, client shimclient.Client, session Session, sinks Sinks, log dlog.Logger) (Watcher, error) {
	if client == nil {
		return nil, errors.New("sessionwatcher: nil shim client")
	}
	if log == nil {
		return nil, errors.New("sessionwatcher: nil logger")
	}
	if session.Started == nil {
		return nil, errors.New("sessionwatcher: Session.Started is unset")
	}
	if sinks.Feed == nil || sinks.Footer == nil || sinks.Topbar == nil || sinks.Sidebar == nil || sinks.Lifecycle == nil {
		return nil, errors.New("sessionwatcher: every sink but Holds is required")
	}

	runCtx, cancel := context.WithCancel(ctx)
	w := &watcher{
		ws:       ws,
		client:   client,
		sinks:    sinks,
		log:      log.With(dlog.Context{"workspace_id": string(ws)}),
		ctx:      runCtx,
		cancel:   cancel,
		link:     shimclient.LinkConnected,
		addr:     rootAddress(),
		agents:   map[string]*agentWatch{},
		shells:   map[string]*shellWatch{},
		monitors: map[string]*conversationv1.DetachedWorkId{},
		known:    map[string]*conversationv1.HistoryPointer{},

		turnWaiters: map[ids.TurnID][]chan turnEnd{},
		closedTurns: map[ids.TurnID]TurnClose{},
		facts:       map[string]*activityFact{},
	}
	w.linkNow.Store(int32(shimclient.LinkConnected))
	if session.MainKnownThrough != nil {
		w.known[mainWatchKey] = session.MainKnownThrough
	}
	for id, ptr := range session.KnownThrough {
		if ptr != nil {
			w.known[id] = ptr
		}
	}

	w.mu.Lock()
	w.log.Debug("daemon.sessionwatcher.start", "opening the session's watch fleet", dlog.Context{
		"vendor_session_id": session.Started.GetVendorSessionId(),
		"turn_in_flight":    session.Started.GetTurnInFlight().GetValue(),
		"live_work":         len(session.Started.GetLiveWork()),
	})

	w.sinks.Topbar.OnSessionStarted(w.ws, session.Started)
	w.sinks.Sidebar.OnSessionStarted(w.ws, session.Started)
	w.publishLinkLocked()

	if t := session.Started.GetTurnInFlight(); t != nil {
		turn := ids.TurnID(t.GetValue())
		w.turn = &turn
	}

	w.openSessionLocked()
	w.openMainLocked()
	for _, item := range session.Started.GetLiveWork() {
		w.routeDetachedWorkLocked(nil, item)
	}
	w.publishLiveWorkLocked()
	w.mu.Unlock()

	go w.runLink()
	return w, nil
}

// rootAddress is the default output address: the workspace's root feed,
// top-level.
func rootAddress() OutputAddress {
	return OutputAddress{Feed: feedid.Feed{Root: true}}
}

// ---- the Watcher answers ----

// Connected reports whether the daemon-to-shim link is serving.
func (w *watcher) Connected() bool { return w.Link() == shimclient.LinkConnected }

// Link is the current link state.
//
// It is read WITHOUT the watcher's lock, from an atomic mirror of w.link. The
// link is asked for from every direction -- freeness, health, and the host
// view's shim_attached, which is recomposed from inside a sink call the
// watcher makes while holding mu -- and a lock-taking reader there is a
// self-deadlock, not a race.
func (w *watcher) Link() LinkState { return LinkState(w.linkNow.Load()) }

// LiveWork is the current live-work set.
func (w *watcher) LiveWork() LiveWorkSet {
	w.mu.Lock()
	defer w.mu.Unlock()
	return w.liveWorkLocked()
}

// TurnInFlight reports the open turn, nil when none is.
func (w *watcher) TurnInFlight() *ids.TurnID {
	w.mu.Lock()
	defer w.mu.Unlock()
	if w.turn == nil {
		return nil
	}
	turn := *w.turn
	return &turn
}

// Free reports freeness: no turn in flight AND an empty live-work set.
func (w *watcher) Free() bool {
	w.mu.Lock()
	defer w.mu.Unlock()
	return w.turn == nil && w.liveWorkLocked().Empty()
}

// SetOutputAddress installs the address rows are stamped with; nil restores
// the root feed.
func (w *watcher) SetOutputAddress(addr *OutputAddress) {
	w.mu.Lock()
	defer w.mu.Unlock()
	if addr == nil {
		w.addr = rootAddress()
	} else {
		w.addr = *addr
	}
	w.log.Debug("daemon.sessionwatcher.set_output_address", "output address installed", dlog.Context{
		"root": w.addr.Feed.Root,
	})
}

// SetMainAgent names the session's main agent, from StartTurnSuccess.prompt.agent.
func (w *watcher) SetMainAgent(agent *conversationv1.AgentId) {
	if agent.GetValue() == "" {
		return
	}
	w.mu.Lock()
	defer w.mu.Unlock()
	w.adoptMainAgentLocked(agent, "start_turn")
}

// adoptMainAgentLocked latches the main agent's identity the first time it is
// learned, and logs a later disagreement rather than silently re-pointing the
// turn's attribution.
func (w *watcher) adoptMainAgentLocked(agent *conversationv1.AgentId, source string) {
	if agent.GetValue() == "" {
		return
	}
	if w.mainAgent == nil {
		w.mainAgent = agent
		w.log.Debug("daemon.sessionwatcher.main_agent", "main agent named", dlog.Context{
			"agent_id": agent.GetValue(), "source": source,
		})
		return
	}
	if w.mainAgent.GetValue() != agent.GetValue() {
		w.log.Warn("daemon.sessionwatcher.main_agent_changed", "the session's main agent was renamed", dlog.Context{
			"previous_agent_id": w.mainAgent.GetValue(),
			"agent_id":          agent.GetValue(),
			"source":            source,
		})
		w.mainAgent = agent
	}
}

// OnTurnOpened is the prompt queue handing over an accepted turn.
func (w *watcher) OnTurnOpened(ws ids.WorkspaceID, prompt *conversationv1.AgentPrompt, page *conversationv1.HistoryPage) {
	w.mu.Lock()
	defer w.mu.Unlock()

	if ws != w.ws {
		// A turn handed to the wrong workspace's watcher is an invariant
		// violation, and there is nothing useful to do with it.
		w.log.Error("daemon.sessionwatcher.turn_opened_foreign", "a turn was opened on another workspace's watcher", dlog.Context{
			"handed_workspace_id": string(ws), "turn_id": prompt.GetId().GetValue(),
		})
		return
	}

	w.adoptMainAgentLocked(prompt.GetAgent(), "start_turn")
	if turnID := prompt.GetId().GetValue(); turnID != "" {
		turn := ids.TurnID(turnID)
		w.turn = &turn
		// The TURN-OPEN EDGE reaches the footer here and nowhere else: no
		// stream frame states that a turn was accepted.
		w.sinks.Footer.OnTurnOpened(ws, turn)
	} else {
		w.log.Error("daemon.sessionwatcher.turn_opened_unidentified", "an opened turn named no id", dlog.Context{
			"agent_id": prompt.GetAgent().GetValue(),
		})
	}
	w.log.Debug("daemon.sessionwatcher.turn_opened", "the queue opened a turn", dlog.Context{
		"turn_id":  prompt.GetId().GetValue(),
		"agent_id": prompt.GetAgent().GetValue(),
		"entries":  len(page.GetEntries()),
	})
	if page != nil {
		w.routeOpeningPageLocked(w.main, page)
	}
}

// Close tears down every watch this workspace owns. It NEVER kills anything:
// attaching created nothing, so detaching ends nothing.
func (w *watcher) Close() error {
	w.mu.Lock()
	if w.closed {
		w.mu.Unlock()
		return nil
	}
	w.closed = true
	w.gen++
	w.failWaitersLocked()
	closing := w.takeStreamsLocked()
	w.log.Debug("daemon.sessionwatcher.close", "closing the session's watch fleet", dlog.Context{
		"streams": len(closing),
	})
	w.mu.Unlock()

	w.cancel()
	closeStreams(closing)
	return nil
}

// closeStreams runs a taken set of stream closers. It is a function of its own
// because a close must never happen under mu.
func closeStreams(closing []func()) {
	for _, closer := range closing {
		closer()
	}
}

// takeStreamsLocked detaches every open stream from the fleet and returns
// their closers, so the caller closes them WITHOUT holding mu: a stream's own
// goroutine takes mu to report its error, and closing under the lock would
// deadlock against it.
func (w *watcher) takeStreamsLocked() []func() {
	var closing []func()
	if w.sessionStream != nil {
		s := w.sessionStream
		w.sessionStream = nil
		closing = append(closing, s.Close)
	}
	if w.main != nil && w.main.stream != nil {
		s := w.main.stream
		w.main.stream = nil
		closing = append(closing, s.Close)
	}
	for _, a := range w.agents {
		if a.stream != nil {
			s := a.stream
			a.stream = nil
			closing = append(closing, s.Close)
		}
	}
	for _, sh := range w.shells {
		if sh.stream != nil {
			s := sh.stream
			sh.stream = nil
			closing = append(closing, s.Close)
		}
	}
	return closing
}

// ---- the live-work level ----

// liveWorkLocked reports the live set, which IS the open watch set plus the
// monitors that have no stream to open. Sorted so the published value is
// stable frame to frame.
func (w *watcher) liveWorkLocked() LiveWorkSet {
	var live LiveWorkSet
	for _, a := range w.agents {
		if a.work == nil {
			// A SYNC subagent's watch carries no detached-work handle: it is
			// the turn's own progress, not live work, and freeness must not
			// wait on it.
			continue
		}
		live.Agents = append(live.Agents, a.id)
	}
	for _, s := range w.shells {
		live.Shells = append(live.Shells, s.work)
	}
	for _, m := range w.monitors {
		live.Monitors = append(live.Monitors, m)
	}
	sort.Slice(live.Agents, func(i, j int) bool { return live.Agents[i].GetValue() < live.Agents[j].GetValue() })
	sort.Slice(live.Shells, func(i, j int) bool { return live.Shells[i].GetValue() < live.Shells[j].GetValue() })
	sort.Slice(live.Monitors, func(i, j int) bool { return live.Monitors[i].GetValue() < live.Monitors[j].GetValue() })
	return live
}

// publishLiveWorkLocked republishes the live set: combined with the in-flight
// turn it is the freeness answer every lease holder waits on.
func (w *watcher) publishLiveWorkLocked() {
	live := w.liveWorkLocked()
	w.log.Debug("daemon.sessionwatcher.live_work", "live-work set changed", dlog.Context{
		"agents": len(live.Agents), "shells": len(live.Shells), "monitors": len(live.Monitors),
	})
	w.sinks.Lifecycle.OnLiveWorkChanged(w.ws, live)
	// The roster hears the same set: its `idle_async` arm retires on an empty
	// one, which no announcement can ever state.
	w.sinks.Sidebar.OnLiveWorkChanged(w.ws, live)
	// The live-work set is half of freeness, so every change to it is a
	// freeness edge a lease holder may be waiting on.
	w.signalFreenessLocked()
}

// ---- connectivity ----

// runLink consumes the client's connectivity truth for as long as the client
// publishes it, and re-opens the fleet when a broken link comes back.
func (w *watcher) runLink() {
	for state := range w.client.Connectivity() {
		w.mu.Lock()
		if w.closed {
			w.mu.Unlock()
			return
		}
		previous := w.link
		degraded := w.degraded
		w.setLinkLocked(state)
		if state == shimclient.LinkConnected && previous != shimclient.LinkConnected && degraded {
			w.reopenLocked("the link came back")
		}
		w.mu.Unlock()
	}
	w.log.Debug("daemon.sessionwatcher.link_feed_ended", "the shim client stopped publishing connectivity", nil)
}

// setLinkLocked records a link transition and republishes it. A transition to
// the state already held is not a transition.
func (w *watcher) setLinkLocked(state LinkState) {
	if w.link == state {
		return
	}
	// DEAD IS TERMINAL. The shim process being gone is the strongest evidence
	// the daemon has about this hop, and it arrives on the client's exit path
	// while every standing stream is breaking of the very same cause. A stream
	// break after death is a CONSEQUENCE of it, never a fresh severing, so it
	// must not walk the link back to redialing: this client redials no more,
	// and a revival is a new client with a new watcher.
	if w.link == shimclient.LinkDead {
		w.log.Debug("daemon.sessionwatcher.link", "the link is dead; a later transition is a consequence of the death", dlog.Context{
			"proposed": int(state),
		})
		return
	}
	w.log.Debug("daemon.sessionwatcher.link", "link state changed", dlog.Context{
		"previous": int(w.link), "link": int(state),
	})
	w.link = state
	w.linkNow.Store(int32(state))
	w.publishLinkLocked()
}

// publishLinkLocked hands the link to the three views that draw it, and the
// bare attachment to the daemon's own machinery (the host view's
// `shim_attached`, which no view sink carries).
func (w *watcher) publishLinkLocked() {
	w.sinks.Footer.OnLink(w.ws, w.link)
	w.sinks.Topbar.OnLink(w.ws, w.link)
	w.sinks.Sidebar.OnLink(w.ws, w.link)
	w.sinks.Lifecycle.OnLinkChanged(w.ws, w.link == shimclient.LinkConnected)
}

// severedLocked records a transport failure on a stream that should still have
// been open. The link is not connected while a standing stream is down, whatever
// the socket says: invariant 11 witnesses the hop by the stream's liveness.
func (w *watcher) severedLocked(operation, detail string, err error) {
	ctx := dlog.Context{"detail": detail}
	if err != nil {
		ctx["error"] = err.Error()
	}
	w.log.Error("daemon.sessionwatcher."+operation, "a standing stream ended without the session ending", ctx)
	w.degraded = true
	w.setLinkLocked(shimclient.LinkRedialing)
}

// reopenLocked tears the fleet down and opens it again, each watch catching up
// from the newest pointer it was served. The generation bump is what makes the
// old goroutines' errors stale rather than a second severing.
func (w *watcher) reopenLocked(reason string) {
	w.gen++
	// The fleet is whole again from here: any open below that fails calls
	// severedLocked, which sets the flag afresh.
	w.degraded = false
	// The closers run OFF the lock, for the reason takeStreamsLocked states:
	// a stream's Close drains its response body and does not return until the
	// SERVER ends the stream, and a standing watch is never ended by the
	// daemon side. Closing here would hold mu for the whole drain and wedge
	// every caller of the watcher -- the prompt queue's freeness read included.
	// The generation bump above is what makes the detached streams' goroutines
	// stale, so nothing waits on the close completing.
	go closeStreams(w.takeStreamsLocked())
	w.log.Warn("daemon.sessionwatcher.reopen", "re-opening the session's watches", dlog.Context{
		"reason": reason, "agents": len(w.agents), "shells": len(w.shells),
	})

	w.openSessionLocked()
	w.openMainLocked()
	for _, a := range w.agents {
		w.openAgentStreamLocked(a)
	}
	for _, s := range w.shells {
		w.openShellStreamLocked(s)
	}
}

// ---- opening watches ----

// openSessionLocked opens WatchSession, the session's standing stream.
func (w *watcher) openSessionLocked() {
	gen := w.gen
	stream, err := w.client.WatchSession(w.ctx)
	if err != nil {
		w.severedLocked("watch_session", "WatchSession could not be opened", err)
		return
	}
	w.sessionStream = stream
	w.log.Debug("daemon.sessionwatcher.watch_session", "session watch opened", nil)
	go w.runSession(gen, stream)
}

// openMainLocked opens the main agent's watch: an UNSET target, which the shim
// resolves to the session's prompt thread.
func (w *watcher) openMainLocked() {
	if w.main == nil {
		w.main = &agentWatch{}
	}
	w.openAgentStreamLocked(w.main)
}

// openAgentStreamLocked opens (or re-opens) one agent watch, catching up from
// the newest pointer that watch was served.
func (w *watcher) openAgentStreamLocked(a *agentWatch) {
	gen := w.gen
	req := &shimv1.WatchAgentRequest{Target: a.id, PageSize: openingPageSize}
	if ptr := w.known[watchKey(a.id)]; ptr != nil {
		req.KnownThrough = ptr
	}
	stream, err := w.client.WatchAgent(w.ctx, req)
	if err != nil {
		w.severedLocked("watch_agent", "WatchAgent could not be opened", err)
		return
	}
	a.stream = stream
	w.log.Debug("daemon.sessionwatcher.watch_agent", "agent watch opened", dlog.Context{
		"agent_id": a.id.GetValue(), "catch_up": req.KnownThrough != nil,
	})
	go w.runAgent(gen, a, stream)
}

// openShellStreamLocked opens (or re-opens) one detached shell's watch.
func (w *watcher) openShellStreamLocked(s *shellWatch) {
	gen := w.gen
	stream, err := w.client.WatchBash(w.ctx, s.work)
	if err != nil {
		w.severedLocked("watch_bash", "WatchBash could not be opened", err)
		return
	}
	s.stream = stream
	w.log.Debug("daemon.sessionwatcher.watch_bash", "shell watch opened", dlog.Context{
		"work_id": s.work.GetValue(),
	})
	go w.runShell(gen, s, stream)
}

// watchKey is a watch's key in the known_through map.
func watchKey(id *conversationv1.AgentId) string {
	if id == nil {
		return mainWatchKey
	}
	return id.GetValue()
}

// ---- consuming streams ----

// runSession consumes the session's standing stream.
func (w *watcher) runSession(gen uint64, stream shimclient.Stream[*conversationv1.SessionUpdate]) {
	for {
		update, err := stream.Recv()
		if err != nil {
			w.streamEnded(gen, "session", "watch_session", "", nil, err)
			return
		}
		w.mu.Lock()
		if w.stale(gen) {
			w.mu.Unlock()
			return
		}
		w.routeSessionUpdateLocked(update)
		w.mu.Unlock()
		w.flushTurnEnds()
	}
}

// runAgent consumes one agent's stream.
func (w *watcher) runAgent(gen uint64, a *agentWatch, stream shimclient.Stream[*shimv1.WatchAgentResponse]) {
	for {
		resp, err := stream.Recv()
		if err != nil {
			w.streamEnded(gen, "agent", "watch_agent", a.id.GetValue(), func() bool { return a.done }, err)
			return
		}
		w.mu.Lock()
		if w.stale(gen) {
			w.mu.Unlock()
			return
		}
		w.routeAgentResponseLocked(a, resp)
		w.mu.Unlock()
		w.flushTurnEnds()
	}
}

// runShell consumes one detached shell's stream.
func (w *watcher) runShell(gen uint64, s *shellWatch, stream shimclient.Stream[*conversationv1.AgentBash]) {
	for {
		bash, err := stream.Recv()
		if err != nil {
			w.streamEnded(gen, "shell", "watch_bash", s.work.GetValue(), func() bool { return s.done }, err)
			return
		}
		w.mu.Lock()
		if w.stale(gen) {
			w.mu.Unlock()
			return
		}
		w.routeBashLocked(s, bash)
		w.mu.Unlock()
		w.flushTurnEnds()
	}
}

// endedTurn is one turn end waiting to be handed to the lifecycle sink.
type endedTurn struct {
	turn ids.TurnID
	how  TurnClose
}

// flushTurnEnds hands every recorded turn end to the lifecycle sink WITHOUT
// the lock. Every site that routes under mu calls it right after unlocking.
func (w *watcher) flushTurnEnds() {
	w.mu.Lock()
	pending := w.pendingTurnEnds
	w.pendingTurnEnds = nil
	w.mu.Unlock()
	for _, ended := range pending {
		w.sinks.Lifecycle.OnTurnEnded(w.ws, ended.turn, ended.how)
	}
}

// stale reports whether a goroutine's generation has been superseded, which
// means its stream was torn down deliberately.
func (w *watcher) stale(gen uint64) bool { return w.closed || gen != w.gen }

// streamEnded decides what a stream's end MEANT. Only the consumer can: a
// producer-side end is legal when the fleet tore the stream down or the
// session is over, and is a transport failure otherwise.
func (w *watcher) streamEnded(gen uint64, kind, operation, key string, reaped func() bool, err error) {
	w.mu.Lock()
	defer w.mu.Unlock()

	if w.stale(gen) || (reaped != nil && reaped()) {
		w.log.Debug("daemon.sessionwatcher.stream_closed", "a torn-down stream ended", dlog.Context{
			"stream": kind, "key": key,
		})
		return
	}
	if w.sessionEnded {
		w.log.Debug("daemon.sessionwatcher.stream_closed", "a stream ended with its session", dlog.Context{
			"stream": kind, "key": key,
		})
		return
	}
	w.severedLocked(operation, kind+" stream ended while the session was live", err)
}
