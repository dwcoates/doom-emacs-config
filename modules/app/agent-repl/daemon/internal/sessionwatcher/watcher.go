package sessionwatcher

import (
	"context"
	"errors"
	"sort"
	"sync"
	"sync/atomic"
	"time"

	"connectrpc.com/connect"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
	"claude-repld/internal/lockwatch"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/wsm"
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
	// refusals counts how many times this watch's OPEN was refused before any
	// frame. It is BOUNDED: a shim that refuses forever is reported as a
	// fault rather than retried forever.
	refusals int
	// opening is the open IN FLIGHT for this watch, nil when none is. The
	// open runs OFF mu (see openTicket), and the ticket is what its
	// completion is judged against: a completion whose ticket is no longer
	// this field is superseded and discarded.
	opening *openTicket
	// paged records that this watch has been served an opening page — its
	// own, or the one StartTurn's answer carried for the main watch. From then
	// on, everything a later opening page carries was written after it.
	paged bool
	// catchUp records that the open in force asked for a CATCH-UP page: one
	// carrying only entries this watch has not been served, because the open
	// named a known_through pointer or the watch had already been paged. Its
	// entries were written while no stream stood, so a cut among them is an
	// edge the views missed and is routed (routePageCutLocked). A watch's
	// FIRST page is a repaint instead: history, whose cuts are never edges.
	catchUp bool
}

// shellWatch is one open WatchBash stream for one detached shell.
type shellWatch struct {
	work   *conversationv1.DetachedWorkId
	stream shimclient.Stream[*conversationv1.AgentBash]
	// done marks a watch REAPED at its terminal, as on agentWatch.
	done bool
	// reopens counts how many times this shell's stream ended before the
	// shell settled and was opened again. It is BOUNDED: a shim that ends the
	// stream the instant it is opened must sever rather than spin.
	reopens int
	// refusals counts refused OPENs, as on agentWatch.
	refusals int
	// opening is the open in flight, as on agentWatch.
	opening *openTicket
}

// shellReopenLimit is how many times one detached shell's watch is re-opened
// after its stream ends early before the failure is treated as the link being
// severed.
const shellReopenLimit = 3

// openRefusalLimit is how many times one watch's REFUSED open is retried
// before the refusal is reported as a fault. A refused open is not a transport
// failure, so exhausting it never severs the link.
const openRefusalLimit = 3

// stallLockName is how the stall watchdog names the watcher's mutex.
const stallLockName = "sessionwatcher.watcher"

// watcher is one live workspace's watch fleet: every shim watch the session
// owns, the daemon-to-shim hop of connectivity truth, and the routing of every
// frame into the resolvers' sinks.
//
// SERIALIZATION: mu guards every field AND is held across every sink call, so
// the resolvers observe frames in stream order per agent however many streams
// are being consumed at once. Each stream has its own goroutine; the only work
// done outside mu is blocking on Recv, and every watch OPEN.
//
// NO SHIM I/O IS EVER PERFORMED UNDER mu. A watch open is a round trip to the
// shim that does not return until the shim serves the stream's FIRST frame
// (shimclient.openStream takes it to surface a refused open), and a shim that
// never serves one holds the call forever. Made under mu, that call held the
// lock with it: MEASURED 2026-09-27T14:00:44, a detached shell's WatchBash
// open never got its first frame, and for eighteen minutes every other stream
// goroutine, TurnInFlight and so every SubmitPrompt for the workspace waited
// on this mutex. Every open is therefore DECIDED under mu, MADE on a goroutine
// of its own, and INSTALLED under mu again only if the fleet still wants it
// (see openTicket).
type watcher struct {
	ws     ids.WorkspaceID
	client shimclient.Client
	sinks  Sinks
	log    dlog.Logger

	ctx    context.Context
	cancel context.CancelFunc

	mu lockwatch.Mutex
	// unwatch retires mu from the stall watchdog; Close calls it. A no-op
	// when no watchdog was wired.
	unwatch func()

	// gen is the fleet's generation. A re-open bumps it, so a goroutine whose
	// stream was torn down under it recognizes its own error as stale and
	// exits silently instead of reporting a transport failure.
	gen    uint64
	closed bool
	// departure is set once the watched shim is GONE (see Departed), and it
	// is set ONCE: the lifecycle sink hears one departure per watcher.
	departure *Departure
	// sessionEnded records that the session itself is over — the vendor query
	// died, or the daemon is deliberately ending it (SessionEnding) — which is
	// the one way a stream may legally end without a terminal.
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
	// fleetConnections is the client's Connections() when the current fleet
	// was opened. A severing that finds the count already past it knows the
	// link broke AND came back before this watcher heard its own stream end,
	// so the LinkConnected that would have re-opened the fleet has already
	// been consumed, with degraded still false (severedLocked).
	fleetConnections uint64

	link LinkState
	// linkNow mirrors link for the lock-free readers; every write to link
	// writes it under mu, so the mirror can never lead the truth.
	linkNow atomic.Int32
	addr    OutputAddress

	turn *ids.TurnID
	// factsTurn is the turn a PURE ATTACH's re-announced facts stood in
	// flight: one the shim was already running when this watcher attached. No StartTurn of this
	// watcher's will ever name the main agent for it, so its main-watch
	// terminal is attributed from the watch itself (routeTerminalLocked).
	factsTurn ids.TurnID
	mainAgent *conversationv1.AgentId
	// viewsMain is the main agent the FEED was last told
	// (nameMainForViewsLocked). It is learned from StartTurn's naming AND from
	// the main watch itself — every frame that watch carries is the main
	// agent's — so the views know the root's owner before they route the first
	// frame that needs it, whichever plane arrives first.
	viewsMain *conversationv1.AgentId
	// held is a main-watch terminal that arrived BEFORE the main agent was
	// named — the stream plane outrunning StartTurn's answer. It is replayed
	// in full at the naming, so a turn is never left standing in flight with
	// its only ending edge already spent. See routeTerminalLocked.
	held *heldTerminal

	// freeWaiters are the standing AwaitFree calls. They are answered by the
	// stream edges — a turn end and a live-work change — never by a poll.
	freeWaiters []chan error
	// busy is what the last freeness judgement found: work in flight. The
	// lifecycle sink's OnFree fires on its true-to-false edge alone.
	busy bool
	// turnWaiters are the standing AwaitTurnEnd calls, keyed by the turn.
	turnWaiters map[ids.TurnID][]chan turnEnd
	// pendingTurnEnds are the turn ends recorded under mu and not yet handed
	// to the lifecycle sink, which is told only once mu is released.
	pendingTurnEnds []endedTurn
	// openAtAttach is Session.OpenAtAttach, held until the session facts
	// arrive and then consumed ONCE: see reconcileOpenAtAttachLocked.
	openAtAttach []OpenTurn
	// pendingUnobserved are the turns the facts showed ended unobserved, not
	// yet handed to the lifecycle sink; flushTurnEnds hands them over with
	// the turn ends.
	pendingUnobserved []ids.TurnID
	// pendingAdoptions are the vendor-started turns this watcher stood in
	// flight and has not yet handed to the lifecycle sink; flushTurnEnds hands
	// them over ahead of the turn ends.
	pendingAdoptions []ids.TurnID
	// adopted is the turn in flight when a VENDOR_STARTED prompt row stood it
	// there (adoptVendorTurnLocked), nil otherwise.
	adopted *ids.TurnID
	// turnAccepted records that the turn in flight is past its opening: the
	// queue handed it over (OnTurnOpened), a row or the session's facts stood
	// it. False only for a turn OnTurnOpening stood that StartTurn has not yet
	// answered for.
	turnAccepted bool
	// waiting are the turns an adopted turn stood BEHIND it, newest last: a
	// turn of the daemon's own whose send reached the shim's vendor queue as
	// the vendor started a turn of its own, or an adopted turn a newer one
	// arrived beside. The shim matches each reply to its send by id (ruled
	// 2026-09-28) and never refuses a StartTurn for a vendor-started turn, so
	// both run; the one RUNNING stands in flight, and each waiting turn
	// stands in flight again, newest first, when the turn ahead of it ends
	// (turnEndedLocked). Invariant: waiting is empty whenever turn is nil.
	waiting []waitingTurn

	// dispatching tracks the OFF-LOCK sink dispatch (flushTurnEnds), so Close
	// can join it. It is a WaitGroup rather than a sleep.
	dispatching sync.WaitGroup
	// opens tracks every watch open in flight (see openTicket), so Close can
	// join them once it has cancelled them. An open is only ever minted under
	// mu on a fleet that is not closed, and Close marks the fleet closed under
	// mu before it waits, so no open is counted after the wait begins.
	opens sync.WaitGroup
	// closedTurns remembers how the last few turns ended, so a wait that
	// arrives after the terminal is still answered; closedTurnOrder is its
	// eviction order.
	closedTurns     map[ids.TurnID]TurnClose
	closedTurnOrder []ids.TurnID
	// knownTurns is every turn this watcher has had in hand — stood in flight,
	// or carried by a routed prompt or a page's stamps — so a terminal naming
	// a turn is judged against what the daemon actually knows
	// (terminalTurnLocked). One id per turn, never evicted.
	knownTurns map[ids.TurnID]struct{}
	// unstampedTerminalSeen records that the once-per-watcher INFO for a turn
	// terminal with no stamp has been written.
	unstampedTerminalSeen bool

	// seenClosings is every row that CLOSES AN ACT — an agent's terminal, or
	// a context cut — this watcher has been served, keyed by the watch that
	// carried it and the row's pointer (closingKey). Each is a row of its own
	// in the book, so its pointer IS its identity, and a second sighting of it
	// is a REPLAY: the shim re-serving rows it already served after the store
	// ended a standing watch. A replay is dropped whole; it never reaches a
	// view, is never charged to the turn now open, and never ends a
	// compaction that began after it. See routeAgentFrameLocked and
	// routePageClosingsLocked.
	//
	// UNBOUNDED BY DESIGN: it grows by one per closing row, and a bounded
	// memory would re-open exactly the hole it closes for the oldest rows.
	seenClosings map[string]struct{}

	// retiredWork is every detached-work HANDLE this watcher reaped at its
	// terminal. A retired handle is never live again: the contract retires a
	// handle at its run's end, by equality, and a later announcement for it is
	// a RE-SERVING — the vendor's end-of-run notification upserts the
	// announcement row to add the output path, and that upsert rides the live
	// watch again. Re-admitting it put a finished run back in the live set,
	// re-opened its watch and re-placed its whole sub-feed, and its settle then
	// dropped it a millisecond later: the footer's "added agent" churn. The
	// footer holds the same rule for the same reason (chips.go retiredWork).
	//
	// UNBOUNDED BY DESIGN, as seenClosings is: one entry per detached run.
	retiredWork map[string]struct{}

	sessionStream shimclient.Stream[*shimv1.WatchSessionResponse]
	// sessionOpening is the session watch's open in flight, as on agentWatch.
	sessionOpening *openTicket
	// started records that the session facts have been taken up, from
	// StartSession's answer or the shim's re-announcement. It is what makes a
	// repeat re-announcement idempotent.
	started bool
	// factsIn closes the first time the session facts are taken up, which is
	// what AwaitSessionFacts waits on.
	factsIn chan struct{}
	main    *agentWatch

	// agents is one entry per LIVE DETACHED SUBAGENT, keyed by AgentId.value.
	// The main watch is not in it: the open set here IS the live subagent set.
	agents map[string]*agentWatch
	// shells is one entry per live detached shell, keyed by DetachedWorkId.value.
	shells map[string]*shellWatch
	// live is THE LIVE-WORK LEDGER: one entry per detached item the shim has
	// announced and not yet concluded, keyed by DetachedWorkId.value. It is
	// the one thing the live-work set is read from, and it is SEPARATE FROM
	// THE WATCHES: an item is admitted at its announcement and leaves only
	// through concludeLocked, at the shim's own conclusion of it, whether its
	// watch is open, still opening, refused, or was never opened at all (a
	// monitor has none; an unaddressable subagent cannot have one). See
	// livework.go.
	live map[string]*liveItem
	// liveSeq numbers admissions to the ledger, so a re-announcement is only
	// ever reconciled against items admitted BEFORE the shim was asked for
	// it (see reconcileLiveWorkLocked).
	liveSeq uint64

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

	// unseenAsks are the asks whose notification RAISED the roster's attention
	// marker and that have not settled yet, keyed by askKey.
	//
	// THE MARKER MEANS AN UNSEEN NOTIFICATION (frontend.v1.RosterRow.attention),
	// and an ask that has been decided is not one: the user answered it, or it
	// was decided without them. So the LAST ask settling retires the marker,
	// exactly as selecting the workspace does. It is a SET rather than a count
	// because a workspace with a second ask still open still has something
	// unseen, and because a settle frame for an ask this watcher never
	// announced must retire nothing.
	unseenAsks map[string]struct{}
}

// activityFact is what one routed activity taught the watcher about the unit
// it addressed.
type activityFact struct {
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
	// kindUnknown is an announcement that named no kind. It is refused, never
	// routed.
	kindUnknown detachedKind = iota
	// kindSubagent is a detached subagent, watched with WatchAgent.
	kindSubagent
	// kindBash is a detached shell, watched with WatchBash.
	kindBash
	// kindMonitor is a background watcher. No stream exists; its feed entry
	// is its call's tool-call card.
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
	if sinks.Feed == nil || sinks.Footer == nil || sinks.Topbar == nil || sinks.Sidebar == nil || sinks.Lifecycle == nil {
		return nil, errors.New("sessionwatcher: every sink but Holds is required")
	}
	if err := session.Opening.validate(); err != nil {
		log.Error("daemon.sessionwatcher.start_refused", "a watcher was started without deciding whether it replays or resumes", dlog.Context{
			"workspace_id": string(ws),
		})
		return nil, err
	}

	runCtx, cancel := context.WithCancel(ctx)
	w := &watcher{
		ws:     ws,
		client: client,
		sinks:  sinks,
		log:    log.With(dlog.Context{"workspace_id": string(ws)}),
		ctx:    runCtx,
		cancel: cancel,
		link:   shimclient.LinkConnected,
		addr:   rootAddress(),
		agents: map[string]*agentWatch{},
		shells: map[string]*shellWatch{},
		live:   map[string]*liveItem{},
		known:  map[string]*conversationv1.HistoryPointer{},

		turnWaiters: map[ids.TurnID][]chan turnEnd{},
		closedTurns: map[ids.TurnID]TurnClose{},
		knownTurns:  map[ids.TurnID]struct{}{},
		facts:       map[string]*activityFact{},

		seenClosings: map[string]struct{}{},
		retiredWork:  map[string]struct{}{},
		unseenAsks:   map[string]struct{}{},

		openAtAttach: session.OpenAtAttach,
		factsIn:      make(chan struct{}),
	}
	w.linkNow.Store(int32(shimclient.LinkConnected))
	w.unwatch = func() {}
	if sinks.Stalls != nil {
		w.unwatch = sinks.Stalls.Watch(&w.mu, stallLockName, ws, w.log)
	}
	// A RESUME STARTS FROM ITS PREDECESSOR'S POINTERS, so every watch it opens
	// is a catch-up; a replay starts from none, so every watch opens on its
	// first page. Nothing else seeds the map: see opening.go.
	if session.Opening.from.Main != nil {
		w.known[mainWatchKey] = session.Opening.from.Main
	}
	for id, ptr := range session.Opening.from.Agents {
		w.known[id] = ptr
	}

	w.mu.Lock()
	w.log.Info("daemon.sessionwatcher.start", "opening the session's watch fleet", dlog.Context{
		"vendor_session_id": session.Started.GetVendorSessionId(),
		"turn_in_flight":    session.Started.GetTurnInFlight().GetValue(),
		"turns_waiting":     len(session.Started.GetTurnsWaiting()),
		"live_work":         len(session.Started.GetLiveWork()),
		// A PURE ATTACH opens with no facts at all: an adopting daemon
		// (crash boot, handover) learns them from the shim's own
		// re-announcement on the watch it is about to open (landing 7).
		"attached": session.Started == nil,
		// WHETHER THIS WATCHER REPLAYS HISTORY, and why: the one fact that
		// says whether its opening pages are first pages or catch-ups.
		"opening":          session.Opening.String(),
		"resumed_pointers": len(w.known),
		"open_at_attach":   len(session.OpenAtAttach),
	})

	if session.Started != nil {
		w.applySessionStartedLocked(session.Started)
	}
	w.publishLinkLocked()

	w.fleetConnections = w.client.Connections()
	w.openSessionLocked()
	w.openMainAfterFactsLocked()
	if session.Started != nil {
		w.adoptLiveWorkLocked(session.Started)
	}
	w.publishLiveWorkLocked()
	w.mu.Unlock()
	// Facts handed to Start may already have closed turns the adoption found
	// open; they are handed over now, off the lock, as every turn end is.
	w.flushTurnEnds()

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

// Pointers is the newest pointer this watcher was served on each watch. It
// reads the same map a re-open reads, and it is answered after Close too: the
// fleet asks a RETIRED watcher for it when opening that watcher's successor.
func (w *watcher) Pointers() Pointers {
	w.mu.Lock()
	defer w.mu.Unlock()
	out := Pointers{Agents: make(map[string]*conversationv1.HistoryPointer, len(w.known))}
	for key, ptr := range w.known {
		if key == mainWatchKey {
			out.Main = ptr
			continue
		}
		out.Agents[key] = ptr
	}
	return out
}

// MainKnownThrough is the newest pointer the main agent's watch was served.
func (w *watcher) MainKnownThrough() *conversationv1.HistoryPointer {
	w.mu.Lock()
	defer w.mu.Unlock()
	return w.known[mainWatchKey]
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

// turnIDValue renders an optional turn for transition records.
func turnIDValue(turn *ids.TurnID) string {
	if turn == nil {
		return ""
	}
	return string(*turn)
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
	before := w.addr.Feed.Root
	if addr == nil {
		w.addr = rootAddress()
	} else {
		w.addr = *addr
	}
	w.log.Debug("daemon.sessionwatcher.set_output_address", "output address installed", dlog.Context{
		"root": w.addr.Feed.Root, "state": "output_feed_root", "before": before, "after": w.addr.Feed.Root,
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
		before := ""
		w.mainAgent = agent
		w.log.Debug("daemon.sessionwatcher.main_agent", "main agent named", dlog.Context{
			"agent_id": agent.GetValue(), "state": "main_agent", "before": before, "after": agent.GetValue(), "source": source,
		})
		w.nameMainForViewsLocked(agent, source, true)
		// THE RELEASE IS NOT DONE HERE. Naming is a precondition for it, not
		// the moment for it: the caller may still owe the views the turn's
		// OPEN edge, and a terminal replayed before that edge leaves the
		// footer with a turn it never saw start. Each naming site releases
		// when it has finished handing over what it knows.
		return
	}
	if w.mainAgent.GetValue() != agent.GetValue() {
		before := w.mainAgent.GetValue()
		w.log.Warn("daemon.sessionwatcher.main_agent_changed", "the session's main agent was renamed", dlog.Context{
			"previous_agent_id": w.mainAgent.GetValue(),
			"agent_id":          agent.GetValue(),
			"source":            source,
		})
		w.mainAgent = agent
		w.log.Debug("daemon.sessionwatcher.state_transition", "the main-agent identity changed", dlog.Context{
			"state": "main_agent", "before": before, "after": agent.GetValue(), "source": source,
		})
		w.nameMainForViewsLocked(agent, source, true)
	}
}

// nameMainForViewsLocked tells the feed and the footer which agent is the
// session's main one, once per distinct naming.
//
// THE ROOT IS THE MAIN AGENT'S FEED AND NOTHING ELSE'S. The feed used to latch
// the first agent it ever saw a frame for as the main one, which is a default:
// a subagent's frame arriving first would have put the whole conversation on a
// sub-feed. The naming is stated instead, from the two places that know —
// StartTurn's answer (adoptMainAgentLocked) and the main watch, whose frames
// are the main agent's — before the frame that needs it is routed.
//
// ONLY THE AUTHORITATIVE NAMING RENAMES. A main-watch row names the main agent
// when nothing has yet; once one stands, a row on that watch naming another
// agent is that agent's own frame, placed by its own feed, and never a rename.
func (w *watcher) nameMainForViewsLocked(agent *conversationv1.AgentId, source string, authoritative bool) {
	if agent.GetValue() == "" || w.viewsMain.GetValue() == agent.GetValue() {
		return
	}
	if !authoritative && w.viewsMain != nil {
		return
	}
	before := w.viewsMain.GetValue()
	w.viewsMain = agent
	if before != "" {
		w.log.Warn("daemon.sessionwatcher.views_main_agent_changed", "the views' main agent was renamed", dlog.Context{
			"previous_agent_id": before, "agent_id": agent.GetValue(), "source": source,
		})
	} else {
		w.log.Info("daemon.sessionwatcher.views_main_agent", "the feed was told the session's main agent", dlog.Context{
			"agent_id": agent.GetValue(), "source": source,
		})
	}
	w.sinks.Feed.OnMainAgent(w.ws, agent)
	w.sinks.Footer.OnMainAgent(w.ws, agent)
}

// waitingTurn is one turn stood behind an adopted turn in flight.
type waitingTurn struct {
	turn ids.TurnID
	// accepted records that the queue handed the turn over (OnTurnOpened), so
	// the views are given its open edge when it stands in flight again. A
	// turn still opening gets that edge from its own OnTurnOpened.
	accepted bool
	// adopted records that the turn was itself stood by a VENDOR_STARTED row.
	adopted bool
}

// OnTurnOpening records the turn a caller is about to hand to the shim. See
// the interface for why the record has to go down BEFORE StartTurn.
func (w *watcher) OnTurnOpening(ws ids.WorkspaceID, turn ids.TurnID) {
	w.mu.Lock()
	defer w.mu.Unlock()
	if ws != w.ws {
		w.log.Error("daemon.sessionwatcher.turn_opening_foreign", "a turn was opened on another workspace's watcher", dlog.Context{
			"handed_workspace_id": string(ws), "turn_id": string(turn),
		})
		return
	}
	if turn == "" {
		w.log.Error("daemon.sessionwatcher.turn_opening_unidentified", "an opening turn named no id", nil)
		return
	}
	if w.waitBehindAdoptedLocked(turn, false) {
		return
	}
	if w.standTurnLocked(turn, "daemon.sessionwatcher.turn_opening", "a turn is going to the shim") {
		w.turnAccepted = false
	}
}

// acceptedLocked reports whether `turn` is the turn in flight and past its
// opening.
func (w *watcher) acceptedLocked(turn ids.TurnID) bool {
	return w.turn != nil && *w.turn == turn && w.turnAccepted
}

// waitBehindAdoptedLocked stands a turn of the daemon's own BEHIND the
// adopted turn in flight, rather than displacing it, and reports whether it
// did. The vendor-started turn is the one running; the shim delivered this
// turn's prompt into the vendor's queue behind it and matches each turn's
// reply to its own send (ruled 2026-09-28), so displacing the running turn
// would leave its terminal ending nothing and its row open forever.
func (w *watcher) waitBehindAdoptedLocked(turn ids.TurnID, accepted bool) bool {
	if w.adopted == nil || w.turn == nil || *w.turn != *w.adopted || *w.turn == turn {
		return false
	}
	if _, ended := w.closedTurns[turn]; ended {
		return false
	}
	for i := range w.waiting {
		if w.waiting[i].turn == turn {
			w.waiting[i].accepted = w.waiting[i].accepted || accepted
			w.log.Debug("daemon.sessionwatcher.turn_waiting", "a turn already waiting behind the adopted turn was handed over", dlog.Context{
				"turn_id": string(turn), "turn_in_flight": string(*w.turn), "accepted": w.waiting[i].accepted,
			})
			return true
		}
	}
	w.waiting = append(w.waiting, waitingTurn{turn: turn, accepted: accepted})
	w.knownTurns[turn] = struct{}{}
	w.log.Info("daemon.sessionwatcher.turn_waiting", "a turn of the daemon's own waits behind the vendor-started turn in flight", dlog.Context{
		"turn_id": string(turn), "turn_in_flight": string(*w.turn), "accepted": accepted,
	})
	return true
}

// standTurnLocked is the ONE writer that puts a turn in flight, and its callers
// are the edges that KNOW a turn is running: the queue handing a turn to the
// shim (OnTurnOpening), the shim accepting it (OnTurnOpened), and the session's
// own facts naming the turn already running at a start or adoption
// (applySessionStartedLocked). No stream row opens a turn: rows can be served
// again, and a re-served prompt row once stood a finished turn back up in
// flight in this watcher alone — the queue held every later prompt behind it
// while the turn record, the footer and the roster all called it closed. The
// one exception is a VENDOR_STARTED prompt row (adoptVendorTurnLocked), which
// is the only statement of its turn's open, and which opens only a turn this
// watcher has never had in hand.
//
// A TURN THAT ALREADY ENDED STAYS ENDED. Its end was already handed to every
// observer, and nothing would ever end it a second time. It reports whether
// the turn now stands in flight.
func (w *watcher) standTurnLocked(turn ids.TurnID, operation, message string) bool {
	if _, ended := w.closedTurns[turn]; ended {
		w.log.Debug("daemon.sessionwatcher.turn_already_ended", "a turn that already ended was offered as in flight; it stays ended", dlog.Context{
			"turn_id": string(turn), "source": operation,
		})
		return false
	}
	before := turnIDValue(w.turn)
	w.turn = &turn
	w.turnAccepted = true
	if w.adopted != nil && *w.adopted != turn {
		w.adopted = nil
	}
	w.knownTurns[turn] = struct{}{}
	w.log.Debug(operation, message, dlog.Context{
		"turn_id": string(turn), "state": "turn_in_flight", "before": before, "after": string(turn),
	})
	return true
}

// OnTurnOpenFailed retires a turn the shim refused. It clears the record only
// when that turn is still the one in flight: a terminal that already ended it
// wins, and so does a later turn.
func (w *watcher) OnTurnOpenFailed(ws ids.WorkspaceID, turn ids.TurnID) {
	w.mu.Lock()
	defer w.mu.Unlock()
	if ws != w.ws {
		return
	}
	if w.dropWaitingLocked(turn, "the shim refused a turn waiting behind the adopted turn") {
		w.flushHeldTerminalLocked()
		w.signalFreenessLocked()
		return
	}
	if w.turn == nil || *w.turn != turn {
		return
	}
	before := turnIDValue(w.turn)
	w.turn = nil
	w.log.Debug("daemon.sessionwatcher.turn_open_failed", "the shim refused a turn; it no longer stands in flight", dlog.Context{
		"turn_id": string(turn), "state": "turn_in_flight", "before": before, "after": "",
	})
	// The refusal IS the answer that would have named the main agent, so a
	// terminal held for that name has nothing left to wait on.
	w.flushHeldTerminalLocked()
	w.signalFreenessLocked()
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
		// A VENDOR-STARTED TURN IS RUNNING AHEAD OF IT: the accepted turn waits
		// behind it and takes its open edges when it stands in flight.
		if w.waitBehindAdoptedLocked(turn, true) {
			if page != nil {
				w.routeOpeningPageLocked(w.main, page, pageTurnAccepted)
			}
			w.releaseHeldTerminalLocked()
			return
		}
		// THE TURN MAY ALREADY BE OVER. Its terminal can beat StartTurn's
		// response back — the very race OnTurnOpening exists for — and
		// re-recording it would stand a dead turn back up in flight, with no
		// edge left to take it down again.
		if !w.standTurnLocked(turn, "daemon.sessionwatcher.state_transition", "the accepted turn became the turn in flight") {
			return
		}
		// The TURN-OPEN EDGE reaches the footer here and nowhere else: no
		// stream frame states that a turn was accepted.
		w.sinks.Footer.OnTurnOpened(ws, turn)
		// AND THE FEED, for the same reason: the turn's own terminal row for a
		// query that died out from under it is drawn against the turn the feed
		// believes is running, and nothing on the streams states it either.
		w.sinks.Feed.OnTurnOpened(ws, turn)
	} else {
		w.log.Error("daemon.sessionwatcher.turn_opened_unidentified", "an opened turn named no id", dlog.Context{
			"agent_id": prompt.GetAgent().GetValue(),
		})
	}
	w.log.Info("daemon.sessionwatcher.turn_opened", "the queue opened a turn", dlog.Context{
		"turn_id":  prompt.GetId().GetValue(),
		"agent_id": prompt.GetAgent().GetValue(),
		"entries":  len(page.GetEntries()),
	})
	if page != nil {
		w.routeOpeningPageLocked(w.main, page, pageTurnAccepted)
	}
	// THE HAND-OVER IS COMPLETE, so a terminal held for this turn's naming is
	// released HERE and not a line earlier: the views have just been given the
	// turn's OPEN edge, and the replay now reaches them in the order they
	// would have seen had the answer beaten the stream. Released at the
	// naming instead, the footer took a terminal for a turn it had never seen
	// start and never came back to idle.
	w.releaseHeldTerminalLocked()
}

// SessionEnding records that the daemon itself is ending this session, so the
// standing streams the shim closes on its way out read as the session's end
// rather than as transport faults. It is idempotent, and it does NOT close
// anything: the shim still writes its own terminals as the session ends, and
// the watcher must stay open to receive them.
func (w *watcher) SessionEnding(reason string) {
	w.mu.Lock()
	defer w.mu.Unlock()
	if w.sessionEnded {
		return
	}
	before := w.sessionEnded
	w.sessionEnded = true
	w.log.Debug("daemon.sessionwatcher.state_transition", "the session-ending latch changed", dlog.Context{
		"state": "session_ended", "before": before, "after": true,
	})
	w.log.Info("daemon.sessionwatcher.session_ending",
		"the daemon is ending the session; its standing streams end with it",
		dlog.Context{"reason": reason})
}

// Close tears down every watch this workspace owns. It NEVER kills anything:
// attaching created nothing, so detaching ends nothing.
func (w *watcher) Close() error {
	w.mu.Lock()
	if w.closed {
		w.mu.Unlock()
		return nil
	}
	beforeClosed := w.closed
	beforeGeneration := w.gen
	w.closed = true
	w.gen++
	w.log.Debug("daemon.sessionwatcher.state_transition", "the watch fleet entered its closed generation", dlog.Context{
		"state": "closed", "before": beforeClosed, "after": true,
		"generation_before": beforeGeneration, "generation_after": w.gen,
	})
	w.failWaitersLocked()
	closing := w.takeStreamsLocked()
	w.log.Info("daemon.sessionwatcher.close", "closing the session's watch fleet", dlog.Context{
		"streams": len(closing),
	})
	// A CLOSE IS A DEPARTURE ONLY WHEN THE SHIM IS GOING. A session this
	// daemon is ending (a stop, a kill, a stand-down) or one whose process is
	// already reaped is gone with its work; a close that merely stops
	// WATCHING a shim that keeps running -- the daemon's own exit, leaving its
	// shims for the next daemon to adopt -- resolves nothing, and telling the
	// bounce registry otherwise would take a bounce over live work.
	var departed *Departure
	_, reaped := w.client.Reaped()
	if w.endingLocked() || reaped {
		departed = w.departLocked(DepartureClosed)
	}
	w.mu.Unlock()
	w.tellDeparted(departed)

	w.cancel()
	closeStreams(closing)
	// EVERY OPEN STILL IN FLIGHT IS JOINED: the cancel above is its context's
	// end, so a hung open returns now, finds the fleet closed and discards
	// what it got. Nothing this watcher started touches the client after
	// Close returns.
	w.opens.Wait()
	// The off-lock sink dispatch is JOINED here: a turn end still being handled
	// reads the state client, and the daemon closes that client once every
	// watcher is closed.
	w.dispatching.Wait()
	// The watchdog stops watching only now: every hold Close itself took is
	// over, and the next tick settles a stall this watcher was reported for.
	w.unwatch()
	return nil
}

// endCutTurnLocked ends the turn a shim that DIED ON ITS OWN was running. The
// turn ran inside the shim's vendor query, which is gone with it, and no
// terminal will ever arrive for it: left standing, its prompt row spun as
// working forever and its durable row stayed open until the next boot closed
// it as an orphan. The turn's end reaches the queue as the agent process
// dying (wsm.CloseAgentDied), and the queue's door closes its row and draws
// that ending in the feed; the feed is only told the turn, so the door has a
// turn to end. The footer and the roster are told nothing here: they draw the
// link, and the bring-up that follows is what they show.
func (w *watcher) endCutTurnLocked() {
	// EVERY TURN THE SHIM HELD ENDS WITH IT: the one in flight, then each one
	// waiting behind it as it stands in flight in turn.
	for w.turn != nil {
		turn := *w.turn
		w.log.Info("daemon.sessionwatcher.turn_cut", "the shim died with a turn in flight; the turn ended with it", dlog.Context{
			"turn_id": string(turn),
		})
		w.flushHeldTerminalLocked()
		w.sinks.Feed.OnTurnOpened(w.ws, turn)
		w.turnEndedLocked(turn, wsm.CloseAgentDied)
	}
}

// Departed implements Watcher.
func (w *watcher) Departed() (Departure, bool) {
	w.mu.Lock()
	defer w.mu.Unlock()
	if w.departure == nil {
		return Departure{}, false
	}
	return *w.departure, true
}

// departLocked records the shim's departure, answering it the FIRST time only
// (nil after), so the sink is told once per watcher whichever edge came first.
// The caller holds mu, and tells the sink once it is released.
func (w *watcher) departLocked(cause DepartureCause) *Departure {
	if w.departure != nil {
		return nil
	}
	d := Departure{Ordered: w.endingLocked(), Cause: cause}
	w.departure = &d
	w.log.Debug("daemon.sessionwatcher.state_transition", "the watched shim departed", dlog.Context{
		"state": "departed", "before": false, "after": true,
	})
	w.log.Info("daemon.sessionwatcher.departed", "the watched shim is gone; its in-flight work ended with it", dlog.Context{
		"cause": string(cause), "ordered": d.Ordered,
	})
	return &d
}

// tellDeparted hands a just-recorded departure to the lifecycle sink, OFF mu:
// the sink's bounce registry reads this watcher under its own lock.
func (w *watcher) tellDeparted(d *Departure) {
	if d == nil {
		return
	}
	w.sinks.Lifecycle.OnDeparted(w.ws, w, *d)
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

// liveWorkLocked reports the live set, read from the ledger and from nothing
// else: a watch's state says nothing about liveness. Sorted so the published
// value is stable frame to frame.
func (w *watcher) liveWorkLocked() LiveWorkSet {
	var live LiveWorkSet
	for _, item := range w.live {
		switch item.kind {
		case kindSubagent:
			live.Agents = append(live.Agents, item.agentOrHandle())
		case kindBash:
			live.Shells = append(live.Shells, item.work)
		case kindMonitor:
			live.Monitors = append(live.Monitors, item.work)
		}
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
	// AND THE FOOTER HEARS THE SAME SET, for the same reason and out of the
	// same value: the strip's `background` arm and the roster's `idle_async`
	// arm are two renderings of ONE fact, so they cannot be allowed to resolve
	// from two ledgers.
	w.sinks.Footer.OnLiveWorkChanged(w.ws, live)
	// AND THE FEED: a detached shell's bubble settles when its run leaves the
	// set, whichever of that, its own terminal or its call's result comes
	// first, so a run nobody can see any more never draws as running.
	w.sinks.Feed.OnLiveWorkChanged(w.ws, live)
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
		// A CONNECTED EDGE THAT ANNOUNCES NO NEW CONNECTION CANNOT MEND A
		// BROKEN FLEET. The feed replays the bring-up's own `connected` to a
		// watcher born on that link, and nothing orders that replay against
		// a standing stream's end, which arrives on the stream's goroutine.
		// When the end is heard first the fleet is degraded, and taking the
		// replay for a reconnect re-opened every watch on a link the client
		// had not re-established -- and painted it connected while its
		// streams were down, which invariant 11 forbids. MEASURED, the unit
		// suite 2026-09-28: TestAReplayedBringUpConnectedIsNotAReconnect
		// failed 6 runs in 40. The client's connection count decides it, as
		// it does in severedLocked: a reconnect has advanced it past the one
		// the broken fleet was opened on, before its edge was published.
		if state == shimclient.LinkConnected && w.degraded {
			if now := w.client.Connections(); now <= w.fleetConnections {
				w.log.Debug("daemon.sessionwatcher.link_not_a_reconnect", "a connected edge named no connection newer than the broken fleet's; the fleet stays down until the link is re-established", dlog.Context{
					"held": int(w.link), "fleet_connections": w.fleetConnections, "client_connections": now,
				})
				w.mu.Unlock()
				continue
			}
		}
		previous := w.link
		degraded := w.degraded
		w.setLinkLocked(state)
		if state == shimclient.LinkConnected && previous != shimclient.LinkConnected && degraded {
			w.reopenLocked("the link came back")
		}
		var departed *Departure
		if w.link == shimclient.LinkDead {
			departed = w.departLocked(DepartureLinkDead)
			if departed != nil && !departed.Ordered {
				w.endCutTurnLocked()
			}
		}
		w.mu.Unlock()
		// The cut turn's end reaches the queue BEFORE the departure does, so
		// its row is closed before anything brings the session back.
		w.flushTurnEnds()
		w.tellDeparted(departed)
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
	// A FIRST-TIME DIAL CANNOT FOLLOW A CONNECTION. `LinkDialing` is the
	// client's bring-up establishing its link for the first time, and this
	// watcher exists only once that bring-up has SUCCEEDED -- it is born
	// connected. The client's connectivity feed delivers every transition it
	// published in order, so the first states a watcher reads are the
	// bring-up's own `dialing` and `connected`, already outrun by the moment
	// it was created. Applying that `dialing` walked the published link back
	// to `init` for a workspace whose turn was already accepted -- MEASURED
	// in a headless run's cold start, where the roster's arm went `submitting`
	// -> `init` -> `submitting` within 1ms of the first StartTurn -- and a
	// lost link is never spelled `dialing`: that is `redialing` or `dead`.
	if state == shimclient.LinkDialing {
		w.log.Debug("daemon.sessionwatcher.link_replay", "a first-dial transition is the bring-up's own replay; the watcher was born on the connected link", dlog.Context{
			"held": int(w.link),
		})
		return
	}
	w.log.Info("daemon.sessionwatcher.link", "link state changed", dlog.Context{
		"previous": int(w.link), "link": int(state),
	})
	w.log.Debug("daemon.sessionwatcher.state_transition", "the link state changed", dlog.Context{
		"state": "link", "before": int(w.link), "after": int(state),
	})
	w.link = state
	w.linkNow.Store(int32(state))
	// THE EVIDENCE IS RECORDED BEFORE THE VIEWS DRAW THE LOSS. A client that
	// sees `dead` in the footer and asks SessionHealth in the same breath must
	// find the fault already standing; recorded after the publish, the answer
	// would depend on which of the two won a race.
	w.raiseLinkFaultLocked(state)
	w.publishLinkLocked()
}

// raiseLinkFaultLocked reports a LOST link at the lifecycle sink, which records
// it as the session's own fault. A link coming back is not a fault, so only the
// two losing transitions are raised.
//
// A LINK THIS DAEMON TOOK DOWN IS NOT LOST. When the session is deliberately
// ending -- announced through `SessionEnding`, or latched on the shim client by
// the `KillSession` the daemon asked for -- the link going redialing and then
// dead is the teardown arriving, and a fault raised against it would leave a
// standing `link_severed` on the health surface for a workspace the user just
// closed. The transition is still PUBLISHED to the footer, topbar and sidebar
// by the caller, so the views draw the loss exactly as before; only the fault
// and its warning are withheld, and only for a teardown the daemon ordered.
func (w *watcher) raiseLinkFaultLocked(state LinkState) {
	if w.endingLocked() && (state == shimclient.LinkRedialing || state == shimclient.LinkDead) {
		w.log.Debug("daemon.sessionwatcher.link_fault",
			"the link went down inside a teardown this daemon ordered; no fault is raised",
			dlog.Context{"link": int(state), "stand_down_asked": w.client.StandingDown()})
		return
	}
	switch state {
	case shimclient.LinkRedialing:
		w.log.Warn("daemon.sessionwatcher.link_fault", "the shim link was severed", nil)
		w.sinks.Lifecycle.OnLinkFault(w.ws, LinkFault{
			Kind:   LinkFaultSevered,
			Detail: "a standing stream ended while the shim was still running",
		})
	case shimclient.LinkDead:
		fault := LinkFault{Kind: LinkFaultDead, Detail: "the shim process is gone"}
		if info, ok := w.client.Reaped(); ok {
			code := int32(info.Code)
			fault.ExitCode = &code
			fault.Detail = "the shim process exited"
		}
		w.log.Warn("daemon.sessionwatcher.link_fault", "the shim process is gone", dlog.Context{
			"has_exit_code": fault.ExitCode != nil,
		})
		w.sinks.Lifecycle.OnLinkFault(w.ws, fault)
	}
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
//
// IT STATES WHAT THE DAEMON KNOWS ABOUT THE PEER, because the severing alone
// does not say which of two very different things happened. A shim that DIED
// takes every stream with it and is remediated by bringing a session back; a
// shim that is ALIVE and ended one stream of several is a producer that
// dropped a watch its consumer still needs, and is remediated in the shim.
//
// MEASURED, realtest run 2026-09-13T16:03:25. Adopted shim pid 3031 ended
// workspace 2b81f45a724642ef's AGENT stream with EOF; its session stream
// stayed open (the teardown eleven seconds later closed two streams for that
// workspace), the process was alive enough to be killed by that teardown, and
// the shim's own log recorded nothing at all. The daemon's three records --
// this one, the link fault and the health fault -- named none of it, so the
// event could not be told from a shim that had simply gone.
// `extra` names the STREAM that ended, when the caller knows which one: a
// severing whose record does not say which of the fleet's watches went is a
// record nobody can act on.
func (w *watcher) severedLocked(operation, detail string, err error, extra ...dlog.Context) {
	ctx := dlog.Context{
		"detail":           detail,
		"shim_pid":         w.client.PID(),
		"stand_down_asked": w.client.StandingDown(),
	}
	if info, reaped := w.client.Reaped(); reaped {
		ctx["shim_reaped"] = true
		ctx["shim_exit_code"] = info.Code
		ctx["shim_exit_signal"] = info.Signal
	} else {
		ctx["shim_reaped"] = false
	}
	if err != nil {
		ctx["error"] = err.Error()
	}
	for _, more := range extra {
		for k, v := range more {
			ctx[k] = v
		}
	}
	w.log.Error("daemon.sessionwatcher."+operation, "a standing stream ended without the session ending", ctx)
	// THE LINK MAY ALREADY BE BACK. A transport break ends the client's own
	// liveness stream and this fleet's streams at once, but the client's
	// redial reaches runLink on the connectivity feed while this ending
	// arrives on the stream's own goroutine, and nothing orders the two. When
	// the redial's LinkConnected was consumed first, the fleet was not yet
	// degraded, so it re-opened nothing; walking the link to redialing now
	// would wait on a LinkConnected that never comes again. MEASURED, the
	// integration suite 2026-09-28 (1 run in 24 under load, ~1 in 50 idle):
	// the session watch stayed down for good, its re-announcement never came,
	// and the registered bounce waited forever behind a shell the shim had
	// already concluded.
	//
	// THE CLIENT'S OWN CONNECTION COUNT DECIDES IT, not the edges this watcher
	// consumed. The count is advanced before LinkConnected is published, so it
	// is never behind a reconnect whatever order the goroutines ran in, and
	// the feed's replay of bring-up edges to a new watcher never moves it.
	if now := w.client.Connections(); now > w.fleetConnections {
		w.log.Info("daemon.sessionwatcher.reopen_after_reconnect", "the link came back before this stream's end was seen; re-opening now", dlog.Context{
			"stream_operation": operation, "fleet_connections": w.fleetConnections, "client_connections": now,
		})
		w.reopenLocked("a stream ended after the link had already come back")
		return
	}
	w.degraded = true
	w.setLinkLocked(shimclient.LinkRedialing)
}

// reopenLocked tears the fleet down and opens it again, each watch catching up
// from the newest pointer it was served. The generation bump is what makes the
// old goroutines' errors stale rather than a second severing.
func (w *watcher) reopenLocked(reason string) {
	beforeGeneration := w.gen
	beforeDegraded := w.degraded
	w.gen++
	// The fleet is whole again from here: any open below that fails calls
	// severedLocked, which sets the flag afresh.
	w.degraded = false
	w.log.Debug("daemon.sessionwatcher.state_transition", "the watch fleet entered a fresh generation", dlog.Context{
		"state": "watch_generation", "before": beforeGeneration, "after": w.gen,
		"degraded_before": beforeDegraded, "degraded_after": false,
	})
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

	w.fleetConnections = w.client.Connections()
	w.openSessionLocked()
	w.openMainAfterFactsLocked()
	for _, a := range w.agents {
		w.openAgentStreamLocked(a)
	}
	for _, s := range w.shells {
		w.openShellStreamLocked(s)
	}
}

// ---- opening watches ----

// ---- the off-lock open ----

// openTicket is ONE watch open in flight. The open is DECIDED under mu, which
// mints the ticket and records it on the watch it is for; it is MADE on a
// goroutine of its own, off mu, under the ticket's context; and its result is
// INSTALLED under mu again only when the ticket is still the one the watch
// records, the watch was not reaped meanwhile and the fleet's generation has
// not moved. Anything else is a STALE open, and its stream is closed rather
// than installed.
//
// The ticket's context is the watcher's own, so Close cancels every open still
// in flight, and a ticket superseded or reaped while its open hangs is
// cancelled at once. A successful open's context lives exactly as long as its
// stream: the installed stream is wrapped so its Close ends the context too.
type openTicket struct {
	gen    uint64
	ctx    context.Context
	cancel context.CancelFunc
	// liveSeq is the ledger's admission count when the open was DECIDED. A
	// session watch's re-announcement is computed by the shim after that
	// instant, so it can only speak for items admitted by then.
	liveSeq uint64
}

// decideOpenLocked mints the ticket for one open of the watch whose open in
// flight is `current`, or answers false when no open is to be made: the fleet
// is closed (it opens nothing more), or an open of this generation is already
// in flight (a second would duplicate it). A ticket it supersedes -- an open
// of an older generation no completion will ever install -- is cancelled, so
// a hung one is released at once. Every minted open is counted in w.opens,
// which Close joins.
func (w *watcher) decideOpenLocked(stream, key string, current *openTicket) (*openTicket, bool) {
	if w.closed {
		w.log.Debug("daemon.sessionwatcher.open_skipped", "a closed fleet opens no watch", dlog.Context{
			"stream": stream, "key": key,
		})
		return nil, false
	}
	// A SESSION THIS DAEMON IS ENDING OPENS NO WATCH. The shim was asked to
	// stand down (or the session is over), so it is closing every stream it
	// serves: a watch opened now races the shim's own exit and can only come
	// back EOF. MEASURED, deploy 2026-09-29T17:15:29: the successor's bounce
	// stood six stale shims down, and a WatchAgent open decided after each
	// stand-down was refused with "incomplete envelope: unexpected EOF" and
	// recorded as two ERRORs apiece.
	if w.endingLocked() {
		w.log.Debug("daemon.sessionwatcher.open_skipped", "a session this daemon is ending opens no watch", dlog.Context{
			"stream": stream, "key": key, "stand_down_asked": w.client.StandingDown(),
		})
		return nil, false
	}
	if w.inFlightLocked(current) {
		w.log.Debug("daemon.sessionwatcher.open_skipped", "an open of this watch is already in flight", dlog.Context{
			"stream": stream, "key": key,
		})
		return nil, false
	}
	current.abandon()
	ctx, cancel := context.WithCancel(w.ctx)
	w.opens.Add(1)
	return &openTicket{gen: w.gen, ctx: ctx, cancel: cancel, liveSeq: w.liveSeq}, true
}

// abandon cancels an open nobody will install. It is nil-safe, because a
// watch with no open in flight records none.
func (t *openTicket) abandon() {
	if t != nil {
		t.cancel()
	}
}

// inFlightLocked reports whether t is an open of the CURRENT generation still
// in flight, which a second open of the same watch must not duplicate.
func (w *watcher) inFlightLocked(t *openTicket) bool {
	return t != nil && !w.closed && t.gen == w.gen
}

// ticketedStream is an installed stream whose Close also ends the context its
// open ran under, so a stream's open context never outlives the stream.
type ticketedStream[T any] struct {
	shimclient.Stream[T]
	cancel context.CancelFunc
}

// Close ends the stream, then its open's context.
func (s *ticketedStream[T]) Close() {
	s.Stream.Close()
	s.cancel()
}

// discardOpenLocked records an open whose completion the fleet no longer
// wants. A refusal or a transport failure on it is STILL RECORDED, with the
// error, rather than dropped: it is no longer anyone's fault to act on, but it
// is a fact about the shim. The caller closes the stream (discardStream) once
// mu is released.
func (w *watcher) discardOpenLocked(stream, key string, t *openTicket, err error) {
	ctx := dlog.Context{
		"stream": stream, "key": key,
		"open_generation": t.gen, "generation": w.gen, "closed": w.closed,
	}
	if err != nil {
		ctx["error"] = err.Error()
		w.log.Info("daemon.sessionwatcher.open_discarded",
			"a watch open the fleet no longer wants failed; its failure is not acted on", ctx)
		return
	}
	w.log.Debug("daemon.sessionwatcher.open_discarded",
		"a watch open the fleet no longer wants completed; its stream is closed, not installed", ctx)
}

// discardStream closes a stale open's stream and ends its context. It runs OFF
// mu: a real stream's Close drains its response body until the server ends it.
func discardStream[T any](t *openTicket, stream shimclient.Stream[T]) {
	t.cancel()
	if stream != nil {
		stream.Close()
	}
}

// openSessionLocked DECIDES the WatchSession open, the session's standing
// stream; openSession makes it off mu.
func (w *watcher) openSessionLocked() {
	t, ok := w.decideOpenLocked("session", "", w.sessionOpening)
	if !ok {
		return
	}
	w.sessionOpening = t
	go w.openSession(t)
}

// openSession makes one decided WatchSession open OFF mu and installs it.
func (w *watcher) openSession(t *openTicket) {
	defer w.opens.Done()
	stream, err := w.client.WatchSession(t.ctx)
	w.mu.Lock()
	if w.sessionOpening != t || w.stale(t.gen) {
		w.discardOpenLocked("session", "", t, err)
		w.mu.Unlock()
		discardStream(t, stream)
		return
	}
	w.sessionOpening = nil
	if err != nil {
		if !w.openEndedByTeardownLocked("session", "", err) {
			w.severedLocked("watch_session", "WatchSession could not be opened", err)
		}
		w.mu.Unlock()
		t.cancel()
		w.flushTurnEnds()
		return
	}
	installed := &ticketedStream[*shimv1.WatchSessionResponse]{Stream: stream, cancel: t.cancel}
	w.sessionStream = installed
	w.log.Debug("daemon.sessionwatcher.watch_session", "session watch opened", nil)
	go w.runSession(t.gen, t.liveSeq, installed)
	w.mu.Unlock()
	w.flushTurnEnds()
}

// openMainAfterFactsLocked opens the main agent's watch ONLY ONCE THE SESSION
// HAS ANNOUNCED ITSELF.
//
// THERE IS NO MAIN AGENT UNTIL THERE IS A SESSION. The shim resolves an unset
// WatchAgent target through the session's identity, so a shim that has not had
// StartSession run on it answers `not_found: no session has been started on
// this shim` — a refusal that is not a race and cannot be waited out, because
// nothing about the passage of time starts a session.
//
// A PURE ATTACH IS EXACTLY THAT CASE. An adopting daemon (crash boot,
// handover) opens with NO facts and learns them from the shim's own
// SessionStarted re-announcement, which rides every new WatchSession right
// after the opening diagnostics (landing 7). Opening the agent watch before
// that frame arrives asked a sessionless survivor for an agent it does not
// have and collected a refusal per attempt; the daemon simply must not ask
// yet. reannouncedLocked opens it the instant the facts land, and a shim that
// never announces one never had an agent to watch.
func (w *watcher) openMainAfterFactsLocked() {
	if !w.started {
		w.log.Debug("daemon.sessionwatcher.watch_agent",
			"no session has announced itself yet; the main agent's watch waits for the re-announcement", nil)
		return
	}
	w.openMainLocked()
}

// openMainLocked opens the main agent's watch: an UNSET target, which the shim
// resolves to the session's prompt thread.
func (w *watcher) openMainLocked() {
	if w.main == nil {
		w.main = &agentWatch{}
	}
	w.openAgentStreamLocked(w.main)
}

// openAgentStreamLocked DECIDES the open (or re-open) of one agent watch,
// catching up from the newest pointer that watch was served; openAgent makes
// it off mu. A watch whose open of this generation is already in flight is
// left to it.
func (w *watcher) openAgentStreamLocked(a *agentWatch) {
	t, ok := w.decideOpenLocked("agent", watchKey(a.id), a.opening)
	if !ok {
		return
	}
	req := &shimv1.WatchAgentRequest{Target: a.id, PageSize: openingPageSize}
	if ptr := w.known[watchKey(a.id)]; ptr != nil {
		req.KnownThrough = ptr
	}
	// WHAT THE OPEN ASKS FOR IS DECIDED WITH IT: the page it will be served
	// answers this request, whatever is routed while it is in flight.
	catchUp := req.KnownThrough != nil || a.paged
	a.opening = t
	go w.openAgent(t, a, req, catchUp)
}

// openAgent makes one decided WatchAgent open OFF mu and installs it.
func (w *watcher) openAgent(t *openTicket, a *agentWatch, req *shimv1.WatchAgentRequest, catchUp bool) {
	defer w.opens.Done()
	stream, err := w.client.WatchAgent(t.ctx, req)
	w.mu.Lock()
	if a.opening != t || a.done || w.stale(t.gen) {
		w.discardOpenLocked("agent", watchKey(a.id), t, err)
		w.mu.Unlock()
		discardStream(t, stream)
		return
	}
	a.opening = nil
	if err != nil {
		a.stream = nil
		switch {
		case w.openEndedByTeardownLocked("agent", watchKey(a.id), err):
		case refusedOpen(err):
			w.openRefusedLocked("watch_agent", watchKey(a.id), w.agentExpectedLocked(a), &a.refusals, err)
		default:
			w.severedLocked("watch_agent", "WatchAgent could not be opened", err)
		}
		w.mu.Unlock()
		t.cancel()
		w.flushTurnEnds()
		return
	}
	installed := &ticketedStream[*shimv1.WatchAgentResponse]{Stream: stream, cancel: t.cancel}
	a.refusals = 0
	a.stream = installed
	a.catchUp = catchUp
	w.log.Debug("daemon.sessionwatcher.watch_agent", "agent watch opened", dlog.Context{
		"agent_id": a.id.GetValue(), "catch_up": a.catchUp,
	})
	go w.runAgent(t.gen, a, installed)
	w.mu.Unlock()
	w.flushTurnEnds()
}

// openShellStreamLocked DECIDES the open (or re-open) of one detached shell's
// watch; openShell makes it off mu. A failed open is surfaced (a refusal, or a
// severed link) when it completes. A shell whose open of this generation is
// already in flight is left to it.
//
// A FAILED OPEN NEVER TOUCHES LIVENESS. The shell stays in the ledger, and its
// entry stays registered with no stream, so its next announcement or the next
// fleet re-open is what retries it. Forgetting the entry on a failure made a
// watch that could not be opened into a statement that the shell had ENDED,
// which only the shim may make (livework.go).
func (w *watcher) openShellStreamLocked(s *shellWatch) {
	t, ok := w.decideOpenLocked("shell", s.work.GetValue(), s.opening)
	if !ok {
		return
	}
	s.opening = t
	go w.openShell(t, s)
}

// openShell makes one decided WatchBash open OFF mu and installs it.
func (w *watcher) openShell(t *openTicket, s *shellWatch) {
	defer w.opens.Done()
	stream, err := w.client.WatchBash(t.ctx, s.work)
	w.mu.Lock()
	key := s.work.GetValue()
	if s.opening != t || s.done || w.stale(t.gen) {
		w.discardOpenLocked("shell", key, t, err)
		w.mu.Unlock()
		discardStream(t, stream)
		return
	}
	s.opening = nil
	if err != nil {
		s.stream = nil
		switch {
		case w.openEndedByTeardownLocked("shell", key, err):
		case refusedOpen(err):
			w.openRefusedLocked("watch_bash", key, w.shells[key] == s, &s.refusals, err)
		default:
			w.severedLocked("watch_bash", "WatchBash could not be opened", err)
		}
		w.mu.Unlock()
		t.cancel()
		w.flushTurnEnds()
		return
	}
	installed := &ticketedStream[*conversationv1.AgentBash]{Stream: stream, cancel: t.cancel}
	s.refusals = 0
	s.stream = installed
	w.log.Debug("daemon.sessionwatcher.watch_bash", "shell watch opened", dlog.Context{
		"work_id": key,
	})
	go w.runShell(t.gen, s, installed)
	w.mu.Unlock()
	w.flushTurnEnds()
}

// refusedOpen reports whether err is a watch OPEN the shim REFUSED, as opposed
// to a transport that failed. Only the open call can carry it -- a stream that
// dies on Recv is a StreamOpenError for nothing -- and only two codes mean it:
// not_found (the store holds no such book or handle YET) and
// failed_precondition (the shim has no such agent YET). Both are SEMANTIC
// answers from a shim that is serving, and neither says anything about the
// link.
func refusedOpen(err error) bool {
	var refusal *shimclient.StreamOpenError
	if !errors.As(err, &refusal) {
		return false
	}
	switch connect.CodeOf(refusal.Err) {
	case connect.CodeNotFound, connect.CodeFailedPrecondition:
		return true
	}
	return false
}

// agentExpectedLocked reports whether the daemon legitimately expects the
// handle this agent watch addresses: the MAIN agent's watch (which the session
// always has), or an entry that is the REGISTERED watch for its agent -- which
// it is exactly because an announcement or a spawn put it there. Anything else
// is a handle nothing announced.
func (w *watcher) agentExpectedLocked(a *agentWatch) bool {
	if a == w.main {
		return true
	}
	return a.id != nil && w.agents[a.id.GetValue()] == a
}

// openRefusedLocked records a REFUSED open. It never severs the link and never
// raises a link fault: the shim answered, so the hop is serving.
//
// An EXPECTED handle's refusal is the ordinary fresh-bring-up race -- the book
// or the shell's rows are not registered yet -- so it is logged at INFO and
// left for the re-open path (a session frame for the main watch, a repeated
// announcement for detached work, the fleet re-open for everything) to open
// again. A handle NOTHING announced, or an expected one whose refusals ran out
// of retries, is a disagreement about what exists and is reported as its own
// fault at WARN.
func (w *watcher) openRefusedLocked(operation, handle string, expected bool, refusals *int, err error) {
	*refusals++
	ctx := dlog.Context{"handle": handle, "refusals": *refusals, "expected": expected}
	if err != nil {
		ctx["error"] = err.Error()
	}
	if expected && *refusals <= openRefusalLimit {
		w.log.Info("daemon.sessionwatcher."+operation,
			"the shim refused a watch open for a handle it does not hold yet; it will be re-opened", ctx)
		return
	}
	w.log.Warn("daemon.sessionwatcher.watch_open_refused",
		"the shim refused a watch open for a handle it will not serve", ctx)
	w.sinks.Lifecycle.OnWatchOpenRefused(w.ws, WatchOpenRefusal{
		Operation: operation,
		Handle:    handle,
		Detail:    shimclient.Detail(err),
	})
}

// retryRefusedMainLocked re-opens the MAIN agent's watch after a REFUSED open.
// Nothing re-announces the main agent, so the session's own stream is the
// occasion: a frame on it proves the shim is serving this session, and by then
// the book the refusal was about is the one the shim is writing.
func (w *watcher) retryRefusedMainLocked() {
	if w.main == nil || w.main.stream != nil || w.main.refusals == 0 || w.inFlightLocked(w.main.opening) {
		return
	}
	if w.main.refusals > openRefusalLimit {
		return
	}
	w.log.Info("daemon.sessionwatcher.watch_agent",
		"a session frame re-opened the main agent's refused watch", dlog.Context{
			"refusals": w.main.refusals,
		})
	w.openMainLocked()
}

// watchKey is a watch's key in the known_through map.
func watchKey(id *conversationv1.AgentId) string {
	if id == nil {
		return mainWatchKey
	}
	return id.GetValue()
}

// ---- consuming streams ----

// runSession consumes the session's standing stream. openedAt is the ledger's
// admission count when this watch's open was decided: the most its
// re-announcement can speak for.
func (w *watcher) runSession(gen, openedAt uint64, stream shimclient.Stream[*shimv1.WatchSessionResponse]) {
	for {
		frame, err := stream.Recv()
		if err != nil {
			w.streamEnded(gen, "session", "watch_session", "", nil, err)
			return
		}
		w.mu.Lock()
		if w.stale(gen) {
			w.mu.Unlock()
			return
		}
		w.retryRefusedMainLocked()
		switch {
		case frame.GetUpdate() != nil:
			w.log.Debug("daemon.sessionwatcher.transition_decision", "selected a watcher transition branch", dlog.Context{"function": "watcher", "branch": "case frame.GetUpdate() != nil"})
			w.routeSessionUpdateLocked(frame.GetUpdate())
		case frame.GetSessionStarted() != nil:
			w.log.Debug("daemon.sessionwatcher.transition_decision", "selected a watcher transition branch", dlog.Context{"function": "watcher", "branch": "case frame.GetSessionStarted() != nil"})
			w.reannouncedLocked(frame.GetSessionStarted(), openedAt)
		default:
			// The shim client validates the oneof before a frame ever reaches
			// here, so an unset arm at this seam is an invariant violation and
			// is recorded rather than dropped.
			w.log.Error("daemon.sessionwatcher.watch_session",
				"a session frame carries no arm", nil)
		}
		w.mu.Unlock()
		w.flushTurnEnds()
	}
}

// reannouncedLocked takes the shim's ONCE-PER-WATCH re-announcement of the
// session's own SessionStarted (landing 7).
//
// A watcher that ALREADY HOLDS the facts does not take them again: the
// re-announcement rides every new watch, including each re-open after a link
// break, and re-publishing the same facts would be churn on every subscriber
// for no new information. It is the ORDINARY case, so it is not warned.
//
// ITS LIVE MEMBERSHIP IS ALWAYS RECONCILED, though. The re-announcement's
// live_work is the shim's statement of what is live NOW, and it is the one
// statement that reaches this daemon after a conclusion it missed -- a terminal
// that fell in a link gap, or one no stream this daemon watches ever carried.
// An item the ledger holds that the shim no longer names has ended, and is
// retired (reconcileLiveWorkLocked).
func (w *watcher) reannouncedLocked(started *conversationv1.SessionStarted, openedAt uint64) {
	if w.started {
		w.log.Debug("daemon.sessionwatcher.watch_session",
			"ignored a re-announced SessionStarted's facts; they are already held, and only its live membership is reconciled",
			dlog.Context{"vendor_session_id": started.GetVendorSessionId()})
		if w.reconcileLiveWorkLocked(started, openedAt) {
			w.publishLiveWorkLocked()
		}
		return
	}
	w.log.Info("daemon.sessionwatcher.watch_session",
		"took the session facts from the shim's re-announcement", dlog.Context{
			"vendor_session_id": started.GetVendorSessionId(),
			"turn_in_flight":    started.GetTurnInFlight().GetValue(),
			"turns_waiting":     len(started.GetTurnsWaiting()),
			"live_work":         len(started.GetLiveWork()),
		})
	w.applySessionStartedLocked(started)
	// A PURE ATTACH'S TURN IN FLIGHT was started by no StartTurn of this
	// watcher's, so none will ever name the main agent for it.
	if t := started.GetTurnInFlight(); t != nil && w.turn != nil && *w.turn == ids.TurnID(t.GetValue()) {
		w.factsTurn = *w.turn
	}
	// THE FACTS ARE THE OCCASION FOR THE AGENT WATCH. A pure attach deferred
	// it precisely until now: the shim has just named the session, so the main
	// agent it resolves an unset target to exists.
	if w.main == nil || w.main.stream == nil {
		w.openMainLocked()
	}
	w.reconcileLiveWorkLocked(started, openedAt)
	w.adoptLiveWorkLocked(started)
	w.publishLiveWorkLocked()
}

// applySessionStartedLocked is the ONE place the session facts are taken up,
// whichever way they arrived: StartSession's own answer, or the shim's
// re-announcement on an adopted watch.
func (w *watcher) applySessionStartedLocked(started *conversationv1.SessionStarted) {
	first := !w.started
	w.started = true
	// THE EDGE IS SIGNALLED AFTER THE FACTS ARE TAKEN UP, under the same lock
	// (the deferred close runs before this function's caller releases it), so
	// a waiter released by it reads the turn in flight and the reconciled
	// open-at-attach turns, never the moment before them.
	if first {
		defer close(w.factsIn)
	}
	w.sinks.Topbar.OnSessionStarted(w.ws, started)
	w.sinks.Footer.OnSessionStarted(w.ws, started)
	w.sinks.Sidebar.OnSessionStarted(w.ws, started)
	// A RESUMED OR ADOPTED SESSION may already carry prompts with no vendor
	// title, so the synthesizer is triggered at the session's naming — not only
	// at turn ends, which a resumed conversation would not produce on its own.
	if w.sinks.Title != nil {
		w.sinks.Title.OnSessionStarted(w.ws)
	}
	if t := started.GetTurnInFlight(); t != nil {
		ahead := ids.TurnID(t.GetValue())
		ownTurn := w.turn != nil && *w.turn == ahead
		if w.standTurnLocked(ahead, "daemon.sessionwatcher.state_transition", "the session's facts name the turn already in flight") {
			if !ownTurn {
				w.standRunningAtAttachLocked(ahead)
			}
			w.restoreWaitingLocked(ahead, started.GetTurnsWaiting())
		}
	} else if len(started.GetTurnsWaiting()) > 0 {
		w.log.Error("daemon.sessionwatcher.turns_waiting_unanchored", "the session's facts name turns waiting behind no turn in flight; none is stood behind anything", dlog.Context{
			"turns_waiting": len(started.GetTurnsWaiting()),
		})
	}
	w.reconcileOpenAtAttachLocked(started)
}

// standRunningAtAttachLocked tells the views that TURN, which the adopted
// shim names as its turn in flight and no delivery of this watcher's opened,
// is running. Without it a daemon that took a busy workspace over drew it
// `ready` until the turn ended (owner report, 2026-09-29: the workspace the
// agent was working in read done after a deploy's handover).
//
// UNDER THE LOCK, BEFORE THE MAIN AGENT'S WATCH OPENS (the caller's caller
// opens it after the facts): the running turn's frames then land on a turn
// that already stands, and nothing standing it afterwards can wipe what they
// said.
func (w *watcher) standRunningAtAttachLocked(turn ids.TurnID) {
	var startedAt *time.Time
	for _, open := range w.openAtAttach {
		if open.ID == turn {
			at := open.StartedAt
			startedAt = &at
			break
		}
	}
	w.log.Info("daemon.sessionwatcher.turn_running_at_attach",
		"the adopted shim is running a turn; the views stand it", dlog.Context{
			"turn_id": string(turn), "row_known": startedAt != nil,
		})
	w.sinks.Footer.OnTurnRunningAtAttach(w.ws, turn, startedAt)
	w.sinks.Sidebar.OnTurnRunningAtAttach(w.ws, turn, startedAt)
}

// restoreWaitingLocked stands the turns the session's facts name as WAITING
// behind `ahead`, the turn in flight, exactly as the watcher stood them when
// it saw them arrive (waitBehindAdoptedLocked). A turn waits only behind a
// turn the vendor started on its own, so `ahead` is the adopted turn. Each
// waiting turn's send reached the shim, so it is accepted: it takes its open
// edges when it stands in flight. The facts list the turns in the order they
// will run and the stack pops its newest, so they go on in reverse.
//
// A re-attaching daemon told only of the running turn closed the waiting one
// as ended unobserved while the shim went on to run it.
func (w *watcher) restoreWaitingLocked(ahead ids.TurnID, waiting []*conversationv1.TurnId) {
	if len(waiting) == 0 {
		return
	}
	adopted := ahead
	w.adopted = &adopted
	for i := len(waiting) - 1; i >= 0; i-- {
		turn := ids.TurnID(waiting[i].GetValue())
		if turn == "" {
			w.log.Error("daemon.sessionwatcher.turn_waiting_unidentified", "the session's facts name a waiting turn with no id; it is not stood behind the turn in flight", dlog.Context{
				"turn_in_flight": string(ahead),
			})
			continue
		}
		if !w.waitBehindAdoptedLocked(turn, true) {
			w.log.Debug("daemon.sessionwatcher.turn_waiting_not_restored", "a turn the session's facts name as waiting is the turn in flight or already ended; it is not stood behind it", dlog.Context{
				"turn_id": string(turn), "turn_in_flight": string(ahead),
			})
		}
	}
}

// reconcileOpenAtAttachLocked compares the turns the adoption found open with
// the shim's own answer, the facts' turn_in_flight. The shim is the authority
// on its own session: a turn it no longer names ended while no daemon was
// watching, and is queued for the lifecycle sink; the one it names is running
// and stays open, to be closed by its own terminal. It runs ONCE, on the first
// facts this watcher takes up, because the snapshot describes the moment of
// attaching and nothing after it.
func (w *watcher) reconcileOpenAtAttachLocked(started *conversationv1.SessionStarted) {
	if len(w.openAtAttach) == 0 {
		return
	}
	inFlight := ids.TurnID(started.GetTurnInFlight().GetValue())
	// A TURN WAITING BEHIND THE ONE IN FLIGHT IS HELD BY THE SHIM TOO: it runs
	// when the turn ahead of it ends, and its own terminal closes it.
	held := map[ids.TurnID]struct{}{inFlight: {}}
	for _, t := range started.GetTurnsWaiting() {
		held[ids.TurnID(t.GetValue())] = struct{}{}
	}
	for _, open := range w.openAtAttach {
		turn := open.ID
		if _, ok := held[turn]; ok {
			w.log.Info("daemon.sessionwatcher.turn_open_at_attach", "a turn open when this daemon attached is still held by the shim; it stays open", dlog.Context{
				"turn_id": string(turn), "turn_in_flight": string(inFlight),
			})
			continue
		}
		w.log.Info("daemon.sessionwatcher.turn_ended_unobserved", "a turn open when this daemon attached had ended while no daemon was watching; it is closed", dlog.Context{
			"turn_id": string(turn), "turn_in_flight": string(inFlight),
		})
		w.pendingUnobserved = append(w.pendingUnobserved, turn)
	}
	w.openAtAttach = nil
}

// adoptLiveWorkLocked opens a watch for every item the session says is already
// live. It runs AFTER the session and main watches are open, because the order
// the watches are opened in is the order a reader sees them.
func (w *watcher) adoptLiveWorkLocked(started *conversationv1.SessionStarted) {
	for _, item := range started.GetLiveWork() {
		w.routeDetachedWorkLocked(nil, item, nil, nil)
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
			w.shellStreamEnded(gen, s, err)
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
	adopted := w.pendingAdoptions
	w.pendingAdoptions = nil
	pending := w.pendingTurnEnds
	w.pendingTurnEnds = nil
	unobserved := w.pendingUnobserved
	w.pendingUnobserved = nil
	if len(adopted) > 0 || len(pending) > 0 || len(unobserved) > 0 {
		// THE DISPATCH IS JOINABLE. It is the one sink call this watcher makes
		// off its own mutex, and the sinks it drives read the state client --
		// so Close, which the daemon runs BEFORE closing that client, waits on
		// exactly this rather than tearing the store out from under a turn end
		// that is still being handled.
		w.dispatching.Add(1)
		defer w.dispatching.Done()
	}
	w.mu.Unlock()
	if len(unobserved) > 0 {
		w.sinks.Lifecycle.OnTurnsEndedUnobserved(w.ws, unobserved)
	}
	// AN ADOPTION BEFORE ANY END: a vendor-started turn short enough to open
	// and end within one flush is recorded before its close is stamped.
	for _, turn := range adopted {
		w.sinks.Lifecycle.OnTurnAdopted(w.ws, turn)
	}
	for _, ended := range pending {
		w.sinks.Lifecycle.OnTurnEnded(w.ws, ended.turn, ended.how)
		// A COMPLETED TURN means a new prompt was processed, so the title
		// digest may have changed; the synthesizer re-synthesizes only when it
		// actually did. Non-blocking (it dispatches its own goroutine).
		if w.sinks.Title != nil {
			w.sinks.Title.OnTurnEnded(w.ws)
		}
	}
}

// flushTurnEndsAsync hands the recorded turn ends to the lifecycle sink on a
// goroutine of its own, joinable through the same WaitGroup the inline flush
// uses.
//
// IT EXISTS FOR EXACTLY ONE CALLER: the release of a HELD terminal. That
// release runs on the PROMPT QUEUE'S OWN call into this watcher — the queue is
// what names the main agent, from StartTurn's answer, while it holds that
// workspace's delivery lock — and the lifecycle sink IS the prompt queue,
// whose OnTurnEnded takes the same lock. Told inline it is a self-deadlock,
// and the turn's delivery never returns. Every other turn end is recorded by a
// stream goroutine, which flushes inline the moment it drops mu.
func (w *watcher) flushTurnEndsAsync() {
	w.dispatching.Add(1)
	go func() {
		defer w.dispatching.Done()
		w.flushTurnEnds()
	}()
}

// stale reports whether a goroutine's generation has been superseded, which
// means its stream was torn down deliberately.
func (w *watcher) stale(gen uint64) bool { return w.closed || gen != w.gen }

// endingLocked reports whether the session this watcher serves is on its way
// out BY THIS DAEMON'S OWN DOING, so a standing stream ending is the answer to
// an act rather than a fault.
//
// THERE ARE TWO WAYS THE DAEMON SAYS SO AND BOTH ARE READ HERE. `SessionEnding`
// is the announcement one caller makes out of band (`Fleet.KillSession`), and
// the shim client's stand-down latch is the shim client's OWN record that this
// daemon asked this shim to go — a `KillSession`, or a `Kill` of the process.
// The latch is the one that cannot be bypassed: every route to ending a
// session goes through one of the two verbs that set it, while the
// announcement depends on each caller remembering to make it -- and the callers
// that do not remember are what put this record in the log.
//
// MEASURED. A relaunch bounce stands the old shim down through
// `rollout.controller.standDown`, which calls `KillSession` on the client
// directly and announces nothing. The shim ended its streams and exited 0; the
// watcher read both EOFs as severings and recorded two ERRORs, a `link_severed`
// WARN and the health fault that follows it, against a teardown this daemon had
// just ordered (realtest sweep 2026-09-12T15:23:03).
//
// AN UNEXPECTED END STAYS EXACTLY AS LOUD. Neither condition holds for a shim
// nobody asked to stand down, so a stream that ends under a live session still
// severs, still degrades the fleet and still raises its fault.
func (w *watcher) endingLocked() bool {
	return w.sessionEnded || w.client.StandingDown()
}

// openEndedByTeardownLocked classifies a failed watch open by the stand-down
// latch, exactly as streamEnded classifies an installed stream's end. An open
// decided before the shim was asked to stand down can still be in flight when
// the shim closes on its way out; its failure is the answer to a teardown this
// daemon ordered, so it neither severs the link nor raises a refusal fault. It
// answers false, recording nothing, when no teardown is under way.
func (w *watcher) openEndedByTeardownLocked(kind, key string, err error) bool {
	if !w.endingLocked() {
		return false
	}
	w.log.Debug("daemon.sessionwatcher.open_ended", "a watch open failed inside a teardown this daemon ordered", dlog.Context{
		"stream": kind, "key": key, "error": err.Error(), "stand_down_asked": w.client.StandingDown(),
	})
	return true
}

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
	if w.endingLocked() {
		w.log.Debug("daemon.sessionwatcher.stream_closed", "a stream ended with its session", dlog.Context{
			"stream": kind, "key": key, "stand_down_asked": w.client.StandingDown(),
		})
		return
	}
	w.severedLocked(operation, kind+" stream ended while the session was live", err, dlog.Context{
		"stream": kind, "key": key,
	})
}

// shellStreamEnded handles one detached shell's stream ending. A shell whose
// watch is STILL REGISTERED has not settled -- its own terminal frame is what
// reaps the watch -- so the stream ending early means the shim dropped a watch
// the daemon still needs, and the watch is opened again rather than left dark
// until the link happens to be redialed. The re-open is bounded, and a
// re-open that cannot be made severs exactly as before.
func (w *watcher) shellStreamEnded(gen uint64, s *shellWatch, err error) {
	w.mu.Lock()
	defer w.mu.Unlock()

	key := s.work.GetValue()
	if w.stale(gen) || s.done {
		w.log.Debug("daemon.sessionwatcher.stream_closed", "a torn-down stream ended", dlog.Context{
			"stream": "shell", "key": key,
		})
		return
	}
	if w.endingLocked() {
		w.log.Debug("daemon.sessionwatcher.stream_closed", "a stream ended with its session", dlog.Context{
			"stream": "shell", "key": key, "stand_down_asked": w.client.StandingDown(),
		})
		return
	}
	if w.shells[key] != s {
		w.log.Debug("daemon.sessionwatcher.stream_closed", "a replaced shell watch's stream ended", dlog.Context{
			"stream": "shell", "key": key,
		})
		return
	}
	s.stream = nil
	if s.reopens >= shellReopenLimit {
		w.severedLocked("watch_bash", "a detached shell's stream kept ending before the shell settled", err)
		return
	}
	s.reopens++
	ctx := dlog.Context{"work_id": key, "reopens": s.reopens}
	if err != nil {
		ctx["error"] = err.Error()
	}
	w.log.Warn("daemon.sessionwatcher.watch_bash",
		"a detached shell's stream ended before the shell settled; re-opening it", ctx)
	w.openShellStreamLocked(s)
}
