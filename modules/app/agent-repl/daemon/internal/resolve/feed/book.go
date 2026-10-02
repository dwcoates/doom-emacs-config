package feed

import (
	"context"
	"errors"
	"fmt"
	"sort"
	"sync"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
)

// FEED PAGING ON DEMAND (docs/protobuf-design/feed-paging-on-demand.md).
//
// A PAGE IS THE STORE'S PAGE. The store owns the one page size of the whole
// stack; this package states none. The daemon holds only what a reader asked
// for — the pages loaded through ReadHistory — plus what its watches deliver
// live and the rows it makes itself, and a feed page is whatever rows one
// store page resolves to (often fewer rows than entries: entries coalesce).
//
// EACH FEED THAT IS AN AGENT'S BOOK (the root: the session's main agent; a
// subagent bubble's sub-feed: that agent) keeps a bookState: the boundary every
// loaded page drew down to, where the next older page is read from, whether
// the conversation's start was reached, and the entries withheld until an
// older page loads. A merge tab or a shell is no agent's book: it holds what
// the daemon draws into it and pages nothing.
//
// WHERE THE DAEMON'S OWN ROWS SIT. A row the daemon makes itself (a cold gate,
// a merge row, a fault, a command panel, a mirrored prompt, a divider) carries
// an order key that FOLLOWS the row before it in conversation order (order.go),
// so it belongs to whichever page's range that row is in and is served with
// it, never on a page of its own. One drawn into a feed that holds no row of
// its plane is keyed at THE MOMENT IT WAS MADE (orderNowBase), which sorts it
// after every entry written before it and before every entry written after:
// in a feed whose history is loaded on demand that is ordinarily the newest
// page, which is where a row the daemon just made belongs.

// HistorySource reads one store page of one agent's history for a reader's
// request. Production wires it to the workspace's shim (ReadHistory), and it
// hands each page to the session watcher's views before answering it, so the
// page names the main agent and reconciles the footer as a watch's page did.
type HistorySource interface {
	// ReadHistory answers TARGET's newest page when AFTER is nil, and otherwise
	// the page of entries placed strictly before the entry AFTER names. A nil
	// TARGET is the session's main agent (the shim's prompt thread).
	// ErrNoHistorySource means no session is up to read from.
	ReadHistory(ctx context.Context, ws ids.WorkspaceID, target *conversationv1.AgentId, after *conversationv1.HistoryPointer) (*conversationv1.HistoryPage, error)
}

var (
	// ErrNoHistorySource is a HistorySource with no session to read from: a
	// cold or hibernated workspace. The feed serves what it holds; nothing
	// older is reachable until a session is up.
	ErrNoHistorySource = errors.New("feed: no session is up to read history from")
	// ErrHistoryUnavailable wraps every failure to read a page a reader's
	// request needed: the shim's or the store's refusal, or a transport error.
	ErrHistoryUnavailable = errors.New("feed: a history page could not be read")
)

// bookState is one feed's loaded history.
type bookState struct {
	// newestLoaded reports that the agent's newest page has been loaded since
	// the feed last held nothing.
	newestLoaded bool
	// liveSince reports that an entry drew a NEW row into this feed outside a
	// reader's load since the newest page was loaded: the newest store page
	// has moved, so the next open reads it again rather than serving a
	// "newest page" that has grown by everything since.
	liveSince bool
	// bounds are the order keys every loaded page drew down to, ascending: the
	// oldest row each load drew in this feed. A page of the feed is the rows
	// from one bound up to the next.
	bounds []string
	// after is the pointer the next older page is read after: the oldest
	// loaded page's `more.last_entry`. Nil before any page is loaded and once
	// the start is reached.
	after *conversationv1.HistoryPointer
	// floor reports that a loaded page reached the conversation's start.
	floor bool
	// awaitingSource reports that a reader opened this feed while no session
	// was up to read its history from: the newest page is loaded for it the
	// moment a watch of the session opens (kickWaitingReaders).
	awaitingSource bool
	// pending are the entries a load withheld because the turn they belong to
	// opened on a page not yet loaded (owner ruling 5: a row whose starting
	// entry lies on an unloaded page is not drawn until that page is), oldest
	// first. Each later load draws the ones its page made whole.
	pending []*conversationv1.HistoryEntryAt
}

// lowest is the oldest bound, "" when nothing was loaded.
func (b *bookState) lowest() string {
	if len(b.bounds) == 0 {
		return ""
	}
	return b.bounds[0]
}

// addBound records the oldest row a load drew in the feed.
func (b *bookState) addBound(key string) {
	at := sort.SearchStrings(b.bounds, key)
	if at < len(b.bounds) && b.bounds[at] == key {
		return
	}
	b.bounds = append(b.bounds, "")
	copy(b.bounds[at+1:], b.bounds[at:])
	b.bounds[at] = key
}

// pageLoad is one reader-requested page load being drawn (wsState.load).
type pageLoad struct {
	// state is the workspace state the load was started against; a reset
	// replaces it, and a page that arrives after one is discarded.
	state *wsState
	// feedKey is the feed whose book the page is.
	feedKey string
	// book is that feed's book.
	book *bookState
	// cutoff is the oldest bound before this load. A row this load draws NEW
	// in the feed below it is the reader's page and is delivered by the page,
	// never pushed (quiet); one at or above it completes rows a reader already
	// holds and is pushed. Empty when nothing was loaded before.
	cutoff string
	// low is the oldest row this load drew in the feed; drew reports any.
	low  string
	drew bool
	// quiet are the rows this load drew new below the cutoff.
	quiet map[string]bool
}

// noteDrawn records one row a load drew in its feed, and reports whether it is
// quiet: new, and below what was loaded before.
func (l *pageLoad) noteDrawn(id, key string, fresh bool) bool {
	if !l.drew || key < l.low {
		l.low, l.drew = key, true
	}
	if fresh && l.cutoff != "" && key < l.cutoff {
		l.quiet[id] = true
	}
	return l.quiet[id]
}

// withholds reports whether one entry of a loaded page waits for an older page:
// it NAMES a turn whose prompt this feed has not drawn, and older history
// remains in which that prompt lies. A prompt opens its own turn and is never
// withheld; at the conversation's start nothing older can complete anything.
//
// AN UNSTAMPED ENTRY IS NEVER WITHHELD. It names no turn, so nothing says an
// older page completes it; it is pre-contract data (every current entry is
// stamped), drawn by position as a replay always drew it and reported once
// as such (reportUnstampedReplay). Withholding it would also hold back rows
// the live plane already drew from the same entries.
func (r *resolver) withholds(s *wsState, at *conversationv1.HistoryEntryAt) bool {
	if s.replayAtFloor || at.GetEntry().GetUserPrompt() != nil {
		return false
	}
	turn := at.GetTurn().GetValue()
	return turn != "" && !s.knownTurns[ids.TurnID(turn)]
}

// redrawPending draws every entry an earlier load withheld that the page just
// drawn made whole, in conversation order and in the replay the page left
// standing, and answers the ones still waiting.
func (r *resolver) redrawPending(s *wsState, agent *conversationv1.AgentId, load *pageLoad) []*conversationv1.HistoryEntryAt {
	var still []*conversationv1.HistoryEntryAt
	drawn := 0
	for _, at := range load.book.pending {
		if r.withholds(s, at) {
			still = append(still, at)
			continue
		}
		r.replayPageEntry(s, agent, at)
		drawn++
	}
	if drawn > 0 {
		r.logger(s.id).Debug("daemon.feed.history_entries_completed",
			"entries withheld for an older page were drawn now that it is loaded",
			dlog.Context{"feed": load.feedKey, "drawn": drawn, "still_withheld": len(still)})
	}
	return still
}

// bookTarget answers the agent whose book a feed is: nil for the root (the
// main agent, which the shim resolves), the sub-feed's agent, and false for a
// feed that is no agent's book.
func bookTarget(addr feedid.Feed) (*conversationv1.AgentId, bool) {
	switch {
	case addr.Root:
		return nil, true
	case addr.Agent != nil:
		return addr.Agent, true
	}
	return nil, false
}

// loadPlan is what one feed would read next, decided under the lock and read
// off it.
type loadPlan struct {
	// addr is the feed whose book is read: the asking feed's own, or the root
	// for a merge tab (bookTarget).
	addr   feedid.Feed
	state  *wsState
	target *conversationv1.AgentId
	after  *conversationv1.HistoryPointer
	newest bool
	// pushAll publishes every row the load draws, quiet or not: a load no
	// reader's page delivers (LoadOlder).
	pushAll bool
}

// planLoad decides the next page FEED's book would read: its newest page when
// NEWEST, the next older one otherwise. False when the feed pages nothing, no
// source is wired, or the conversation's start was already reached. Called
// with r.mu held.
func (r *resolver) planLoad(s *wsState, f *feedState, newest bool) (loadPlan, bool) {
	addr := s.feedAddrs[f.key]
	target, isBook := bookTarget(addr)
	if !isBook || r.deps.History == nil {
		return loadPlan{}, false
	}
	if newest || !f.book.newestLoaded {
		return loadPlan{addr: addr, state: s, target: target, newest: true}, true
	}
	if f.book.floor {
		return loadPlan{}, false
	}
	return loadPlan{addr: addr, state: s, target: target, after: f.book.after}, true
}

// planOpening decides the newest page an OPENING of FEED reads first: its own
// book's, unless held and unmoved. A MERGE TAB IS NO BOOK, but every row it
// draws is the main agent's (its addressed turns), so its opening reads the
// root's newest page when the root holds none. Called with r.mu held.
func (r *resolver) planOpening(s *wsState, f *feedState) (loadPlan, bool) {
	if s.feedAddrs[f.key].Merge != nil {
		f = r.feed(s, feedid.Feed{Root: true})
		if f.book.newestLoaded {
			return loadPlan{}, false
		}
	} else if f.book.newestLoaded && !f.book.liveSince {
		return loadPlan{}, false
	}
	return r.planLoad(s, f, true)
}

// loadMu answers the mutex that serializes one workspace's page loads, so two
// readers never read and draw the same older page twice. It is held across
// the read, which is why it is not the resolver's own mutex.
func (r *resolver) loadMu(ws ids.WorkspaceID) *sync.Mutex {
	r.mu.Lock()
	defer r.mu.Unlock()
	if r.loads == nil {
		r.loads = map[ids.WorkspaceID]*sync.Mutex{}
	}
	mu, ok := r.loads[ws]
	if !ok {
		mu = &sync.Mutex{}
		r.loads[ws] = mu
	}
	return mu
}

// loaded is what one load drew in its feed: the oldest row, when it drew any.
type loaded struct {
	low  string
	drew bool
}

// load reads the page PLAN names OFF the resolver's mutex and draws it into
// its feed's book (plan.addr). ErrNoHistorySource passes through unwrapped; every other
// failure is ErrHistoryUnavailable.
func (r *resolver) load(ctx context.Context, plan loadPlan) (loaded, error) {
	ws, addr := plan.state.id, plan.addr
	log := r.lockedLogger(ws)
	page, err := r.deps.History.ReadHistory(ctx, ws, plan.target, plan.after)
	if errors.Is(err, ErrNoHistorySource) {
		log.Info("daemon.feed.history_load_no_source",
			"a reader's page could not be loaded: no session is up to read history from; the feed serves what it holds",
			dlog.Context{"agent": plan.target.GetValue(), "newest": plan.newest})
		return loaded{}, err
	}
	if err != nil {
		log.Error("daemon.feed.history_load_failed",
			"a page a reader's request needed could not be read",
			dlog.Context{"agent": plan.target.GetValue(), "newest": plan.newest, "cause": err.Error()})
		return loaded{}, fmt.Errorf("%w: %v", ErrHistoryUnavailable, err)
	}
	entries := page.GetEntries()
	if len(entries) == 0 && page.GetFloor() == nil {
		// A PAGE OF NOTHING THAT CLAIMS MORE BEFORE IT names no entry to read
		// after: following it would read the same page forever.
		log.Error("daemon.feed.history_load_empty_more",
			"a history page carried no entries yet claimed older history remains",
			dlog.Context{"agent": plan.target.GetValue(), "newest": plan.newest})
		return loaded{}, fmt.Errorf("%w: a page with no entries claimed older history", ErrHistoryUnavailable)
	}
	if page.GetFloor() == nil && page.GetMore().GetLastEntry().GetValue() == "" {
		// THE NEXT OLDER PAGE IS READ AFTER THE POINTER `more` NAMES, and a page
		// naming none leaves nowhere to read the next one from.
		log.Error("daemon.feed.history_load_no_boundary",
			"a history page stated neither the conversation's start nor the pointer older history is read after",
			dlog.Context{"agent": plan.target.GetValue(), "newest": plan.newest, "boundary": boundaryName(page)})
		return loaded{}, fmt.Errorf("%w: a page stated no boundary to read older history from", ErrHistoryUnavailable)
	}

	r.mu.Lock()
	f := r.feed(plan.state, addr)
	l := &pageLoad{
		state:   plan.state,
		feedKey: f.key,
		book:    &f.book,
		cutoff:  f.book.lowest(),
		quiet:   map[string]bool{},
	}
	if plan.pushAll {
		l.cutoff = ""
	}
	r.mu.Unlock()

	agent := plan.target
	if agent == nil {
		agent = r.mainAgentID(ws)
	}
	r.replayPage(ws, agent, page, l)

	r.mu.Lock()
	defer r.mu.Unlock()
	if r.workspaces[ws] != plan.state {
		return loaded{}, nil
	}
	// A RE-READ NEWEST PAGE MOVES ONLY THE TOP: the older pages a walk
	// already loaded stay loaded, and the next older page is still read from
	// below the oldest of them.
	if !plan.newest || !f.book.newestLoaded {
		f.book.after = page.GetMore().GetLastEntry()
		f.book.floor = page.GetFloor() != nil
	}
	if plan.newest {
		f.book.newestLoaded = true
		f.book.liveSince = false
	}
	if l.drew {
		f.book.addBound(l.low)
	}
	if f.book.floor && len(f.book.bounds) > 0 {
		// AT THE CONVERSATION'S START THE OLDEST PAGE REACHES THE FEED'S TOP:
		// every row held below its bound — a fork's ported conversation, a
		// daemon-made row keyed before the history — is served with it rather
		// than standing on a page of its own.
		f.book.bounds[0] = ""
	}
	log.Info("daemon.feed.history_loaded",
		"a store page a reader's request needed was loaded into its feed",
		dlog.Context{
			"feed": f.key, "agent": plan.target.GetValue(), "newest": plan.newest,
			"entries": len(entries), "floor": f.book.floor, "drew_rows": l.drew,
			"bound": l.low, "quiet": len(l.quiet), "pending": len(f.book.pending),
		})
	return loaded{low: l.low, drew: l.drew}, nil
}

// mainAgentID answers the session's main agent as this feed knows it, nil
// when none is named yet.
func (r *resolver) mainAgentID(ws ids.WorkspaceID) *conversationv1.AgentId {
	r.mu.Lock()
	defer r.mu.Unlock()
	if main := r.state(ws).mainAgent; main != "" {
		return &conversationv1.AgentId{Value: main}
	}
	return nil
}

// lockedLogger resolves a workspace's logger under the resolver's mutex, for a
// caller that does not hold it.
func (r *resolver) lockedLogger(ws ids.WorkspaceID) dlog.Logger {
	r.mu.Lock()
	defer r.mu.Unlock()
	return r.logger(ws)
}

// historyKickBound bounds the newest-page read a session's first watch opening
// makes for a reader that opened its feed before the session was up. It is a
// last resort on one local ReadHistory (the shim answers it off the store in
// milliseconds), sized like boot.DefaultAdoptBound; an overrun is ERROR and
// the reader's next open reads the page again.
const historyKickBound = 10 * time.Second

// kickWaitingReaders loads AGENT's newest page for the readers that opened its
// feed before a session was up to read it from (bookState.awaitingSource). A
// watch of AGENT just opened, so its session is up now. The load runs OFF the
// caller's goroutine: the caller is the session watcher, and the read hands
// the page back to it (HistorySource). Called with r.mu held.
func (r *resolver) kickWaitingReaders(s *wsState, agent *conversationv1.AgentId) {
	addr := feedid.Feed{Root: true}
	if agent.GetValue() != "" && agent.GetValue() != s.mainAgent {
		addr = feedid.Feed{Agent: agent}
	}
	f, ok := s.feeds[r.feedKey(s.id, addr)]
	if !ok || !f.book.awaitingSource {
		return
	}
	f.book.awaitingSource = false
	r.logger(s.id).Info("daemon.feed.history_kick",
		"a watch opened for a feed a reader holds without its history; its newest page is loaded for it",
		dlog.Context{"feed": f.key, "agent": agent.GetValue()})
	ws := s.id
	go func() {
		ctx, cancel := context.WithTimeout(context.Background(), historyKickBound)
		defer cancel()
		if _, err := r.loadPushed(ctx, ws, addr, true); err != nil {
			r.lockedLogger(ws).Error("daemon.feed.history_kick_failed",
				"the newest page a waiting reader needed could not be loaded; its next open reads it again",
				dlog.Context{"feed": f.key, "cause": err.Error()})
		}
	}()
}

// loadPushed loads ADDR's newest (NEWEST) or next older page for no reader's
// page in particular: every row it draws is PUSHED, and every standing walk of
// the feed that had served down to the oldest loaded row is moved down to the
// page loaded, so a reader holds those rows exactly as a page would have
// served them. False when nothing could be loaded.
func (r *resolver) loadPushed(ctx context.Context, ws ids.WorkspaceID, addr feedid.Feed, newest bool) (bool, error) {
	mu := r.loadMu(ws)
	mu.Lock()
	defer mu.Unlock()

	r.mu.Lock()
	s := r.state(ws)
	f := r.feed(s, addr)
	_, bounded := r.deliverable(s, f, "load_pushed")
	var (
		plan loadPlan
		ok   bool
	)
	if newest {
		plan, ok = r.planOpening(s, f)
	} else {
		plan, ok = r.planLoad(s, f, false)
		ok = ok && !bounded
	}
	prevLow := f.book.lowest()
	r.mu.Unlock()
	if !ok {
		r.lockedLogger(ws).Debug("daemon.feed.load_pushed_nothing",
			"a pushed page load found nothing to load",
			dlog.Context{"feed": f.key, "newest": newest, "bounded_by_separation": bounded})
		return false, nil
	}
	plan.pushAll = true
	got, err := r.load(ctx, plan)
	if errors.Is(err, ErrNoHistorySource) {
		return false, nil
	}
	if err != nil {
		return false, err
	}

	r.mu.Lock()
	defer r.mu.Unlock()
	moved := 0
	for _, w := range s.readers {
		if w.feedKey != f.key || w.top || w.oldest == nil || !got.drew {
			continue
		}
		if prevLow != "" && *w.oldest > prevLow {
			continue
		}
		if got.low < *w.oldest {
			low := got.low
			w.oldest = &low
			moved++
		}
	}
	r.logger(ws).Debug("daemon.feed.load_pushed",
		"a page was loaded for no reader's page and pushed; the walks that held everything loaded were moved to it",
		dlog.Context{"feed": f.key, "newest": newest, "walks_moved": moved, "drew_rows": got.drew})
	return true, nil
}
