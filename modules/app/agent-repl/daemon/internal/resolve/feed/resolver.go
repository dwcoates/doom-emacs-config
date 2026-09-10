package feed

import (
	"context"
	"errors"
	"fmt"
	"sync"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"google.golang.org/protobuf/proto"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
	"claude-repld/internal/paint"
	"claude-repld/internal/sessionwatcher"
)

// errNotARow is what a family function answers for a frame that DRAWS NOTHING
// — an unmodeled tool, a succeeded hook, an artifact listing. It is a typed
// answer rather than a nil row so a caller can never mistake "nothing to draw"
// for "the resolver failed".
var errNotARow = errors.New("feed: this frame draws no row")

// resolver is the feed resolver. One instance serves every workspace; the
// per-workspace state hangs off it under one mutex, because every input is an
// event and none of them is long-running.
type resolver struct {
	deps Deps

	mu         sync.Mutex
	workspaces map[ids.WorkspaceID]*wsState
	// tokens is the whole watch-token table: minting here is what makes a
	// token opaque, foreign tokens detectable, and pinning exact.
	tokens map[string]*watchToken
	// tokenSeq mints token values.
	tokenSeq uint64
	// loggers caches each workspace's resolved durable logger.
	loggers map[ids.WorkspaceID]dlog.Logger
}

// wsState is one workspace's whole feed universe plus the accumulation every
// piecemeal frame folds into.
type wsState struct {
	id ids.WorkspaceID

	// feeds are the workspace's feeds, keyed by the encoded feed address.
	feeds map[string]*feedState
	// feedAddrs remembers each key's decoded address, for breadcrumbs.
	feedAddrs map[string]feedid.Feed
	// subFeeds maps a sub-feed's key to the bubble row that owns it, which is
	// how a page deep inside nesting draws its crumbs.
	subFeeds map[string]*subFeedHead

	// address is the output address a lease holder installed; nil is the root
	// feed.
	address *sessionwatcher.OutputAddress

	// mainAgent is the first agent this workspace ever produced a frame for.
	// Every later agent the resolver has not seen created is UNPLACEABLE, and
	// lands on the root feed with a warning rather than being dropped.
	mainAgent string
	// agentFeeds maps a created subagent's id to its sub-feed key.
	agentFeeds map[string]string

	// synthSeq is the per-workspace monotonically increasing synthesized
	// sequence non-durable rows mint their identity from.
	synthSeq uint64

	// readers are the standing page walks, one per open connection.
	readers map[ReaderID]*walk

	// units is the per-activity accumulation: the start facts a settled frame
	// is drawn against, and the fold of a growing one.
	units map[string]*unitState
	// responses is the prose fold, keyed by activity id.
	responses map[string]*proseState
	// plans is the open plan episode per agent, keyed by agent id.
	plans map[string]*planState
	// planEpisodes counts the episodes an agent has had, so a second episode
	// never collides with the first's FeedId.
	planEpisodes map[string]uint64
	// shells is the detached-shell accumulation, keyed by detached work id.
	shells map[string]*shellState
	// detachedUnits are the units a detachment announced BEFORE this resolver
	// had drawn them, kept so the placement survives the order the frames
	// arrive in. A store replay is the ordinary case: a unit's row replays at
	// the position of its LAST upsert — its terminal — which is after the
	// detachment that was announced while it was still running, so a resolver
	// that could only apply a detachment to an already-drawn unit redrew the
	// work as if it had never left the turn.
	detachedUnits map[string]string
	// subagents is the bubble accumulation, keyed by the SPAWN unit's id.
	subagents map[string]*subagentState
	// standing holds each permission row's offered standing token, DAEMON-SIDE.
	standing map[string]*conversationv1.AgentPermissionStanding
	// permissionRows maps a permission ask id to the row it drew, so a
	// decision upserts the same row.
	permissionRows map[string]*permissionState
	// gatedCalls maps a permission ask id to the activity it gates, so a
	// denial marks that call denied.
	gatedCalls map[string]string
	// questionAsks maps a question ask id to what the daemon SERVED for it, so
	// an answer is delivered to the right agent and echoed against the batch
	// the user actually saw.
	questionAsks map[string]*questionState
	// turnEvidence collects per-turn evidence lines — a mid-turn api error, a
	// compaction that failed — that the turn's terminal row surfaces.
	turnEvidence map[string][]turnEvidenceLine
	// turnRefusals records that a response in this turn ended on the vendor's
	// REFUSAL. conversation.v1's AgentModelError is an empty message, so the
	// terminal alone cannot say whether the model errored or refused; the
	// response's own AgentResponseFailureReason.refused is the only place that
	// fact is stated, and the terminal row is drawn after it.
	turnRefusals map[string]bool
	// turnQueryDeaths records, per turn, that the SESSION's query died under
	// it. The shim owes every open turn a terminal and writes one as
	// AgentFailure.execution_error -- conversation.v1 gives a dead query no
	// failure arm of its own -- so that frame arrives after the death and
	// would otherwise redraw the terminal as execution_error. The death is the
	// truer account, and feed.proto has an arm for exactly it, so the witness
	// outlives the row it drew and the cause it carries is the death's own.
	turnQueryDeaths map[string]*conversationv1.SessionQueryDied
	// turnInFlight is the turn the session is running, learned from the rows
	// it stamps. It is what a session-scoped death (query_died) terminates.
	turnInFlight *ids.TurnID
	// turnStamp is the turn ROWS ARE ATTRIBUTED TO. It tracks turnInFlight
	// except across a QUERY DEATH, which is the one ending that still OWES
	// rows: the gate's stand-down denies every permission ask left pending,
	// and those denials arrive after the death's terminal row. They belong to
	// the turn that was running — feed.proto leaves `turn` unset only "for a
	// row that belongs to no turn" — so the death keeps the stamp standing
	// while an ordinary terminal clears it.
	turnStamp *ids.TurnID
	// answerRows maps a response activity id to the row it drew, so the turn's
	// conclusion can name its answering row.
	answerRows map[string]*frontendv1.FeedId

	// apiResponseSeq numbers the API responses observed so far; a unit
	// arriving with usage opens the next one.
	apiResponseSeq uint64
	// unitAPIResponse files each unit under the API response it arrived in.
	unitAPIResponse map[string]uint64
	// apiResponseUsage is each API response's formatted cost stamp, keyed by
	// the response's number. Absent means that response stated no usage.
	apiResponseUsage map[uint64]string
}

// subFeedHead is the bubble row a sub-feed lives inside.
type subFeedHead struct {
	// row is the head row's identity.
	row *frontendv1.FeedId
	// parentFeed is the key of the feed that head row sits on.
	parentFeed string
	// label is the crumb's drawn text.
	label string
}

// feedState is one feed: its rows in first-appearance order, the publication
// log a tail replays from, and the subscribers following it.
type feedState struct {
	// key is the encoded feed address.
	key string
	// order is the row ids in FIRST-APPEARANCE order; an upsert never moves a
	// row.
	order []string
	// rows is the current whole of each row.
	rows map[string]*frontendv1.FeedRow
	// nonDurable marks the rows that exist in resolver memory only and never
	// appear in a page.
	nonDurable map[string]bool
	// seq is the publication counter: every upsert publication takes the next
	// value, and a watch token pins to one.
	seq uint64
	// log is the retained publication log, oldest first.
	log []*loggedRow
	// retention caps the log.
	retention int
	// subs are the tails following this feed.
	subs map[*tailSub]struct{}
	// historyMore records that older history exists beyond what was replayed,
	// so a walk that reaches the oldest replayed row answers truncated rather
	// than claiming the start.
	historyMore *frontendv1.FailureHistoryReplayTruncated
}

// loggedRow is one publication: the row as published, and the sequence it took.
type loggedRow struct {
	seq uint64
	row *frontendv1.FeedRow
}

// walk is one reader's page position: the index into order of the OLDEST row
// it has been served. Ephemeral, dropped on every open and on CloseReader.
type walk struct {
	// feedKey is the feed this walk is of.
	feedKey string
	// oldest is the index of the oldest row served so far.
	oldest int
	// standing reports whether the walk has been opened at all.
	standing bool
}

// watchToken is one minted tail address: which feed, and the publication the
// tail begins AFTER.
type watchToken struct {
	ws       ids.WorkspaceID
	feedKey  string
	afterSeq uint64
}

// newResolver validates the dependencies and builds the resolver.
func newResolver(deps Deps) (*resolver, error) {
	if deps.Log == nil {
		return nil, errors.New("feed: Deps.Log is required")
	}
	if deps.WorkspaceDir == nil {
		return nil, errors.New("feed: Deps.WorkspaceDir is required")
	}
	if deps.Encode == nil {
		deps.Encode = feedid.Encode
	}
	if deps.EncodeFeed == nil {
		deps.EncodeFeed = feedid.EncodeFeed
	}
	if deps.StripSentinels == nil {
		deps.StripSentinels = func(s string) string { return s }
	}
	if deps.Now == nil {
		deps.Now = time.Now
	}
	if deps.PageSize <= 0 {
		deps.PageSize = DefaultPageSize
	}
	if deps.TailRetention <= 0 {
		deps.TailRetention = DefaultTailRetention
	}
	return &resolver{
		deps:       deps,
		workspaces: map[ids.WorkspaceID]*wsState{},
		tokens:     map[string]*watchToken{},
		loggers:    map[ids.WorkspaceID]dlog.Logger{},
	}, nil
}

// logger resolves a workspace's durable logger. Failing to resolve the
// workspace is an INVARIANT VIOLATION, recorded once at ERROR against the
// global surface — the record is about the resolution failing, not about the
// workspace, which is why it is the one thing that may be global.
func (r *resolver) logger(ws ids.WorkspaceID) dlog.Logger {
	if l, ok := r.loggers[ws]; ok {
		return l
	}
	global := r.deps.Log.Global()
	dir, err := r.deps.WorkspaceDir(ws)
	if err != nil {
		if errors.Is(err, context.Canceled) || errors.Is(err, context.DeadlineExceeded) {
			// THE DAEMON IS GOING AWAY, and the workspace lookup runs under
			// the daemon's own context. A cancelled read is that shutdown, not
			// an unresolvable workspace: it is recorded at INFO, and the global
			// fallback is NOT cached, so a resolver that outlives the
			// cancellation still routes the workspace's records to its own
			// sink rather than to the global one forever.
			global.Info("daemon.feed.workspace_lookup_cancelled",
				"a workspace's log sink was not resolved; the daemon's context was cancelled",
				dlog.Context{"workspace": string(ws), "cause": err.Error()})
			return global
		}
		global.Error("daemon.feed.workspace_unresolved",
			"the feed resolver could not resolve a workspace's log sink; its records are unroutable",
			dlog.Context{"workspace": string(ws), "cause": err.Error()})
		r.loggers[ws] = global
		return global
	}
	l, err := r.deps.Log.Workspace(dir)
	if err != nil {
		global.Error("daemon.feed.workspace_sink_unavailable",
			"the feed resolver could not open a workspace's log sink; its records are unroutable",
			dlog.Context{"workspace": string(ws), "dir": dir, "cause": err.Error()})
		r.loggers[ws] = global
		return global
	}
	l = l.With(dlog.Context{"workspace": string(ws)})
	r.loggers[ws] = l
	return l
}

// state resolves a workspace's state, creating it on first sight.
func (r *resolver) state(ws ids.WorkspaceID) *wsState {
	s, ok := r.workspaces[ws]
	if ok {
		return s
	}
	s = &wsState{
		id:              ws,
		feeds:           map[string]*feedState{},
		feedAddrs:       map[string]feedid.Feed{},
		subFeeds:        map[string]*subFeedHead{},
		agentFeeds:      map[string]string{},
		readers:         map[ReaderID]*walk{},
		units:           map[string]*unitState{},
		responses:       map[string]*proseState{},
		plans:           map[string]*planState{},
		planEpisodes:    map[string]uint64{},
		shells:          map[string]*shellState{},
		detachedUnits:   map[string]string{},
		subagents:       map[string]*subagentState{},
		standing:        map[string]*conversationv1.AgentPermissionStanding{},
		permissionRows:  map[string]*permissionState{},
		questionAsks:    map[string]*questionState{},
		gatedCalls:      map[string]string{},
		turnEvidence:    map[string][]turnEvidenceLine{},
		turnRefusals:    map[string]bool{},
		turnQueryDeaths: map[string]*conversationv1.SessionQueryDied{},
		answerRows:      map[string]*frontendv1.FeedId{},

		unitAPIResponse:  map[string]uint64{},
		apiResponseUsage: map[uint64]string{},
	}
	r.workspaces[ws] = s
	return s
}

// feed resolves one feed's state within a workspace, creating it on first
// sight and remembering its address for breadcrumbs.
func (r *resolver) feed(s *wsState, addr feedid.Feed) *feedState {
	key := r.feedKey(s.id, addr)
	f, ok := s.feeds[key]
	if ok {
		return f
	}
	f = &feedState{
		key:        key,
		rows:       map[string]*frontendv1.FeedRow{},
		nonDurable: map[string]bool{},
		retention:  r.deps.TailRetention,
		subs:       map[*tailSub]struct{}{},
	}
	s.feeds[key] = f
	s.feedAddrs[key] = addr
	return f
}

// feedKey renders a feed address as the string this resolver keys by.
func (r *resolver) feedKey(ws ids.WorkspaceID, addr feedid.Feed) string {
	id := r.deps.EncodeFeed(ws, addr)
	if id.GetValue() != "" {
		return id.GetValue()
	}
	// A feedid implementation that has not landed yet encodes to the empty
	// value for every address. Keying every feed alike would silently merge
	// the universe, so the resolver derives a key of its own from the address
	// rather than trusting an empty encoding.
	switch {
	case addr.Merge != nil:
		return "merge:" + string(*addr.Merge)
	case addr.Agent != nil:
		return "agent:" + addr.Agent.GetValue()
	default:
		return "root"
	}
}

// placement is where one frame's row lands: which feed, and which row it nests
// under for presentation.
type placement struct {
	feed   feedid.Feed
	parent *frontendv1.FeedRowParent
}

// place answers where an agent's rows go. WHILE AN OUTPUT ADDRESS IS SET every
// row the session produces goes on the addressed feed under the addressed row;
// cleared, an agent's rows go on its own sub-feed, and the main agent's on the
// root.
//
// An agent the resolver never saw created is placed on the ROOT with a WARN —
// never dropped: a row nobody can place is still a row the user must see.
func (r *resolver) place(s *wsState, agent *conversationv1.AgentId) placement {
	if s.address != nil {
		return r.outputPlacement(s)
	}
	id := agent.GetValue()
	if id == "" || id == s.mainAgent {
		if s.mainAgent == "" {
			s.mainAgent = id
		}
		return placement{feed: feedid.Feed{Root: true}}
	}
	if s.mainAgent == "" {
		s.mainAgent = id
		return placement{feed: feedid.Feed{Root: true}}
	}
	if _, ok := s.agentFeeds[id]; ok {
		return placement{feed: feedid.Feed{Agent: agent}}
	}
	r.logger(s.id).Warn("daemon.feed.unplaceable_agent",
		"a frame arrived for an agent whose creation was never seen; the row lands on the root feed",
		dlog.Context{"agent": id, "main_agent": s.mainAgent})
	return placement{feed: feedid.Feed{Root: true}}
}

// outputPlacement is where a row the LEASE HOLDER draws for this session
// belongs: the standing output address, or the root feed when none stands.
func (r *resolver) outputPlacement(s *wsState) placement {
	if s.address == nil {
		return placement{feed: feedid.Feed{Root: true}}
	}
	p := placement{feed: s.address.Feed}
	if s.address.Parent != nil {
		p.parent = &frontendv1.FeedRowParent{Row: r.deps.Encode(*s.address.Parent)}
	}
	return p
}

// upsert replaces one row whole and publishes it on its feed's tail. It is the
// ONE write path: every family function ends here, so identity, ordering and
// publication can never disagree between families.
func (r *resolver) upsert(s *wsState, at placement, row *frontendv1.FeedRow, durable bool) {
	f := r.feed(s, at.feed)
	id := row.GetId().GetValue()
	if id == "" {
		r.logger(s.id).Error("daemon.feed.row_without_identity",
			"a composed row carried no FeedId and cannot be upserted",
			dlog.Context{"feed": f.key})
		return
	}
	if at.parent != nil && row.Parent == nil {
		row.Parent = at.parent
	}
	// A ROW IS PUBLISHED AS A SNAPSHOT. A family composes its row from
	// accumulated state it keeps mutating, so the value handed here can be the
	// very object a later frame edits: retaining it would let a published row
	// change under a reader, and would make it compare equal to its own
	// successor.
	snapshot, ok := proto.Clone(row).(*frontendv1.FeedRow)
	if !ok {
		r.logger(s.id).Error("daemon.feed.row_not_clonable",
			"a composed row could not be snapshotted for publication",
			dlog.Context{"feed": f.key, "row": id})
		return
	}
	existing, seen := f.rows[id]
	if seen && proto.Equal(existing, snapshot) {
		// AN IDENTICAL ROW IS NOT A PUBLICATION. A repeated frame — a stream
		// re-opening with the start it already announced, a re-delivered
		// upsert — states nothing new, and pushing it again is churn every
		// reader would have to filter for itself.
		r.logger(s.id).Debug("daemon.feed.row_unchanged",
			"an upsert restated the row it already published",
			dlog.Context{"feed": f.key, "row": id})
		return
	}
	if !seen {
		f.order = append(f.order, id)
	}
	f.rows[id] = snapshot
	if !durable {
		f.nonDurable[id] = true
	}
	f.seq++
	f.log = append(f.log, &loggedRow{seq: f.seq, row: snapshot})
	if len(f.log) > f.retention {
		f.log = f.log[len(f.log)-f.retention:]
	}
	for sub := range f.subs {
		sub.enqueue(snapshot)
	}
}

// retire removes a row from a feed. A retired row stops appearing in pages;
// the tail's already-delivered publications are history and are not rewritten.
func (r *resolver) retire(s *wsState, addr feedid.Feed, id string) bool {
	f := r.feed(s, addr)
	if _, ok := f.rows[id]; !ok {
		return false
	}
	delete(f.rows, id)
	delete(f.nonDurable, id)
	for i, existing := range f.order {
		if existing == id {
			f.order = append(f.order[:i], f.order[i+1:]...)
			break
		}
	}
	return true
}

// rowID composes a row's identity from the address of what it DRAWS.
func (r *resolver) rowID(ws ids.WorkspaceID, addr feedid.Feed, key feedid.RowKey) *frontendv1.FeedId {
	id := r.deps.Encode(feedid.Ref{WS: ws, Feed: addr, Row: key})
	if id.GetValue() != "" {
		return id
	}
	// The same fallback the feed key takes, and for the same reason: an
	// unlanded encoder must not collapse every row onto one identity.
	return &frontendv1.FeedId{Value: fmt.Sprintf("%s|%s|%s|%s|%s",
		ws, r.feedKey(ws, addr), key.Kind, key.ID, key.Sub)}
}

// SetOutputAddress installs the address a lease holder wants this session's
// rows stamped with; nil restores the root feed.
func (r *resolver) SetOutputAddress(ws ids.WorkspaceID, addr *sessionwatcher.OutputAddress) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	s.address = addr
	target := "root"
	if addr != nil {
		target = r.feedKey(ws, addr.Feed)
	}
	r.logger(ws).Debug("daemon.feed.output_address",
		"the feed's output address changed", dlog.Context{"feed": target, "cleared": addr == nil})
}

// UpsertAtOutputAddress upserts a daemon-synthesized row AT THE SESSION'S
// STANDING OUTPUT ADDRESS: the row's identity is composed from the addressed
// feed and it is parented exactly as the resolver's own draw of the same row
// key will be, so the two are ONE row rather than a root copy the addressed
// draw can never replace.
func (r *resolver) UpsertAtOutputAddress(ws ids.WorkspaceID, key feedid.RowKey, row *frontendv1.FeedRow) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	at := r.outputPlacement(s)
	row.Id = r.rowID(ws, at.feed, key)
	r.logger(ws).Debug("daemon.feed.synthesized_at_output",
		"a daemon-synthesized row was upserted at the session's output address",
		dlog.Context{"feed": r.feedKey(ws, at.feed), "row": row.GetId().GetValue()})
	r.upsert(s, at, row, true)
}

// UpsertSynthesized upserts a daemon-synthesized row on the named feed.
func (r *resolver) UpsertSynthesized(ws ids.WorkspaceID, feed feedid.Feed, row *frontendv1.FeedRow) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	r.logger(ws).Debug("daemon.feed.synthesized",
		"a daemon-synthesized row was upserted",
		dlog.Context{"feed": r.feedKey(ws, feed), "row": row.GetId().GetValue()})
	r.upsert(s, placement{feed: feed}, row, true)
}

// UpsertCommandPanel mints a NON-DURABLE root-feed row carrying a recognized
// command's panel.
func (r *resolver) UpsertCommandPanel(ws ids.WorkspaceID, panel *frontendv1.FeedCommandPanel) *frontendv1.FeedId {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	root := feedid.Feed{Root: true}
	id := r.synthID(s, root)
	row := &frontendv1.FeedRow{Id: id, Row: &frontendv1.FeedRow_CommandPanel{CommandPanel: panel}}
	r.logger(ws).Debug("daemon.feed.command_panel",
		"a recognized command's panel was mirrored into the root feed as a non-durable row",
		dlog.Context{"row": id.GetValue()})
	r.upsert(s, placement{feed: root}, row, false)
	return id
}

// UpsertCommandRefused mints a NON-DURABLE root-feed refusal card.
func (r *resolver) UpsertCommandRefused(ws ids.WorkspaceID, command, reason string, addSupport bool) *frontendv1.FeedId {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	root := feedid.Feed{Root: true}
	id := r.synthID(s, root)
	refused := &frontendv1.FeedCommandRefused{
		Command: &frontendv1.FeedCommandRefusedCommand{Text: command},
		Reason:  &frontendv1.FeedCommandRefusedReason{Text: reason},
	}
	if addSupport {
		refused.AddSupport = &frontendv1.FeedCommandAddSupportOffer{}
	}
	row := &frontendv1.FeedRow{Id: id, Row: &frontendv1.FeedRow_CommandRefused{CommandRefused: refused}}
	r.logger(ws).Debug("daemon.feed.command_refused",
		"a recognized-but-unsupported command's refusal card was mirrored into the root feed",
		dlog.Context{"row": id.GetValue(), "command": command, "add_support": addSupport})
	r.upsert(s, placement{feed: root}, row, false)
	return id
}

// synthID mints the next synthesized identity for a workspace.
func (r *resolver) synthID(s *wsState, addr feedid.Feed) *frontendv1.FeedId {
	s.synthSeq++
	return r.rowID(s.id, addr, feedid.RowKey{
		Kind: feedid.KindSynth,
		ID:   fmt.Sprintf("%d", s.synthSeq),
	})
}

// RetireRow removes a synthesized row whose reason to exist ended.
func (r *resolver) RetireRow(ws ids.WorkspaceID, feed feedid.Feed, id *frontendv1.FeedId) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	removed := r.retire(s, feed, id.GetValue())
	r.logger(ws).Debug("daemon.feed.retire_row",
		"a row was retired from its feed",
		dlog.Context{"feed": r.feedKey(ws, feed), "row": id.GetValue(), "found": removed})
}

// StandingFor answers a permission row's held standing token.
func (r *resolver) StandingFor(ws ids.WorkspaceID, id *frontendv1.FeedId) (*conversationv1.AgentPermissionStanding, bool) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	standing, ok := s.standing[id.GetValue()]
	return standing, ok
}

// ServedPermission answers what the daemon served for one permission ask: the
// agent that is blocked on it, and the standing token the vendor offered
// (nil when none was). It reports false when this workspace drew no such ask.
//
// The ANSWER VERB is the caller: a client sends back only the ask's identity,
// and the agent it must be delivered to lives nowhere else.
func (r *resolver) ServedPermission(ws ids.WorkspaceID, ask string) (*conversationv1.AgentId, *conversationv1.AgentPermissionStanding, bool) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	state, ok := s.permissionRows[ask]
	if !ok {
		return nil, nil, false
	}
	return state.agent, s.standing[state.row.GetValue()], true
}

// ServedQuestion answers what the daemon served for one question ask: the
// agent that is blocked on it and the batch as served. It reports false when
// this workspace drew no such ask.
func (r *resolver) ServedQuestion(ws ids.WorkspaceID, ask string) (*conversationv1.AgentId, *conversationv1.AgentQuestionBatch, bool) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	state, ok := s.questionAsks[ask]
	if !ok {
		return nil, nil, false
	}
	return state.agent, state.batch, true
}

// MintSubFeedHead records a bubble row's sub-feed and its crumb label.
func (r *resolver) MintSubFeedHead(ws ids.WorkspaceID, head *frontendv1.FeedId, sub feedid.Feed, label string) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	r.mintSubFeed(s, head, sub, label)
}

// mintSubFeed is MintSubFeedHead's body, called with the lock held.
func (r *resolver) mintSubFeed(s *wsState, head *frontendv1.FeedId, sub feedid.Feed, label string) {
	key := r.feedKey(s.id, sub)
	parentKey := "root"
	for existingKey, f := range s.feeds {
		if _, ok := f.rows[head.GetValue()]; ok {
			parentKey = existingKey
			break
		}
	}
	s.subFeeds[key] = &subFeedHead{row: head, parentFeed: parentKey, label: label}
	if sub.Agent != nil {
		s.agentFeeds[sub.Agent.GetValue()] = key
	}
	r.feed(s, sub)
	r.logger(s.id).Debug("daemon.feed.sub_feed",
		"a bubble row's sub-feed was recorded",
		dlog.Context{"feed": key, "head": head.GetValue(), "parent_feed": parentKey, "label": label})
}

// painter answers the injected painter, or a plain one when none was
// injected: an unpainted read is a drawable card, and refusing the whole row
// over a missing highlighter would lose the file the agent read.
func (r *resolver) painter() paint.Painter {
	return r.deps.Painter
}
