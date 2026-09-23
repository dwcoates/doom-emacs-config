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

	// plane is the ORDERING PLANE rows are currently being drawn into. It is
	// planeLive except while a history page is being replayed, and it is what
	// keeps a fork's ported conversation above the rows the fork draws for
	// itself no matter which of the two arrives first.
	plane rowPlane
	// portedDrawn records that this workspace's ported conversation has been
	// drawn, so it is replayed once rather than on every agent's page.
	portedDrawn bool

	// readers are the standing page walks, one per open connection.
	readers map[ReaderID]*walk

	// units is the per-activity accumulation: the start facts a settled frame
	// is drawn against, and the fold of a growing one.
	units map[string]*unitState
	// responses is the prose fold, keyed by activity id.
	responses map[string]*proseState
	// thinking is the reasoning fold, keyed by activity id. Kept SEPARATE from
	// responses so the response-only logic (directive suppression, answer-row
	// filing) can never reach a thinking fold: a thinking bubble is never the
	// turn's answer and is never suppressed as a directive's empty prose.
	thinking map[string]*proseState
	// plans is the agent's current plan episode, keyed by agent id. It is the
	// OPEN one, or the last one once that has settled.
	plans map[string]*planState
	// planUnits attributes a plan-mode call to its episode, keyed by activity
	// id, so the same call re-delivered is never taken for a new episode.
	planUnits map[string]*planState
	// shells is the detached-shell accumulation, keyed by detached work id.
	shells map[string]*shellState
	// liveShells is the detached shells the session watcher's live set held at
	// its last publication, keyed by work id. A shell that leaves it without
	// having settled is settled lost (settleShellsLeftLive).
	liveShells map[string]struct{}
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
	// replayTurn is the turn a HISTORY REPLAY is standing in: the turn of the
	// last main-agent prompt THIS PAGE drew, cleared by the terminal that ends
	// it and at both ends of every page. A replayed terminal is charged to this
	// and never to turnInFlight, because the page's head can open mid-turn: its
	// oldest terminals end turns whose prompts are older than the page, and the
	// live turn the queue just opened is not one of them. Charging that orphan
	// to turnInFlight drew a turn_ended row for a turn that had only begun
	// (every turn open repaints the opening page) and settled its prompt.
	replayTurn *ids.TurnID
	// clearTurns is the set of turns the daemon opened as a `/clear`. A clear's
	// visible outcome is the cleared divider it leaves, NOT a terminal row: the
	// turn is interrupted to make the cut, and drawing that interrupt as a
	// bubble below the divider is the "response cut short" card a clear must
	// never leave. Membership is what lets drawTerminal suppress that bubble and
	// lets drawContextCut recognise the cut as the confirmation of the optimistic
	// red bar the receipt path drew.
	clearTurns map[ids.TurnID]bool
	// clearConfirmed records that a clear turn's ContextCut(Cleared) actually
	// arrived — the clear SUCCEEDED. Only a confirmed clear suppresses its
	// terminal; a clear that never confirmed FAILED, and its failure is surfaced
	// loudly while its optimistic red bar is retired.
	clearConfirmed map[ids.TurnID]bool
	// clearedTurnByPointer maps a confirmed clear cut's store pointer back to the
	// turn it belongs to, so the SECOND plane's delivery of the same cut — which
	// can land after the turn's terminal, with no turn in flight — still resolves
	// to the one turn-keyed divider row rather than minting a second red bar.
	clearedTurnByPointer map[string]ids.TurnID
	// directiveTurns is the set of turns the daemon opened as a context-cut
	// DIRECTIVE — /clear or /compact. A directive is not a conversational prompt,
	// so it draws no user-prompt bubble AND no response bubble: its only visible
	// outcome is the separation bar and the feed it clears. The set is what lets
	// drawAgentPrompt and drawResponse skip the prompt/response frames the shim
	// emits for the directive turn.
	//
	// IT IS NEVER FORGOTTEN once set. Each store plane delivers the directive's
	// frames independently, and the file plane's copy can land AFTER the turn's
	// terminal has cleared the in-flight fact; a set that dropped the turn at the
	// terminal let that late copy draw a stale bubble below the bar (the owner's
	// "/clear renders after a later prompt").
	directiveTurns map[ids.TurnID]bool
	// directiveUnits is the set of response units a directive turn produced, so a
	// LATE re-delivery of that response — after the terminal cleared the in-flight
	// turn, when the unit can no longer be attributed to its directive turn — is
	// suppressed too.
	directiveUnits map[string]bool
	// answerRows maps a response activity id to the row it drew, so the turn's
	// conclusion can name its answering row.
	answerRows map[string]*frontendv1.FeedId

	// finalAnswers is the ORDERED list of this workspace's root-feed
	// final-response rows — the rows a concluded turn named as its answer, the
	// ones the webapp draws with the GREEN final-answer border. It is the
	// selectable set reply-to-a-past-response mode walks (SelectResponse), in
	// feed order: a turn concludes after its rows are drawn, so conclusion
	// order IS root-feed order. Subagent terminals never reach the conclusion
	// site (drawTerminal returns early when turn is nil), so only the main
	// turn's answers land here.
	finalAnswers []*frontendv1.FeedId
	// finalAnswerSeen dedupes finalAnswers by FeedId value: a turn's terminal
	// replays across planes (history then live, the file plane after the
	// stream plane), and the same answer row must be appended once, not once
	// per replay.
	finalAnswerSeen map[string]bool
	// endedTurns is every turn this resolver has seen END — its terminal drawn
	// (or suppressed, for a confirmed /clear), its query died, or its prompt
	// ported from a parent workspace where it had already ended. It is the one
	// input to a prompt row's `working` flag (stampPromptWorking): a prompt
	// works exactly while its turn is absent from this set. Never unlearned —
	// turn ids are unique, and no turn ever resumes after its terminal.
	endedTurns map[ids.TurnID]bool
	// answerFault is the ONE standing `final_answer_unresolved` fault, nil when
	// none stands. It is raised at a terminal whose answer did not land, or by
	// an open response fold's stall window elapsing, and retracted when the next
	// turn starts (or, for the stall, by the frame that finally arrived).
	answerFault *answerFaultState
	// stalls are the armed stall windows, keyed by the response unit.
	stalls map[string]*stallState
	// stallSeq numbers the armed windows so a timer that fired and is waiting
	// on the resolver's mutex can tell it is no longer the armed one.
	stallSeq uint64

	// answerMarkdown copies each final-response row's settled markdown, keyed
	// by its FeedId value, so a reply-to-a-past-response submission can PREPEND
	// the referenced response verbatim without re-walking the fold. It holds
	// only the selectable finals, so a lookup that misses is a feedid the
	// daemon does not deem selectable — the submit path's refusal, never a
	// silent empty prefix.
	answerMarkdown map[string]string

	// apiResponseSeq numbers the API responses observed so far; a unit
	// arriving with usage opens the next one.
	apiResponseSeq uint64
	// unitAPIResponse files each unit under the API response it arrived in.
	unitAPIResponse map[string]uint64
	// apiResponseTurn records the turn each API response was filed under, so a
	// response bubble's stamp can sum ONLY its own turn's responses. Keyed by
	// the response's number; the empty string is "no turn in flight" (units
	// seen before any prompt).
	apiResponseTurn map[uint64]string
	// apiResponseTurnTokens is each API response's TURN-SCOPED token count —
	// fresh input plus output, cached context excluded (see usage.go) — keyed
	// by the response's number. Absent means that response stated no usage.
	apiResponseTurnTokens map[uint64]uint64
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

// THE FEED'S ORDERING KEY IS THE PLANE A ROW WAS DRAWN IN, then the order it
// was drawn within that plane. It is not bare arrival order, and the reason is
// a FORK: a forked workspace's feed carries three things — the parent's
// conversation the fork ported, the store's replay of the ported transcript,
// and the rows the fork draws as it runs — and the first two arrive by routes
// that race the third. Ordered by arrival alone, one run put the parent's
// answer above the fork's first question and the next run put it below.
//
// Within a plane the order is still first appearance, and an upsert still
// never moves a row: a plane is decided when a row is FIRST drawn.
type rowPlane uint8

const (
	// planePorted is a fork's ported parent conversation: older than anything
	// this workspace has of its own, by construction.
	planePorted rowPlane = iota
	// planeHistory is the store's own replayed history.
	planeHistory
	// planeLive is everything drawn as it happens.
	planeLive
)

// String names a plane for a log record. The feed orders by plane THEN seq, so
// a row's plane and seq are the whole story of where it landed relative to
// every other row — which is what a "why did this row sort here" trace needs.
func (p rowPlane) String() string {
	switch p {
	case planePorted:
		return "ported"
	case planeHistory:
		return "history"
	case planeLive:
		return "live"
	}
	return "unknown"
}

// rowRank is one row's place in its feed's order.
type rowRank struct {
	plane rowPlane
	// seq is the publication sequence the row was FIRST drawn at, which
	// orders rows within one plane.
	seq uint64
}

// before reports whether this rank sorts ahead of other.
func (r rowRank) before(other rowRank) bool {
	if r.plane != other.plane {
		return r.plane < other.plane
	}
	return r.seq < other.seq
}

// feedState is one feed: its rows in first-appearance order, the publication
// log a tail replays from, and the subscribers following it.
type feedState struct {
	// key is the encoded feed address.
	key string
	// order is the row ids in ORDERING-KEY order (see rowPlane); an upsert
	// never moves a row.
	order []string
	// rank is each row's ordering key, kept so an insertion knows where the
	// row belongs among the rows already drawn.
	rank map[string]rowRank
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
	if deps.AfterFunc == nil {
		deps.AfterFunc = func(d time.Duration, f func()) Timer { return time.AfterFunc(d, f) }
	}
	if deps.AnswerStall <= 0 {
		deps.AnswerStall = DefaultAnswerStall
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
		l.Debug("daemon.feed.row_decision", "selected the cached workspace logger", dlog.Context{
			"function": "logger", "workspace": string(ws), "cached": true,
		})
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
		r.logger(ws).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "ok"})
		return s
	}
	s = newWSState(ws)
	r.workspaces[ws] = s
	return s
}

// newWSState is the ONE place a workspace's feed state is born, so a workspace
// seen for the first time and a workspace RESET to empty (see reset.go) start
// from the same zero. A field added to wsState is initialised here and is
// therefore dropped by a reset without that reset having to name it — which is
// what makes "the feed is as empty as one never opened" hold as the state grows.
func newWSState(ws ids.WorkspaceID) *wsState {
	return &wsState{
		id:                   ws,
		feeds:                map[string]*feedState{},
		feedAddrs:            map[string]feedid.Feed{},
		subFeeds:             map[string]*subFeedHead{},
		agentFeeds:           map[string]string{},
		readers:              map[ReaderID]*walk{},
		units:                map[string]*unitState{},
		responses:            map[string]*proseState{},
		thinking:             map[string]*proseState{},
		plans:                map[string]*planState{},
		planUnits:            map[string]*planState{},
		shells:               map[string]*shellState{},
		liveShells:           map[string]struct{}{},
		detachedUnits:        map[string]string{},
		subagents:            map[string]*subagentState{},
		standing:             map[string]*conversationv1.AgentPermissionStanding{},
		permissionRows:       map[string]*permissionState{},
		questionAsks:         map[string]*questionState{},
		gatedCalls:           map[string]string{},
		turnEvidence:         map[string][]turnEvidenceLine{},
		turnRefusals:         map[string]bool{},
		turnQueryDeaths:      map[string]*conversationv1.SessionQueryDied{},
		clearTurns:           map[ids.TurnID]bool{},
		clearConfirmed:       map[ids.TurnID]bool{},
		clearedTurnByPointer: map[string]ids.TurnID{},
		directiveTurns:       map[ids.TurnID]bool{},
		directiveUnits:       map[string]bool{},
		answerRows:           map[string]*frontendv1.FeedId{},
		finalAnswerSeen:      map[string]bool{},
		endedTurns:           map[ids.TurnID]bool{},
		answerMarkdown:       map[string]string{},
		stalls:               map[string]*stallState{},

		unitAPIResponse:       map[string]uint64{},
		apiResponseTurn:       map[uint64]string{},
		apiResponseTurnTokens: map[uint64]uint64{},

		plane: planeLive,
	}
}

// feed resolves one feed's state within a workspace, creating it on first
// sight and remembering its address for breadcrumbs.
func (r *resolver) feed(s *wsState, addr feedid.Feed) *feedState {
	key := r.feedKey(s.id, addr)
	f, ok := s.feeds[key]
	if ok {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "ok"})
		return f
	}
	f = &feedState{
		key:        key,
		rows:       map[string]*frontendv1.FeedRow{},
		rank:       map[string]rowRank{},
		nonDurable: map[string]bool{},
		retention:  r.deps.TailRetention,
		subs:       map[*tailSub]struct{}{},
	}
	s.feeds[key] = f
	s.feedAddrs[key] = addr
	return f
}

// insert files a row's id at the place its rank names.
//
// THE SCAN IS BACKWARD because the ordinary case is a live row after every row
// already drawn, which the first comparison settles. Only a plane that arrives
// late — a history page replayed into a feed that already carries live rows —
// walks any distance, and it walks it once per row.
func (f *feedState) insert(id string, rank rowRank) {
	f.rank[id] = rank
	at := len(f.order)
	for at > 0 && rank.before(f.rank[f.order[at-1]]) {
		at--
	}
	f.order = append(f.order, "")
	copy(f.order[at+1:], f.order[at:])
	f.order[at] = id
}

// feedKey renders a feed address as the string this resolver keys by.
func (r *resolver) feedKey(ws ids.WorkspaceID, addr feedid.Feed) string {
	id := r.deps.EncodeFeed(ws, addr)
	if id.GetValue() != "" {
		r.logger(ws).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "id.GetValue() != \"\""})
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
	case addr.Shell != nil:
		return "shell:" + string(*addr.Shell)
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
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "s.address != nil"})
		return r.outputPlacement(s)
	}
	id := agent.GetValue()
	if id == "" || id == s.mainAgent {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "id == \"\" || id == s.mainAgent"})
		if s.mainAgent == "" {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "s.mainAgent == \"\""})
			s.mainAgent = id
		}
		return placement{feed: feedid.Feed{Root: true}}
	}
	if s.mainAgent == "" {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "s.mainAgent == \"\""})
		s.mainAgent = id
		return placement{feed: feedid.Feed{Root: true}}
	}
	if _, ok := s.agentFeeds[id]; ok {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "_, ok := s.agentFeeds[id]; ok"})
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
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "s.address == nil"})
		return placement{feed: feedid.Feed{Root: true}}
	}
	p := placement{feed: s.address.Feed}
	if s.address.Parent != nil {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "s.address.Parent != nil"})
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
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "at.parent != nil && row.Parent == nil"})
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
	// A PROMPT ROW'S `working` IS STATED HERE, on the one path every producer's
	// row takes — the resolver's own draws and the prompt queue's mirror alike —
	// so no producer can publish a prompt that disagrees with the turn's
	// lifecycle.
	stampPromptWorking(s, snapshot)
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
	f.seq++
	if !seen {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "!seen"})
		f.insert(id, rowRank{plane: s.plane, seq: f.seq})
		// THE ORDERING TRACE. A row's plane and seq are fixed HERE, at first
		// draw, and the feed sorts by (plane, seq) forever after — so this one
		// line per row is the whole account of why any row landed where it did
		// relative to every other (e.g. a directive prompt re-delivered on the
		// live plane after a later prompt sorts AFTER it, by a higher seq in the
		// same plane). At INFO so it survives at normal verbosity; the row id
		// encodes the feed, kind and key, and the turn ties it to its turn.
		r.logger(s.id).Info("daemon.feed.row_placed",
			"a row was placed in the feed order at first draw",
			dlog.Context{
				"feed":  f.key,
				"row":   id,
				"plane": s.plane.String(),
				"seq":   f.seq,
				"turn":  row.GetTurn().GetValue(),
			})
	}
	f.rows[id] = snapshot
	if !durable {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "!durable"})
		f.nonDurable[id] = true
	}
	f.log = append(f.log, &loggedRow{seq: f.seq, row: snapshot})
	if len(f.log) > f.retention {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "len(f.log) > f.retention"})
		f.log = f.log[len(f.log)-f.retention:]
	}
	// THE DELIVERY BOUND GOVERNS THE PUSH, NOT JUST THE PAGE. A separation
	// that cut context withholds every row that sorts ABOVE it: a page walk
	// clamps to it (see pages.go), and the live push must too. Without this a
	// row first drawn AFTER the divider yet sorting above it — a file-plane
	// history row the sidecar forwards late — would be pushed to every tail,
	// which appends by arrival and so lands it BELOW the divider on screen,
	// where nothing retracts it. It stays stored (order, rank, log) so a later
	// walk still orders it correctly; it is only kept off the wire.
	if r.pushWithheldByBound(s, f, id) {
		return
	}
	for sub := range f.subs {
		sub.enqueue(snapshot)
	}
}

// pushWithheldByBound reports whether ID is hidden by the feed's newest
// context-cutting separation and must therefore be kept off the live push. The
// separation itself, and every row the bound does not hide, always pushes. It
// shares the ONE rule pages.go's deliverable uses (boundHides), so the live push
// and the page can never disagree about which rows the bound withholds — the same
// rule that keeps a reconnect's replayed post-cut conversation on the wire
// rather than blanking it.
func (r *resolver) pushWithheldByBound(s *wsState, f *feedState, id string) bool {
	bi := boundIndex(f, f.order)
	if bi < 0 {
		return false
	}
	if !boundHides(f.rank[id], f.rank[f.order[bi]]) {
		return false
	}
	r.logger(s.id).Info("daemon.feed.push_withheld",
		"a row hidden by the newest context-cut divider was stored but kept off the live push",
		dlog.Context{"feed": f.key, "row": id, "bound": f.order[bi]})
	return true
}

// retire removes a row from a feed and publishes that removal on the feed's
// tail. A retired row stops appearing in pages, AND — the DUAL of upsert — a
// removal row is pushed to every connected tail so an already-open feed drops
// the row live rather than showing it stale until the next reload (the
// foreground Bash that detaches, whose running tool card is retired in favor
// of the shell bubble). The tail's already-delivered upserts of the row are
// history; the removal is a new publication that supersedes them.
func (r *resolver) retire(s *wsState, addr feedid.Feed, id string) bool {
	f := r.feed(s, addr)
	if _, ok := f.rows[id]; !ok {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "_, ok := f.rows[id]; !ok"})
		return false
	}
	delete(f.rows, id)
	delete(f.nonDurable, id)
	delete(f.rank, id)
	for i, existing := range f.order {
		if existing == id {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "existing == id"})
			f.order = append(f.order[:i], f.order[i+1:]...)
			break
		}
	}
	// PUBLISH THE REMOVAL, exactly as upsert publishes a change: mint a
	// sequence, append it to the retained log so a tail that connects later
	// replays it, and enqueue it to every connected tail so an open feed
	// updates NOW. The removal row carries ONLY its id — the arm's presence is
	// the instruction, and the client drops the row that id keys.
	removal := &frontendv1.FeedRow{
		Id:  &frontendv1.FeedId{Value: id},
		Row: &frontendv1.FeedRow_Removed{Removed: &frontendv1.FeedRowRemoved{}},
	}
	f.seq++
	r.logger(s.id).Info("daemon.feed.row_retired",
		"a row was retired and its removal published on the feed tail",
		dlog.Context{"feed": f.key, "row": id, "seq": f.seq})
	f.log = append(f.log, &loggedRow{seq: f.seq, row: removal})
	if len(f.log) > f.retention {
		f.log = f.log[len(f.log)-f.retention:]
	}
	for sub := range f.subs {
		sub.enqueue(removal)
	}
	return true
}

// rowID composes a row's identity from the address of what it DRAWS.
func (r *resolver) rowID(ws ids.WorkspaceID, addr feedid.Feed, key feedid.RowKey) *frontendv1.FeedId {
	id := r.deps.Encode(feedid.Ref{WS: ws, Feed: addr, Row: key})
	if id.GetValue() != "" {
		r.logger(ws).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "id.GetValue() != \"\""})
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
		r.logger(ws).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "addr != nil"})
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
		r.logger(ws).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "addSupport"})
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
		r.logger(ws).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "!ok"})
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
		r.logger(ws).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "!ok"})
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
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "_, ok := f.rows[head.GetValue()]; ok"})
			parentKey = existingKey
			break
		}
	}
	s.subFeeds[key] = &subFeedHead{row: head, parentFeed: parentKey, label: label}
	if sub.Agent != nil {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "sub.Agent != nil"})
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
