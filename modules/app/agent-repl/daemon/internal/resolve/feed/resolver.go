package feed

import (
	"context"
	"errors"
	"fmt"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"google.golang.org/protobuf/proto"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
	"claude-repld/internal/lockwatch"
	"claude-repld/internal/paint"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/wsm"
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

	mu         lockwatch.Mutex
	workspaces map[ids.WorkspaceID]*wsState
	// tokens is the whole watch-token table: minting here is what makes a
	// token opaque, foreign tokens detectable, and pinning exact.
	tokens map[string]*watchToken
	// tokenSeq mints token values.
	tokenSeq uint64
	// loggers caches each workspace's resolved durable logger.
	loggers map[ids.WorkspaceID]dlog.Logger
	// lineage is what each workspace's descent is known to be: whether it is a
	// fork, and which turns are its own (lineage.go). It has a mutex of its
	// own because the reads that fill it run outside mu.
	lineage lineages
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

	// mainAgent is the session's main agent, as the session watcher NAMED it
	// (OnMainAgent). Its rows are the root feed's; every other agent's rows are
	// on the sub-feed its spawn minted. An agent that is neither is
	// UNPLACEABLE: nothing is drawn for it, and the failure is reported loudly
	// rather than landing on the root — the root is the main agent's because it
	// IS the main agent, never as a default.
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
	// holdingPushes is set while a history page is being replayed: every
	// publication is held on its feed until the page is wholly placed
	// (resolver.holdPushes).
	holdingPushes bool

	// readers are the standing page walks, one per open connection.
	readers map[ReaderID]*walk

	// units is the per-activity accumulation: the start facts a settled frame
	// is drawn against, and the fold of a growing one.
	units map[string]*unitState
	// drawnRows is the FeedId each activity unit's row was last announced at
	// (Deps.ItemDrawn), so a redraw at the same address announces nothing.
	drawnRows map[string]string
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
	// liveMonitors is the monitors the live set held at its last publication,
	// keyed by unit. A monitor that leaves it with its card still running has
	// that card settled (settleMonitorsLeftLive).
	liveMonitors map[string]struct{}
	// heldDetachments are what a held detachment's announcement said about
	// whose work it is, by unit, kept so the claim can check it against the
	// agent that turns out to carry the unit, and so a detachment that never
	// finds its unit is reported with its full context.
	heldDetachments map[string]heldDetachment
	// announcedAgents are the agents a detachment announcement NAMED for a
	// unit that had drawn no bubble yet, by unit. The announcement's subagent
	// kind names the running agent (AgentDetachedWork.kind), and for a
	// subagent RESUMED BY SENDMESSAGE nothing else ever does: the unit is the
	// send, so no spawn start will arrive to name it. The bubble takes it when
	// it is first drawn (drawSubagent).
	announcedAgents map[string]*conversationv1.AgentId
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
	// replayCloses is the durable close of every turn the page being replayed
	// opens, read before the replay (recordedCloses). A replayed turn left with
	// no terminal of its own is ended from it (turnclosed.go). Nil outside a
	// replay.
	replayCloses map[ids.TurnID]wsm.RecordedClose
	// entryTurn is the STAMP of the entry being drawn (HistoryEntryAt.turn),
	// in force only while that entry is drawn (drawingEntry). Nil for an
	// unstamped entry, which is what selects the positional fallback.
	entryTurn *ids.TurnID
	// entryPlace is the CONVERSATION PLACE of the entry being drawn
	// (HistoryEntryAt.place), in force only while that entry is drawn
	// (placingEntry): what the order key of every row it first draws is
	// minted from (order.go). Nil for an entry whose serving side stated none,
	// and outside any entry.
	entryPlace *conversationv1.ConversationPlace
	// inEntry reports that an entry is being drawn at all, placed or not, so a
	// row an unplaced entry draws is recorded as the protocol's receipt-order
	// fallback rather than as a row the daemon made itself.
	inEntry bool
	// replayTail is, per feed key, the order key the replay in progress last
	// placed or restated in that feed: what a row the replay makes itself
	// follows (order.go). Emptied at the start of every replay.
	replayTail map[string]string
	// knownTurns is every turn this feed has seen OPENED: a prompt drawn for
	// it, or the daemon handing it over (entryturn.go).
	knownTurns map[ids.TurnID]bool
	// predatesPage is every turn a replay met at a page's head whose prompt is
	// older than the page, so its later entries are not judged again.
	predatesPage map[ids.TurnID]bool
	// unknownTurnsReported is every stamped turn already reported as naming a
	// prompt its book does not carry, so the ERROR is written once per turn.
	unknownTurnsReported map[ids.TurnID]bool
	// replayPromptDrawn, replayAtFloor and replayUnstamped describe the page
	// being replayed: whether it has drawn a main-agent prompt yet, whether it
	// claims to reach the book's floor, and how many of its entries carried no
	// stamp and were attributed by position.
	replayPromptDrawn bool
	replayAtFloor     bool
	replayUnstamped   int
	// inherited is the attribution a fork's inherited past is drawn under,
	// carried from one inherited entry to the next (lineage.go). It is kept
	// apart from the fork's own attribution so the copied conversation can
	// never become the turn the fork is running.
	inherited inheritedStance
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

	// liveEndings files each turn that ended LIVE with what its end said, for
	// the desktop banner (liveending.go). The prompt queue's turn end takes
	// each entry once.
	liveEndings map[ids.TurnID]TurnEnding

	// THE FRESH-INPUT ACCOUNTS (usage.go). unitAccount files each
	// usage-carrying unit in its agent's account — the main agent's turn or a
	// subagent's lifetime — and unitFresh is that unit's fresh input, replaced
	// on every restating frame.
	unitAccount map[string]string
	unitFresh   map[string]uint64
	// accountTally is each account's fresh input so far; accountStated reports
	// that the account stated any usage at all (absence draws no stamp).
	accountTally  map[string]uint64
	accountStated map[string]bool
	// accountLanded is each account's tally when its latest response bubble
	// LANDED: the base the account's next bubble counts its delta from.
	accountLanded map[string]uint64
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
	// planeInherited is the store's copy of the conversation a fork inherited
	// — its book's entries of turns the fork never opened — whenever it
	// arrives: on a page, or live while the copy is still being ingested
	// (lineage.go). Older than anything the fork produced, by construction.
	planeInherited
	// planeHistory is the store's own replayed history.
	planeHistory
	// planeLive is everything drawn as it happens.
	planeLive
)

// replayed reports whether rows drawn in this plane are a replay of settled
// history — a page, or a fork's inherited past — rather than the conversation
// as it happens.
func (p rowPlane) replayed() bool {
	return p == planeHistory || p == planeInherited
}

// inheritedPast reports whether rows drawn in this plane are the conversation
// a fork inherited from its parent — its ported prompts or the store's copy —
// and so precede everything the fork produced.
func (p rowPlane) inheritedPast() bool {
	return p == planePorted || p == planeInherited
}

// String names a plane for a log record. A row's plane is recorded beside its
// order key, which is the whole story of where it landed (order.go).
func (p rowPlane) String() string {
	switch p {
	case planePorted:
		return "ported"
	case planeInherited:
		return "inherited"
	case planeHistory:
		return "history"
	case planeLive:
		return "live"
	}
	return "unknown"
}

// rowRank is one row's place in its feed's order, fixed at its first draw.
type rowRank struct {
	// plane is the plane the row was first drawn in.
	plane rowPlane
	// key is the row's order key (order.go): what the feed sorts by, and what
	// every publication of the row carries as FeedRow.order.
	key string
}

// before reports whether this rank sorts ahead of other.
func (r rowRank) before(other rowRank) bool {
	return r.key < other.key
}

// feedState is one feed: its rows in first-appearance order, the publication
// log a tail replays from, and the subscribers following it.
type feedState struct {
	// key is the encoded feed address.
	key string
	// order is the row ids in ORDER-KEY order (order.go); an upsert never
	// moves a row.
	order []string
	// rank is each row's ordering key, kept so an insertion knows where the
	// row belongs among the rows already drawn.
	rank map[string]rowRank
	// rows is the current whole of each row.
	rows map[string]*frontendv1.FeedRow
	// nonDurable marks the rows that exist in resolver memory only and never
	// appear in a page.
	nonDurable map[string]bool
	// superseded marks the THINKING rows a later response row in this feed has
	// superseded (superseded.go): the record `upsert` stamps
	// FeedResponse.superseded from on every draw.
	superseded map[string]bool
	// entryRows is, per entry base, how many rows that entry has first drawn
	// in this feed: the next row's sub-index (order.go).
	entryRows map[string]uint32
	// followers is, per entry base, how many rows have been minted to follow
	// it (order.go).
	followers map[string]uint32
	// seq is the publication counter: every upsert publication takes the next
	// value, and a watch token pins to one.
	seq uint64
	// log is the retained publication log, oldest first.
	log []*loggedRow
	// retention caps the log.
	retention int
	// subs are the tails following this feed.
	subs map[*tailSub]struct{}
	// held are the publications a history replay is holding back from subs
	// until its page is wholly placed (resolver.holdPushes). Empty outside a
	// replay.
	held []*frontendv1.FeedRow
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

// walk is one reader's page position: the order key of the OLDEST row it has
// been served. Ephemeral, dropped on every open and on CloseReader.
type walk struct {
	// feedKey is the feed this walk is of.
	feedKey string
	// oldest is the order key of the oldest row served so far; nil when the
	// walk has been served nothing, which stands it at the feed's start.
	oldest *string
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
	if deps.Decode == nil {
		deps.Decode = feedid.Decode
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
	r := &resolver{
		deps:       deps,
		workspaces: map[ids.WorkspaceID]*wsState{},
		tokens:     map[string]*watchToken{},
		loggers:    map[ids.WorkspaceID]dlog.Logger{},
	}
	// ONE MUTEX FOR EVERY WORKSPACE'S FEED, so its stall is the run log's
	// record. It lives as long as the process and is never unwatched.
	if deps.Stalls != nil {
		deps.Stalls.Watch(&r.mu, stallLock, "", deps.Log.Global())
	}
	return r, nil
}

// stallLock is how the stall watchdog names the resolver's mutex.
const stallLock = "feed.resolver"

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
		drawnRows:            map[string]string{},
		responses:            map[string]*proseState{},
		thinking:             map[string]*proseState{},
		plans:                map[string]*planState{},
		planUnits:            map[string]*planState{},
		shells:               map[string]*shellState{},
		liveShells:           map[string]struct{}{},
		liveMonitors:         map[string]struct{}{},
		detachedUnits:        map[string]string{},
		heldDetachments:      map[string]heldDetachment{},
		announcedAgents:      map[string]*conversationv1.AgentId{},
		subagents:            map[string]*subagentState{},
		standing:             map[string]*conversationv1.AgentPermissionStanding{},
		permissionRows:       map[string]*permissionState{},
		questionAsks:         map[string]*questionState{},
		gatedCalls:           map[string]string{},
		turnEvidence:         map[string][]turnEvidenceLine{},
		turnRefusals:         map[string]bool{},
		clearTurns:           map[ids.TurnID]bool{},
		clearConfirmed:       map[ids.TurnID]bool{},
		clearedTurnByPointer: map[string]ids.TurnID{},
		directiveTurns:       map[ids.TurnID]bool{},
		knownTurns:           map[ids.TurnID]bool{},
		predatesPage:         map[ids.TurnID]bool{},
		unknownTurnsReported: map[ids.TurnID]bool{},
		directiveUnits:       map[string]bool{},
		answerRows:           map[string]*frontendv1.FeedId{},
		finalAnswerSeen:      map[string]bool{},
		endedTurns:           map[ids.TurnID]bool{},
		answerMarkdown:       map[string]string{},
		liveEndings:          map[ids.TurnID]TurnEnding{},
		stalls:               map[string]*stallState{},
		replayTail:           map[string]string{},

		unitAccount:   map[string]string{},
		unitFresh:     map[string]uint64{},
		accountTally:  map[string]uint64{},
		accountStated: map[string]bool{},
		accountLanded: map[string]uint64{},

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
		superseded: map[string]bool{},
		entryRows:  map[string]uint32{},
		followers:  map[string]uint32{},
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

// firstRank is the rank a row takes at its first draw: the rank of the row it
// replaces when the placement says so, a freshly minted key otherwise.
func (r *resolver) firstRank(s *wsState, f *feedState, id string, at placement) (rowRank, string) {
	if at.inherit != nil {
		// THE ROW TAKES THE PLACE OF THE ONE IT REPLACES, so a page walk
		// draws it where that row stood rather than below everything drawn
		// since.
		return *at.inherit, "inherited"
	}
	key, how := r.mintOrder(s, f, id)
	return rowRank{plane: s.plane, key: key}, how
}

// recordRepublication records that an already-placed row was published again
// to the tails following its feed, and why. A late or reordered row on screen
// is explained by these records, so every re-push a reader can see is at INFO
// — except the one that is the conversation happening: a live entry growing
// its own row (a streamed response, a tool's result), which is DEBUG because
// it is every frame of every turn.
func (r *resolver) recordRepublication(s *wsState, f *feedState, id string, rank rowRank) {
	if len(f.subs) == 0 {
		return
	}
	reason := "daemon_restated"
	switch {
	case s.plane.replayed() || s.plane == planePorted:
		reason = "history_replay"
	case s.inEntry:
		r.logger(s.id).Debug("daemon.feed.row_republished",
			"a live entry updated a row already placed; it was published to the feed's open tails",
			dlog.Context{"feed": f.key, "row": id, "key": rank.key, "reason": "live_entry", "tails": len(f.subs)})
		return
	}
	r.logger(s.id).Info("daemon.feed.row_republished",
		"a row already placed was published again to the feed's open tails",
		dlog.Context{"feed": f.key, "row": id, "key": rank.key, "reason": reason, "plane": s.plane.String(), "tails": len(f.subs)})
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
	// inherit, when set, is the ordering key a row TAKES OVER at its first
	// draw instead of minting a new one: a detached shell's head replacing the
	// running card it was, at that card's own position in the feed.
	inherit *rowRank
}

// place answers where an agent's rows go, and whether they can go anywhere.
// WHILE AN OUTPUT ADDRESS IS SET every row the session produces goes on the
// addressed feed under the addressed row; cleared, the main agent's rows go on
// the root and any other agent's on its own sub-feed.
//
// THERE IS NO FALLBACK. An agent that is neither the named main agent nor one
// whose spawn minted a sub-feed is UNPLACEABLE: the caller draws nothing, and
// the failure is logged at ERROR and raised on the topbar. It used to land on
// the root feed with a WARN — and that is how a subagent's background shell
// came to be drawn under the main agent's last answer.
func (r *resolver) place(s *wsState, agent *conversationv1.AgentId) (placement, bool) {
	if s.address != nil {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "s.address != nil"})
		return r.outputPlacement(s), true
	}
	id := agent.GetValue()
	feed, err := feedid.AgentFeed(id, s.mainAgent)
	if err == nil && feed.Agent != nil {
		if _, minted := s.agentFeeds[id]; !minted {
			err = errAgentFeedUnminted
		}
	}
	if err != nil {
		r.reportUnplaceableAgent(s, id, err)
		return placement{}, false
	}
	return placement{feed: feed}, true
}

// errAgentFeedUnminted is why an agent that is not the main one has no feed:
// no spawn this resolver drew ever created it.
var errAgentFeedUnminted = errors.New("feed: no drawn spawn created this agent, so it has no sub-feed")

// reportUnplaceableAgent records, loudly and once per frame, that an agent's
// row could not be placed and was not drawn, and puts it on the topbar.
func (r *resolver) reportUnplaceableAgent(s *wsState, agent string, cause error) {
	r.logger(s.id).Error("daemon.feed.unplaceable_agent",
		"a frame arrived for an agent that has no feed; nothing was drawn for it",
		dlog.Context{"agent": agent, "main_agent": s.mainAgent, "cause": cause.Error()})
	r.raiseWarning(s, "unplaceable_agent:"+agent,
		fmt.Sprintf("rows of agent %q could not be placed in any feed and were not drawn", agent))
}

// raiseWarning puts a resolution failure on the topbar's warning chip, the
// webapp's one error surface. The caller has already logged it with its full
// context; nil Warnings leaves the log as the only record, which is what a
// test that is not about the topbar wants.
func (r *resolver) raiseWarning(s *wsState, key, line string) {
	if r.deps.Warnings == nil {
		return
	}
	r.deps.Warnings.RaiseWarning(s.id, key, line)
}

// outputPlacement is where a row the SESSION ITSELF draws belongs — a person's
// prompt, a clear's divider, a turn's terminal the daemon composes — rather
// than any agent's work: the standing output address, or the root feed, which
// is the session's own, when none stands.
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
//
// A row drawn at a MIRRORED output address is drawn on the root feed too
// (mirror.go), through this same path, so the copy is ordered and published
// exactly as every other row is.
func (r *resolver) upsert(s *wsState, at placement, row *frontendv1.FeedRow, durable bool) {
	r.upsertOne(s, at, row, durable)
	if copy, ok := r.mirrorRow(s, at.feed, row); ok {
		r.upsertOne(s, placement{feed: feedid.Feed{Root: true}}, copy, durable)
	}
}

// upsertOne is upsert on one feed.
func (r *resolver) upsertOne(s *wsState, at placement, row *frontendv1.FeedRow, durable bool) {
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
	// A THINKING ROW'S `superseded` IS STATED HERE TOO, from the feed's own
	// record, so every draw of the fold restates it (superseded.go).
	stampSuperseded(f, id, snapshot)
	existing, seen := f.rows[id]
	// THE ROW'S ORDER KEY IS FIXED AT ITS FIRST DRAW and stated on every
	// publication of it (order.go), so it is settled before the snapshot is
	// compared with what was published last.
	rank, how := f.rank[id], "restated"
	if !seen {
		rank, how = r.firstRank(s, f, id, at)
	}
	r.stampOrder(s, f, id, snapshot, rank.key)
	if s.plane != planeLive {
		s.replayTail[f.key] = rank.key
	}
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
	if seen {
		r.recordRepublication(s, f, id, rank)
	}
	if !seen {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "!seen"})
		f.insert(id, rank)
		// THE ORDERING TRACE. A row's order key is fixed HERE, at first draw,
		// and the feed sorts by it forever after — so this one line per row is
		// the whole account of why any row landed where it did relative to
		// every other: `how` says whether the key came from the row's entry's
		// conversation place, from following the row before it, or from the
		// row it replaced. At INFO so it survives at normal verbosity; the row
		// id encodes the feed, kind and key, and the turn ties it to its turn.
		r.logger(s.id).Info("daemon.feed.row_placed",
			"a row was placed in the feed order at first draw",
			dlog.Context{
				"feed":  f.key,
				"row":   id,
				"plane": rank.plane.String(),
				"key":   rank.key,
				"how":   how,
				"index": f.indexOf(id),
				"seq":   f.seq,
				"turn":  row.GetTurn().GetValue(),
			})
		// A RESPONSE ROW'S PLACEMENT SUPERSEDES THE THINKING ROW BEFORE IT. The
		// earlier row is re-pushed AFTER this row's own publication, so a reader
		// sees the new response land and then the thinking above it collapse,
		// and the two publications take sequences in that order.
		if isResponseRow(snapshot) {
			if earlier := r.supersedeOnPlace(s, f, id, snapshot); earlier != "" {
				defer r.republishSuperseded(s, at.feed, earlier)
			}
		}
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
	r.publish(s, f, snapshot)
}

// publish hands one publication — an upsert's snapshot or a removal — to the
// feed's tails. It is the ONE door every publication leaves by.
//
// A HISTORY REPLAY HOLDS ITS PUBLICATIONS until its page is wholly placed
// (holdPushes). A page is drawn oldest first and the delivery bound is read off
// the order, so a page published row by row served every row above its newest
// cut BEFORE the cut that withholds it had been drawn: the reader drew each
// one, then took the cut and truncated them all. Held, the page is published
// once the order holds its newest cut, and every row that cut withholds is
// kept off the wire by the same rule a page walk reads.
func (r *resolver) publish(s *wsState, f *feedState, row *frontendv1.FeedRow) {
	if s.holdingPushes {
		f.held = append(f.held, row)
		return
	}
	r.push(s, f, row)
}

// push enqueues one publication on every tail following the feed, unless it
// is an upsert the delivery rules keep off the wire (withheldFromPush). A
// removal always goes: it retracts, and a reader that never held the row
// drops nothing.
func (r *resolver) push(s *wsState, f *feedState, row *frontendv1.FeedRow) {
	if row.GetRemoved() == nil && r.withheldFromPush(s, f, row.GetId().GetValue()) {
		return
	}
	for sub := range f.subs {
		sub.enqueue(row)
	}
}

// holdPushes starts holding this workspace's publications for a history
// replay, and answers the release the replay defers. The release publishes
// every held row in the order it was drawn, judged against the order AS IT
// NOW STANDS: a row the page's own later cut withholds never reaches a tail,
// and a row the page retired again is not re-published (its removal, held
// behind it, is).
func (r *resolver) holdPushes(s *wsState) func() {
	s.holdingPushes = true
	return func() {
		s.holdingPushes = false
		for _, f := range s.feeds {
			held := f.held
			f.held = nil
			for _, row := range held {
				if _, stands := f.rows[row.GetId().GetValue()]; row.GetRemoved() == nil && !stands {
					continue
				}
				r.push(s, f, row)
			}
		}
	}
}

// withheldFromPush reports whether an upsert of ID must be kept off the live
// push. It stays stored (order, rank, log) so a page still serves it where
// it belongs; it only never reaches a tail.
//
// THE DELIVERY BOUND GOVERNS THE PUSH, NOT JUST THE PAGE. A separation that cut
// context withholds every row that sorts ABOVE it: a page walk clamps to it
// (see pages.go), and the live push must too. Without this a row first drawn
// AFTER the divider yet sorting above it — a file-plane history row the
// sidecar forwards late — would be pushed to every tail, and a reader would
// draw a row the cut ended back above the divider.
//
// A FORK'S INHERITED PAST IS NEVER PUSHED (lineage.go): it sorts above every
// row the fork has of its own, and an inherited cut pushed live would truncate
// the fork's own conversation off the reader's screen. Pages serve it where it
// belongs.
func (r *resolver) withheldFromPush(s *wsState, f *feedState, id string) bool {
	if rank, ok := f.rank[id]; ok && rank.plane == planeInherited {
		r.logger(s.id).Debug("daemon.feed.push_withheld_inherited",
			"a row of a fork's inherited past was stored but kept off the live push; pages serve it",
			dlog.Context{"feed": f.key, "row": id})
		return true
	}
	return r.pushWithheldByBound(s, f, id)
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
	retired := r.retireOne(s, addr, id)
	if mirrored, ok := r.mirrorID(s, addr, id); ok {
		r.retireOne(s, feedid.Feed{Root: true}, mirrored)
	}
	return retired
}

// retireOne is retire on one feed.
func (r *resolver) retireOne(s *wsState, addr feedid.Feed, id string) bool {
	f := r.feed(s, addr)
	if _, ok := f.rows[id]; !ok {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "_, ok := f.rows[id]; !ok"})
		return false
	}
	wasResponse := isResponseRow(f.rows[id])
	key := f.rank[id].key
	delete(f.rows, id)
	delete(f.nonDurable, id)
	delete(f.rank, id)
	delete(f.superseded, id)
	at := -1
	for i, existing := range f.order {
		if existing == id {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "existing == id"})
			f.order = append(f.order[:i], f.order[i+1:]...)
			at = i
			break
		}
	}
	// A RETIRED RESPONSE MAY HAVE BEEN THE ONLY ONE AFTER A THINKING ROW, which
	// is then the feed's latest response again; it is re-pushed after the
	// removal's own publication (superseded.go).
	if wasResponse && at >= 0 {
		if earlier := r.unsupersedeOnRetire(s, f, id, at); earlier != "" {
			defer r.republishSuperseded(s, addr, earlier)
		}
	}
	// PUBLISH THE REMOVAL, exactly as upsert publishes a change: mint a
	// sequence, append it to the retained log so a tail that connects later
	// replays it, and enqueue it to every connected tail so an open feed
	// updates NOW. The removal row carries ONLY its id — the arm's presence is
	// the instruction, and the client drops the row that id keys. It carries
	// the removed row's order key, as every row does.
	removal := removalRow(id, key)
	f.seq++
	r.logger(s.id).Info("daemon.feed.row_retired",
		"a row was retired and its removal published on the feed tail",
		dlog.Context{"feed": f.key, "row": id, "key": key, "seq": f.seq})
	f.log = append(f.log, &loggedRow{seq: f.seq, row: removal})
	if len(f.log) > f.retention {
		f.log = f.log[len(f.log)-f.retention:]
	}
	r.publish(s, f, removal)
	return true
}

// removalRow is the publication that retracts row ID, whose order key was KEY.
func removalRow(id, key string) *frontendv1.FeedRow {
	return &frontendv1.FeedRow{
		Id:    &frontendv1.FeedId{Value: id},
		Order: &frontendv1.FeedRowOrder{Key: key},
		Row:   &frontendv1.FeedRow_Removed{Removed: &frontendv1.FeedRowRemoved{}},
	}
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
func (r *resolver) MintSubFeedHead(ws ids.WorkspaceID, head *frontendv1.FeedId, headFeed feedid.Feed, sub feedid.Feed, label string) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	r.mintSubFeed(s, head, headFeed, sub, label)
}

// mintSubFeed is MintSubFeedHead's body, called with the lock held.
//
// THE PARENT IS THE FEED THE HEAD IS DRAWN ON, STATED BY THE CALLER. It used to
// be searched for among the rows already placed, which a subagent bubble mints
// BEFORE its row is upserted — so a first compose found nothing and recorded
// the ROOT as the parent of a bubble drawn on another subagent's sub-feed. The
// crumb chain of a subagent of a subagent then stopped one level short, and a
// footer jump to it walked from the root into a crumb the root does not hold.
func (r *resolver) mintSubFeed(s *wsState, head *frontendv1.FeedId, headFeed feedid.Feed, sub feedid.Feed, label string) {
	key := r.feedKey(s.id, sub)
	parentKey := r.feedKey(s.id, headFeed)
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

// A MIRRORED OUTPUT ADDRESS (wsm.OutputAddress.Mirror) draws every row that
// lands at it ALSO on the root feed, as an ordinary row of the conversation.
// It is how a merge's repair turns -- the workspace's own session resolving a
// conflict or fixing a suite -- appear both in the merge bubble's tab and in
// the main feed. The copy is a second resolved row with its own identity (the
// same row key, addressed on the root feed), never one row the client fans
// out: a row nested directly under the addressed parent (the tab) is a
// top-level row on the root feed, and a row nested under another row of the
// addressed feed is nested under that row's own root copy.

// mirrorRow answers the root feed's copy of a row drawn on feed, and false
// when the row is not drawn at a mirrored output address. A row whose identity
// cannot be re-keyed is recorded at ERROR and not mirrored: the tab still has
// it, and the main feed is missing it loudly rather than holding a guess.
func (r *resolver) mirrorRow(s *wsState, feed feedid.Feed, row *frontendv1.FeedRow) (*frontendv1.FeedRow, bool) {
	id, ok := r.mirrorID(s, feed, row.GetId().GetValue())
	if !ok {
		return nil, false
	}
	copy, cloned := proto.Clone(row).(*frontendv1.FeedRow)
	if !cloned {
		r.logger(s.id).Error("daemon.feed.mirror", "a mirrored row could not be copied onto the root feed",
			dlog.Context{"row": row.GetId().GetValue()})
		return nil, false
	}
	copy.Id = &frontendv1.FeedId{Value: id}
	// THE COPY IS ORDERED ON THE ROOT FEED ITSELF: a restated row carries the
	// addressed feed's order key, which is no key of the root's.
	copy.Order = nil
	copy.Parent = nil
	if parent := row.GetParent().GetRow().GetValue(); parent != "" && !r.isAddressParent(s, parent) {
		mirroredParent, ok := r.mirrorID(s, feed, parent)
		if !ok {
			return nil, false
		}
		copy.Parent = &frontendv1.FeedRowParent{Row: &frontendv1.FeedId{Value: mirroredParent}}
	}
	return copy, true
}

// mirrorID answers the root feed's identity for a row id drawn on feed, and
// false when feed is not a mirrored output address.
func (r *resolver) mirrorID(s *wsState, feed feedid.Feed, id string) (string, bool) {
	if s.address == nil || !s.address.Mirror || feed.Root {
		return "", false
	}
	if r.feedKey(s.id, feed) != r.feedKey(s.id, s.address.Feed) {
		return "", false
	}
	ref, err := r.deps.Decode(&frontendv1.FeedId{Value: id})
	if err != nil {
		r.logger(s.id).Error("daemon.feed.mirror", "a row drawn at a mirrored output address could not be re-keyed onto the root feed; the main feed does not show it",
			dlog.Context{"row": id, "cause": err.Error()})
		return "", false
	}
	ref.Feed = feedid.Feed{Root: true}
	mirrored := r.deps.Encode(ref).GetValue()
	if mirrored == "" {
		r.logger(s.id).Error("daemon.feed.mirror", "a mirrored row's root identity encoded to nothing; the main feed does not show it",
			dlog.Context{"row": id})
		return "", false
	}
	return mirrored, true
}

// isAddressParent reports whether a parent id is the output address's own
// parent row (the merge tab the rows nest under), which a root copy drops.
func (r *resolver) isAddressParent(s *wsState, parent string) bool {
	if s.address.Parent == nil {
		return false
	}
	return r.deps.Encode(*s.address.Parent).GetValue() == parent
}
