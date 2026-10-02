// Package feed is the feed resolver: row synthesis, prose folding, the output
// address, pages and tail tokens.
//
// It holds in-memory accumulation per workspace and publishes only COMPLETE
// rows. Page walks are per READER (per open connection), never persisted:
// a fresh or re-attached webview lands at the tail and pages back. See
// ARCHITECTURE.md "resolvers" and docs/overhaul/daemon.md "Resolvers and push
// duties".
package feed

import (
	"context"
	"errors"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
	"claude-repld/internal/lockwatch"
	"claude-repld/internal/paint"
	"claude-repld/internal/publish"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/wsm"
)

// ReaderID identifies one open connection's page walk. It is minted per
// OpenFeed and dropped when the connection ends; nothing about a walk survives
// a restart.
type ReaderID string

// The typed refusals the read side answers with. Each is a fact about the
// REQUEST, never about the feed's contents: an empty feed is a page, not an
// error.
//
// THE SERVER MAPS THESE ONTO THE LANDED ERROR ARMS, one for one, and this is
// the one place the mapping is written down:
//
//	ErrNoWalk       → GetFeedPageError.no_walk_standing
//	ErrUnknownFeed  → OpenFeedError.feed_not_in_workspace /
//	                  GetFeedPageError.feed_not_in_workspace
//	ErrUnknownToken → WatchFeed's transport refusal (the token names no feed
//	                  this daemon minted, so no page-level arm applies)
//	ErrTokenExpired → the same, with the reader expected to re-open the feed
//	ErrTargetNotFound     → LoadFeedThroughError.not_found
//	ErrHistoryUnavailable → LoadFeedThroughError.history_unavailable; OpenFeed
//	                        and GetFeedPage have no arm for it and answer a
//	                        transport error (book.go)
//
// `feed_undecodable` is deliberately NOT produced here: the server decodes a
// FeedId before it reaches this package, so a value that does not decode never
// becomes a resolver call.
var (
	// ErrUnknownToken is a watch token this resolver never minted, or one
	// minted for another workspace or feed.
	ErrUnknownToken = errors.New("feed: unknown watch token")
	// ErrTokenExpired is a token whose pinned start has fallen out of the
	// feed's retained publication log, so the never-miss guarantee cannot be
	// honored. The reader re-opens the feed.
	ErrTokenExpired = errors.New("feed: watch token's pinned start is no longer retained")
	// ErrNoWalk is a `next` with no walk standing for this reader.
	ErrNoWalk = errors.New("feed: no page walk standing for this reader")
	// ErrUnknownFeed is a feed this workspace does not own.
	ErrUnknownFeed = errors.New("feed: unknown feed for this workspace")
)

// Tail is one feed's live row stream, pinned to the token OpenFeed minted. It
// is publish's guarantee in the feed's spelling: the reader misses no row
// between the page it was served and the first row it streams.
type Tail interface {
	// Rows yields every row upsert for this feed from the pinned start, in
	// order, closing only when ctx is cancelled.
	Rows(ctx context.Context) <-chan *frontendv1.FeedRow
	// Token is the watch token this tail answers to.
	Token() *agentreplv1.FeedWatchToken
}

// Resolver is the feed's whole surface: the watcher's sink, the daemon-fact
// setters, and the read side.
type Resolver interface {
	sessionwatcher.FeedSink

	// Tail opens the live row stream for one feed, pinned to the token minted
	// by the OpenFeed that served this reader's page.
	Tail(ctx context.Context, ws ids.WorkspaceID, feed feedid.Feed, token *agentreplv1.FeedWatchToken) (Tail, error)
	// OpenPage answers OpenFeed: the NEWEST page of one feed, plus the watch
	// token the tail echoes. It mints the reader's walk.
	OpenPage(ctx context.Context, ws ids.WorkspaceID, feed feedid.Feed, reader ReaderID) (*frontendv1.FeedPage, *agentreplv1.FeedWatchToken, error)
	// NextPage answers GetFeedPage's next arm: the page before this reader's
	// current position. The daemon holds the walk.
	NextPage(ctx context.Context, ws ids.WorkspaceID, feed feedid.Feed, reader ReaderID) (*frontendv1.FeedPage, error)
	// CloseReader drops a reader's walk when its connection ends.
	CloseReader(ws ids.WorkspaceID, reader ReaderID)
	// LoadThrough answers LoadFeedThrough: the reader's root-feed walk,
	// advanced page by page — each handed to emit as it is served — until the
	// target row is served (loadthrough.go). ErrTargetNotFound: the walk
	// reached the conversation's start without it; ErrHistoryUnavailable
	// (wrapped): a page could not be read.
	LoadThrough(ctx context.Context, ws ids.WorkspaceID, reader ReaderID, target *frontendv1.FeedId, emit func(*frontendv1.FeedPage) error) (*frontendv1.FeedId, error)
	// LoadOlder loads the root feed's next older store page for a feature
	// that reads held rows past the oldest loaded one, pushing its rows to
	// every tail; false when nothing older can be loaded (loadthrough.go).
	LoadOlder(ctx context.Context, ws ids.WorkspaceID) (bool, error)
	// SourceUp says a history source for the workspace came up (a shim
	// client the fleet can read history through, whatever the vendor session
	// is doing): every feed a reader opened while none was up has its newest
	// page loaded and pushed to that reader (book.go).
	SourceUp(ws ids.WorkspaceID)
	// KeepNewestPage loads the root feed's newest store page now, while a
	// source is still up and only when the feed does not already hold it, so
	// the conversation stays drawn once the source goes (a failed vendor
	// start's shim is about to be stopped). Its rows are pushed (book.go).
	KeepNewestPage(ctx context.Context, ws ids.WorkspaceID) error
	// NoteFreshBook says the workspace's session is coming up FRESH: its main
	// agent's book is new and holds nothing, so the root feed's newest page is
	// known empty and at its start, and no reader's open asks the store for a
	// book that does not exist yet. The first live entry makes the next open
	// read the newest page as usual (book.go).
	NoteFreshBook(ws ids.WorkspaceID)

	// SetOutputAddress installs the address a lease holder wants the turns
	// IT starts drawn at; nil withdraws it. It redirects no other turn and
	// moves no row: the prompt queue records it on a turn the holder starts,
	// and only that turn draws there (turnaddress.go).
	SetOutputAddress(ws ids.WorkspaceID, addr *sessionwatcher.OutputAddress)
	// OutputAddress answers a copy of the address standing for one
	// workspace, nil when none stands. The prompt queue reads it as it
	// records a turn the lease holder started.
	OutputAddress(ws ids.WorkspaceID) *sessionwatcher.OutputAddress
	// AddressTurn records the address one turn draws at, nil for the root
	// feed: the address the prompt queue recorded on the turn
	// (wsm.Turn.Address), handed over as it is recorded, so the live draw and
	// every replay (TurnAddresses) place the turn from one fact.
	AddressTurn(ws ids.WorkspaceID, turn ids.TurnID, addr *sessionwatcher.OutputAddress)

	// ResetWorkspace empties one workspace's feed whole: every row of every
	// feed is retired on the wire and every accumulation is dropped, so the
	// workspace is as it would be if its feed had never been opened. It
	// belongs to a BIND — the one verb that changes which vendor conversation
	// a workspace runs — and to nothing else; a restart resumes the same
	// conversation and keeps its rows. `because` is the sentence the record
	// carries. reset.go states exactly what is dropped and what is kept.
	ResetWorkspace(ws ids.WorkspaceID, because string)

	// OnClearReceived draws a /clear's cleared divider the moment the daemon
	// accepts it, BEFORE the shim round-trip: the red bar and the cleared feed
	// appear instantly, and the shim's later ContextCut confirms the same row in
	// place with its "context cleared" subtext. It also registers the turn as a
	// directive so it draws no user-prompt bubble and its terminal draws no
	// "response cut short" bubble.
	OnClearReceived(ws ids.WorkspaceID, turn ids.TurnID)
	// OnCompactReceived registers a /compact as a directive turn so it, too,
	// draws no user-prompt bubble. It draws no optimistic divider — a
	// compaction's divider carries a summary that does not exist until the shim
	// compacts.
	OnCompactReceived(ws ids.WorkspaceID, turn ids.TurnID)
	// OnContextCutAborted undoes a directive turn the shim refused before any
	// turn ran: it retires the optimistic /clear divider and forgets the turn, so
	// no phantom bar is left and the feed recovers to what it showed before.
	OnContextCutAborted(ws ids.WorkspaceID, turn ids.TurnID)
	// OnTurnClosed is THE DOOR'S FEED HALF: the prompt queue's door closed a
	// turn's durable row with this close. A turn this feed already ended (its
	// own terminal drew the ending) is left as it is; any other turn it has
	// seen opened is ended from the close, so every turn that ends has exactly
	// one ending row. See turnclosed.go.
	OnTurnClosed(ws ids.WorkspaceID, turn ids.TurnID, close wsm.RecordedClose)
	// UpsertSynthesized upserts a daemon-synthesized row: a merge tab, the
	// cold gate, a session separation, or the mirror of an accepted user
	// prompt. The mirror is a user_prompt row stamped with the minted TurnId
	// and drawn with the metaprompt sentinel spans STRIPPED — the full text
	// stays on the durable record.
	UpsertSynthesized(ws ids.WorkspaceID, feed feedid.Feed, row *frontendv1.FeedRow)
	// UpsertDurable upserts a daemon-synthesized row A NEW DAEMON MUST DRAW
	// AGAIN — a merge's bubble, which no store replays — and records it, as
	// published and at the order key it was first drawn at, in Deps.DurableRows.
	// A workspace's recorded rows are drawn again, each where it stood, the
	// moment that workspace's feed is first touched; a bind's reset forgets
	// them with every other row.
	UpsertDurable(ws ids.WorkspaceID, feed feedid.Feed, row *frontendv1.FeedRow)
	// UpsertAtTurnAddress upserts a daemon-synthesized row of one turn at
	// THE ADDRESS THAT TURN DRAWS AT rather than a named feed: the accepted
	// prompt of a merge's own turn belongs in its tab, and every other
	// accepted prompt on the root feed, because the resolver's own later draw
	// of that same row key lands there and would otherwise leave a second row
	// standing forever.
	UpsertAtTurnAddress(ws ids.WorkspaceID, turn ids.TurnID, key feedid.RowKey, row *frontendv1.FeedRow)
	// UpsertCommandPanel mints a NON-DURABLE root-feed row carrying a
	// recognized command's panel. Its FeedId comes from (workspace, the
	// per-workspace monotonically increasing synthesized sequence), so a
	// re-push upserts rather than duplicating. Resolver memory only: never
	// stored, never replayed after a restart, and never in a page.
	UpsertCommandPanel(ws ids.WorkspaceID, panel *frontendv1.FeedCommandPanel) *frontendv1.FeedId
	// UpsertCommandRefused mints a NON-DURABLE root-feed refusal card for a
	// recognized-but-unsupported command. command is the literal as typed,
	// reason is the daemon's composed sentence, and addSupport requests the
	// "engineer support for it" offer marker. Same id minting and the same
	// non-durability as UpsertCommandPanel.
	UpsertCommandRefused(ws ids.WorkspaceID, command, reason string, addSupport bool) *frontendv1.FeedId
	// RetireRow removes a row from the feed — a synthesized row whose reason
	// to exist ended.
	RetireRow(ws ids.WorkspaceID, feed feedid.Feed, id *frontendv1.FeedId)

	// StandingFor answers a permission row's held standing token: the vendor's
	// offered standing for that ask, kept DAEMON-SIDE so it never reaches a
	// client. Reports false when the row carries no standing offer.
	StandingFor(ws ids.WorkspaceID, id *frontendv1.FeedId) (*conversationv1.AgentPermissionStanding, bool)

	// ServedPermission answers what the daemon SERVED for one permission ask:
	// the agent blocked on it and the standing token the vendor offered, nil
	// when none was. False means this workspace drew no such ask.
	//
	// A client sends back only the ask's identity, so the agent an answer must
	// be delivered to lives nowhere else.
	ServedPermission(ws ids.WorkspaceID, ask string) (*conversationv1.AgentId, *conversationv1.AgentPermissionStanding, bool)
	// ServedQuestion answers what the daemon SERVED for one question ask: the
	// agent blocked on it and the batch as served, so an answer naming a
	// question the batch never carried is refused rather than forwarded.
	ServedQuestion(ws ids.WorkspaceID, ask string) (*conversationv1.AgentId, *conversationv1.AgentQuestionBatch, bool)

	// FinalResponses answers the workspace's ordered selectable final-response
	// rows — the ones drawn with the green final-answer border, oldest first —
	// for reply-to-a-past-response mode. The server owns the selection cursor
	// over this set; the resolver owns the set itself, because it draws the
	// border. Empty is "no final responses yet", never an error.
	FinalResponses(ws ids.WorkspaceID) []*frontendv1.FeedId
	// RollbackPrompts answers the prompt rows a rollback can reach, oldest
	// first: the prompts that open a turn of the workspace's own current
	// conversation, after its newest clear or compaction. The server walks
	// its prompt selection over this set. Empty is "nothing to roll back to",
	// never an error.
	RollbackPrompts(ws ids.WorkspaceID) []*frontendv1.FeedId
	// RollbackTarget answers what rolling back to just before a prompt row
	// drops — its turn and every later reachable turn — and the prompt as
	// said; false when the row is not a prompt a rollback can reach.
	RollbackTarget(ws ids.WorkspaceID, row *frontendv1.FeedId) (RollbackTarget, bool)
	// RollBackTurns removes turns from the feed for good (rollback.go). The
	// removal always happens; an error says only that recording it durably
	// failed, already logged and raised.
	RollBackTurns(ws ids.WorkspaceID, turns []ids.TurnID) error
	// LiveDetachedIn answers how many detached subagents and shells drawn in
	// the turns are still live: what a files-restoring rollback stops.
	LiveDetachedIn(ws ids.WorkspaceID, turns []ids.TurnID) int
	// SelectableText answers the text of one selectable root-feed row
	// (FeedRow.selectable: a prompt or a landed response bubble) as markdown,
	// with whether it is a prompt, and whether the feedid is selectable at
	// all. A miss is a feedid the daemon does not deem selectable — the submit
	// path and the selection refuse it rather than using an empty text.
	SelectableText(ws ids.WorkspaceID, id *frontendv1.FeedId) (SelectableText, bool)
	// TakeTurnEnding answers what a turn's LIVE end said — the final answer's
	// prose, the errored ending's line, the terminal's failure class — and
	// forgets it. False is a turn whose ending was not drawn live here. The
	// prompt queue's turn end is the one caller: it hands the ending to the
	// desktop banner.
	TakeTurnEnding(ws ids.WorkspaceID, turn ids.TurnID) (TurnEnding, bool)

	// MintSubFeedHead records a bubble row's sub-feed so pages opened on it
	// resolve their breadcrumbs. The resolver calls it for subagent bubbles
	// itself; the merge orchestrator calls it for the merge head it
	// synthesizes, because only the orchestrator knows the branch line.
	// HEADFEED is the feed the head row is drawn ON: the crumb chain climbs
	// through it, so it is stated by the caller rather than searched for.
	MintSubFeedHead(ws ids.WorkspaceID, head *frontendv1.FeedId, headFeed feedid.Feed, sub feedid.Feed, label string)
}

// PortedPrompt is one prompt a FORK carried over from its parent: a question
// the parent was asked, under the child's own turn identity.
//
// It carries the text rather than composed content because that is what the
// daemon's own record holds; the row is drawn through the same functions a
// delivered prompt is drawn through.
type PortedPrompt struct {
	// Turn is the turn identity the row carries in the CHILD.
	Turn string
	// Text is the prompt's full text, as the parent recorded it.
	Text string
	// Origin is who the row is drawn as being from.
	Origin conversationv1.PromptOrigin
}

// ImageResolver turns a record's image reference — a host path or a URL — into
// a `src` a webview can load, plus the alt text. That resolution is the
// daemon's and never the client's, and it is injected because how a host path
// becomes a fetchable URL is the server's business rather than this package's.
type ImageResolver func(*conversationv1.ImageBlock) (src string, alt string, err error)

// Deps are the feed resolver's collaborators. They are injected rather than
// reached for so each can be faked in a test, and so the leaf packages this
// resolver draws from (feedid, paint, prompts) can land in parallel with it.
type Deps struct {
	// Log is the daemon's logging surface. Required.
	Log dlog.Surfaces
	// Stalls is the lock stall watchdog the resolver's mutex is registered
	// with. OPTIONAL: nil leaves it unwatched, which is what every test that
	// does not exercise it leaves it. Production always wires it: every
	// watcher's sink call and every accepted prompt's mirror takes this
	// mutex, and it is held across the workspace directory lookup.
	Stalls lockwatch.Registry
	// WorkspaceDir resolves a workspace's directory so records land in that
	// workspace's durable sink. Required: failing to resolve is an invariant
	// violation, never a reason to write globally.
	WorkspaceDir func(ids.WorkspaceID) (string, error)
	// Encode renders a row address as its FeedId. Defaults to feedid.Encode.
	Encode func(feedid.Ref) *frontendv1.FeedId
	// Decode parses a FeedId back into its row address. Defaults to
	// feedid.Decode.
	Decode func(*frontendv1.FeedId) (feedid.Ref, error)
	// EncodeFeed renders a feed address as its FeedId. Defaults to
	// feedid.EncodeFeed.
	EncodeFeed func(ids.WorkspaceID, feedid.Feed) *frontendv1.FeedId
	// Painter highlights code and parses ANSI. Required for read cards.
	Painter paint.Painter
	// StripSentinels removes the host's metaprompt sentinel spans from the
	// DRAWN text of a prompt row; the full text stays on the record. Defaults
	// to identity, which is what a prompt with no metaprompt needs.
	StripSentinels func(string) string
	// ResolveImage turns an image reference into a drawable src. Required for
	// prompt rows carrying images.
	ResolveImage ImageResolver
	// PortedPrompts answers the conversation a FORK carried over from its
	// parent, oldest first. It is read at the opening history page and drawn
	// ABOVE everything the workspace has of its own, which is what makes a
	// forked feed the parent's conversation followed by the fork's.
	//
	// nil means no workspace ever carries a ported conversation, which is
	// what a test that is not about forking wants.
	PortedPrompts func(context.Context, ids.WorkspaceID) ([]PortedPrompt, error)
	// TurnCloses answers the durable close of each named turn that has one
	// (wsm.DB.TurnCloses). A replay reads it for the turns its page opens, and
	// ends a turn whose page carries no terminal from its recorded close.
	//
	// nil draws no ending from a record, which is what a test that is not
	// about replayed closes wants.
	TurnCloses func(context.Context, ids.WorkspaceID, []ids.TurnID) (map[ids.TurnID]wsm.RecordedClose, error)
	// TurnAddresses answers the output address each named turn was recorded
	// with (wsm.DB.TurnAddresses): a recorded turn answers its address, nil for
	// the root feed, and an unrecorded one is absent. A replay takes each
	// recorded turn's address into the table the live draw places it by
	// (turnaddress.go), so a turn replays where it was drawn live.
	//
	// nil adds no address, which is what a test that is not about addressed
	// turns wants.
	TurnAddresses func(context.Context, ids.WorkspaceID, []ids.TurnID) (map[ids.TurnID]*wsm.OutputAddress, error)
	// OwnedTurns answers which of the named turns the workspace recorded as
	// its own (wsm.DB.RecordedTurns). On a FORK it is what tells the
	// conversation the fork inherited from the turns it ran itself: a
	// main-agent entry of any other turn, or of none, is the inherited past
	// and is drawn in the inherited plane (lineage.go).
	//
	// nil treats every entry as the workspace's own, which is what a test
	// that is not about forking wants.
	OwnedTurns func(context.Context, ids.WorkspaceID, []ids.TurnID) (map[ids.TurnID]bool, error)
	// DurableRows records the rows UpsertDurable publishes and answers them
	// again to a new daemon (wsm.DB's durable feed rows).
	//
	// nil records nothing, which is what a test that is not about a new
	// daemon wants. Production always wires it.
	DurableRows DurableRows
	// Now is the resolver's clock, injected so tests never sleep. Defaults to
	// time.Now.
	Now func() time.Time
	// AfterFunc schedules the response stall window, injected for the same
	// reason Now is: the window is a DAEMON-SIDE timer, and a test that waited
	// on the real one would be waiting on wall-clock rather than on a fact.
	// Defaults to time.AfterFunc. nil disables the stall window entirely.
	AfterFunc func(time.Duration, func()) Timer
	// AnswerStall is how long an open response fold may go with neither a frame
	// nor a terminal before the turn is called not timely. Defaults to
	// DefaultAnswerStall.
	AnswerStall time.Duration
	// Faults records the `final_answer_unresolved` fault a turn raises when its
	// answer does not land. It is the state client health.ObserveFaults
	// decorates, so the fault reaches the footer by the ONE path every fault
	// reaches it by. nil leaves the condition recorded in the log alone, which
	// is what a test that is not about the footer wants.
	Faults FaultRecorder
	// Warnings puts a resolution failure — a row that could not be placed —
	// on the topbar's warning chip, the webapp's one error surface. The raising
	// site logs the failure with its full context either way; nil leaves the
	// log as the only record, which is what a test that is not about the topbar
	// wants.
	Warnings WarningRaiser
	// EntryPlaced is told the FeedId of every detached-work-capable entry the
	// moment the feed first draws it, and again whenever that FeedId changes:
	// a subagent bubble keyed by its spawn unit, a shell bubble's head keyed by
	// its work id, a monitor's tool-call card keyed by its unit. It is how the footer's jump rows name the entry ON THE FEED
	// THAT DRAWS IT (a subagent of a subagent is on its parent's sub-feed, not
	// the root) instead of guessing an address.
	//
	// CALLED WITH THE RESOLVER'S LOCK HELD, the same order the fault path
	// already takes (feed, then footer): the receiver must never call back
	// into this resolver. nil tells nobody, which is what a test that is not
	// about the footer wants.
	EntryPlaced func(ws ids.WorkspaceID, unit string, row *frontendv1.FeedId)
	// RolledBack is the durable record of rolled-back turns (wsm.DB). nil
	// records and loads nothing, which is what a test that is not about
	// surviving a restart wants.
	RolledBack RolledBackTurnStore
	// History reads the store pages a reader's request needs (book.go). nil
	// pages nothing: every feed serves exactly what it holds, which is what a
	// test that is not about loading history wants. Production always wires
	// it.
	History HistorySource
	// TailRetention is how many published rows a feed retains for a tail's
	// replay. Defaults to DefaultTailRetention.
	TailRetention int
}

// WarningRaiser is the topbar's raised-warning channel, as this resolver needs
// it (topbar.Resolver.RaiseWarning).
type WarningRaiser interface {
	// RaiseWarning puts one keyed line on the warning strip.
	RaiseWarning(ws ids.WorkspaceID, key, line string)
}

// DefaultTailRetention is how many published rows one feed retains, so a token
// minted at an earlier instant can still replay without a gap.
const DefaultTailRetention = 4096

// New builds the feed resolver.
func New(deps Deps) (Resolver, error) {
	return newResolver(deps)
}

// Topic is the per-feed publication a tail subscribes to. It is exported so
// the server can wire streams without reaching inside the resolver.
type Topic = publish.Topic[*frontendv1.FeedRow]

// DurableRows is the record UpsertDurable writes through to: wsm.DB's durable
// feed rows.
type DurableRows interface {
	PutDurableFeedRow(ctx context.Context, row wsm.DurableFeedRow) error
	DurableFeedRows(ctx context.Context, id ids.WorkspaceID) ([]wsm.DurableFeedRow, error)
	ClearDurableFeedRows(ctx context.Context, id ids.WorkspaceID) error
}
