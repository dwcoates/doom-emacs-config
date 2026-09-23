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
	"claude-repld/internal/paint"
	"claude-repld/internal/publish"
	"claude-repld/internal/sessionwatcher"
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

	// SetOutputAddress installs the address a lease holder wants this
	// session's rows stamped with; nil restores the root feed.
	SetOutputAddress(ws ids.WorkspaceID, addr *sessionwatcher.OutputAddress)

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
	// UpsertSynthesized upserts a daemon-synthesized row: a merge tab, the
	// cold gate, a session separation, or the mirror of an accepted user
	// prompt. The mirror is a user_prompt row stamped with the minted TurnId
	// and drawn with the metaprompt sentinel spans STRIPPED — the full text
	// stays on the durable record.
	UpsertSynthesized(ws ids.WorkspaceID, feed feedid.Feed, row *frontendv1.FeedRow)
	// UpsertAtOutputAddress upserts a daemon-synthesized row at the session's
	// STANDING OUTPUT ADDRESS rather than a named feed: the mirror of an
	// accepted user prompt belongs wherever the lease holder addressed the
	// session's output (a merge tab), never unconditionally on the root feed,
	// because the resolver's own later draw of that same row key lands at the
	// address and would otherwise leave the root copy standing forever.
	UpsertAtOutputAddress(ws ids.WorkspaceID, key feedid.RowKey, row *frontendv1.FeedRow)
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

	// RaiseColdGate draws the STANDING cold-context gate from the shim's cold
	// facts: the raw counts and instants the CLIENT formats and ticks, plus the
	// summarizers and compaction scopes the daemon will accept back. While
	// standing the gate owns the composer.
	RaiseColdGate(ws ids.WorkspaceID, cold *conversationv1.SessionCold, summarizers []*conversationv1.AgentModel, scopes []conversationv1.SessionCompactScope) *frontendv1.FeedId
	// ResolveColdGate replaces the standing gate with the trace of what was
	// chosen. The row stays in history: the decision is a fact a reader
	// scrolling back must find.
	ResolveColdGate(ws ids.WorkspaceID, remediation *conversationv1.SessionColdRemediation) *frontendv1.FeedId

	// FinalResponses answers the workspace's ordered selectable final-response
	// rows — the ones drawn with the green final-answer border, oldest first —
	// for reply-to-a-past-response mode. The server owns the selection cursor
	// over this set; the resolver owns the set itself, because it draws the
	// border. Empty is "no final responses yet", never an error.
	FinalResponses(ws ids.WorkspaceID) []*frontendv1.FeedId
	// ResponseMarkdown answers the settled markdown of one selectable final
	// response, and whether the feedid is selectable at all. A miss is a
	// feedid the daemon does not deem selectable — the submit path refuses it
	// rather than delivering an empty reply prefix.
	ResponseMarkdown(ws ids.WorkspaceID, id *frontendv1.FeedId) (string, bool)

	// MintSubFeedHead records a bubble row's sub-feed so pages opened on it
	// resolve their breadcrumbs. The resolver calls it for subagent bubbles
	// itself; the merge orchestrator calls it for the merge head it
	// synthesizes, because only the orchestrator knows the branch line.
	MintSubFeedHead(ws ids.WorkspaceID, head *frontendv1.FeedId, sub feedid.Feed, label string)
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
	// WorkspaceDir resolves a workspace's directory so records land in that
	// workspace's durable sink. Required: failing to resolve is an invariant
	// violation, never a reason to write globally.
	WorkspaceDir func(ids.WorkspaceID) (string, error)
	// Encode renders a row address as its FeedId. Defaults to feedid.Encode.
	Encode func(feedid.Ref) *frontendv1.FeedId
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
	// PageSize is how many rows a page carries. Defaults to DefaultPageSize.
	PageSize int
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

// DefaultPageSize is the page size the daemon picks when Deps names none. The
// wire carries no cursor and no page size: the daemon owns both.
const DefaultPageSize = 50

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
