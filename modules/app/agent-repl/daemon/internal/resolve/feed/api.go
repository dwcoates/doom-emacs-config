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
	// UpsertSynthesized upserts a daemon-synthesized row: a merge tab, the
	// cold gate, a session separation, or the mirror of an accepted user
	// prompt. The mirror is a user_prompt row stamped with the minted TurnId
	// and drawn with the metaprompt sentinel spans STRIPPED — the full text
	// stays on the durable record.
	UpsertSynthesized(ws ids.WorkspaceID, feed feedid.Feed, row *frontendv1.FeedRow)
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

	// RaiseColdGate draws the STANDING cold-context gate from the shim's cold
	// facts: the raw counts and instants the CLIENT formats and ticks, plus the
	// summarizers and compaction scopes the daemon will accept back. While
	// standing the gate owns the composer.
	RaiseColdGate(ws ids.WorkspaceID, cold *conversationv1.SessionCold, summarizers []*conversationv1.AgentModel, scopes []conversationv1.SessionCompactScope) *frontendv1.FeedId
	// ResolveColdGate replaces the standing gate with the trace of what was
	// chosen. The row stays in history: the decision is a fact a reader
	// scrolling back must find.
	ResolveColdGate(ws ids.WorkspaceID, remediation *conversationv1.SessionColdRemediation) *frontendv1.FeedId

	// MintSubFeedHead records a bubble row's sub-feed so pages opened on it
	// resolve their breadcrumbs. The resolver calls it for subagent bubbles
	// itself; the merge orchestrator calls it for the merge head it
	// synthesizes, because only the orchestrator knows the branch line.
	MintSubFeedHead(ws ids.WorkspaceID, head *frontendv1.FeedId, sub feedid.Feed, label string)
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
	// Now is the resolver's clock, injected so tests never sleep. Defaults to
	// time.Now.
	Now func() time.Time
	// PageSize is how many rows a page carries. Defaults to DefaultPageSize.
	PageSize int
	// TailRetention is how many published rows a feed retains for a tail's
	// replay. Defaults to DefaultTailRetention.
	TailRetention int
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
