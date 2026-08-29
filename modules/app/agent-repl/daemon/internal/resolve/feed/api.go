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

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
	"claude-repld/internal/notimpl"
	"claude-repld/internal/publish"
	"claude-repld/internal/sessionwatcher"
)

// ReaderID identifies one open connection's page walk. It is minted per
// OpenFeed and dropped when the connection ends; nothing about a walk survives
// a restart.
type ReaderID string

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
	// stored, never replayed after a restart.
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
}

// New builds the feed resolver.
func New(log dlog.Surfaces) (Resolver, error) {
	return nil, notimpl.Err
}

// Topic is the per-feed publication a tail subscribes to. It is exported so
// the server can wire streams without reaching inside the resolver.
type Topic = publish.Topic[*frontendv1.FeedRow]
