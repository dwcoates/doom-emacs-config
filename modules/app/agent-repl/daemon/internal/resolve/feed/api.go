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

	"agent-repld/internal/chessboard"
	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
	"agent-repld/internal/lockwatch"
	"agent-repld/internal/paint"
	"agent-repld/internal/publish"
	"agent-repld/internal/resolve/holds"
	"agent-repld/internal/sessionwatcher"
	"agent-repld/internal/wsm"
)

// The typed refusals the read side answers with. Each is a fact about the
// REQUEST, never about the feed's contents: an empty feed is a page, not an
// error.
//
// THE SERVER MAPS THESE ONTO THE LANDED ERROR ARMS, one for one, and this is
// the one place the mapping is written down:
//
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
	// ErrUnknownFeed is a feed this workspace does not own.
	ErrUnknownFeed = errors.New("feed: unknown feed for this workspace")
)

// Tail is one feed's live publication stream, pinned to the token OpenFeed
// minted. It is publish's guarantee in the feed's spelling: the reader misses
// no publication between the page (and, on the root feed, the held section)
// it was served and the first publication it streams.
type Tail interface {
	// Publications yields every publication for this feed from the pinned
	// start, in order — row upserts and removals, and on the root feed the held
	// section's changes — closing only when ctx is cancelled.
	Publications(ctx context.Context) <-chan Publication
	// Token is the watch token this tail answers to.
	Token() *agentreplv1.FeedWatchToken
}

// Resolver is the feed's whole surface: the watcher's sink, the daemon-fact
// setters, and the read side.
type Resolver interface {
	sessionwatcher.FeedSink
	// The held section's setters (held.go): the prompt queue's standing holds
	// and edit claim, and the merge orchestrator's posed offer.
	holds.Sink
	// The held section's ingress setters (held.go): the held-prompt ingress's
	// unaccepted entries, and the prompt handler's claim of each entry's turn.
	holds.IngressSink

	// SetAccount installs the account the workspace's session spends as: the
	// logged-in email, "" for a logged-out root. A failed turn's outcome
	// marker names it (feed.proto FeedOutcomeAccount). The daemon calls it
	// beside the topbar's account cell, from the one place that resolves it.
	SetAccount(ws ids.WorkspaceID, email string)

	// Tail opens the live row stream for one feed, pinned to the token minted
	// by the OpenFeed that served this reader's page.
	Tail(ctx context.Context, ws ids.WorkspaceID, feed feedid.Feed, token *agentreplv1.FeedWatchToken) (Tail, error)
	// OpenPage answers OpenFeed: the NEWEST page of one feed, plus the watch
	// token the tail echoes, plus — for the root feed only, nil otherwise — the
	// held section as it stands at the token's pin.
	OpenPage(ctx context.Context, ws ids.WorkspaceID, feed feedid.Feed) (*frontendv1.FeedPage, *agentreplv1.FeedWatchToken, *frontendv1.FeedHeld, error)
	// OlderPage answers GetFeedPage's older_than arm: the page of rows sorting
	// before oldestHeld, the order key of the oldest row the reader holds.
	OlderPage(ctx context.Context, ws ids.WorkspaceID, feed feedid.Feed, oldestHeld string) (*frontendv1.FeedPage, error)
	// LoadThrough answers LoadFeedThrough: the root feed walked older from
	// directly above oldestHeld (from the newest page when nil), page by page —
	// each handed to emit as it is served — until the target row is served
	// (loadthrough.go). ErrTargetNotFound: the walk reached the conversation's
	// start without it; ErrHistoryUnavailable (wrapped): a page could not be
	// read.
	LoadThrough(ctx context.Context, ws ids.WorkspaceID, oldestHeld *string, target *frontendv1.FeedId, emit func(*frontendv1.FeedPage) error) (*frontendv1.FeedId, error)
	// LoadOlder loads the root feed's next older store page for a feature
	// that reads held rows past the oldest loaded one, pushing its rows to
	// every tail; false when nothing older can be loaded (loadthrough.go).
	LoadOlder(ctx context.Context, ws ids.WorkspaceID) (bool, error)
	// ReadNewest reads the root feed's newest page WHOLE for a workspace
	// being opened (OpenTabs' conversation read): the page and the turn
	// prompts its rows belong to, through the same loading a reader's open
	// takes, and pushed to every tail. A page held and unmoved is not read
	// again, and an OpenFeed after it serves the page from memory.
	// ErrNoHistorySource: no shim is up to read from (the page is loaded for
	// the first reader once one is); ErrHistoryUnavailable (wrapped): the
	// read failed (book.go).
	ReadNewest(ctx context.Context, ws ids.WorkspaceID) error
	// SourceUp says a history source for the workspace came up (a shim
	// client the fleet can read history through, whatever the vendor session
	// is doing): every feed a reader opened while none was up has its newest
	// page loaded and pushed to that reader (book.go).
	SourceUp(ws ids.WorkspaceID)
	// NoteFreshBook says the workspace's session is coming up FRESH: its main
	// agent's book is new and holds nothing, so the root feed's newest page is
	// known empty and at its start, and no reader's open asks the store for a
	// book that does not exist yet. The first live entry makes the next open
	// read the newest page as usual (book.go).
	NoteFreshBook(ws ids.WorkspaceID)
	// NoteLineage tells the resolver where a fork's inherited past is read
	// from, nearest generation first; nil for a workspace that is no fork.
	// The fleet states it at every bring-up (lineage.go).
	NoteLineage(ws ids.WorkspaceID, segments []LineageSegment)

	// SetOutputAddress installs the address a lease holder wants the turns
	// IT starts drawn at; nil withdraws it. It redirects no other turn and
	// moves no row: the prompt queue records it on a turn the holder starts,
	// and only that turn draws there (turnaddress.go).
	// RefreshChessBoards re-publishes every drawn board of the named sessions,
	// whose board state changed (chessboard.Boards.Run drives it).
	RefreshChessBoards(sessions []chessboard.Session)

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
	// SetOutputMirror installs the mirror a lease holder wants the turns IT
	// starts drawn at TOO, in another workspace's feed (an async merge
	// workspace's merge turns, in the requester's bubble); nil withdraws it.
	// Like SetOutputAddress it moves no row (turnmirror.go).
	SetOutputMirror(ws ids.WorkspaceID, m *wsm.MirrorAddress)
	// OutputMirror answers a copy of the mirror standing for one workspace,
	// nil when none stands. The prompt queue reads it as it records a turn
	// the lease holder started.
	OutputMirror(ws ids.WorkspaceID) *wsm.MirrorAddress
	// MirrorTurn records the mirror one turn is also drawn at, nil for none:
	// the mirror the prompt queue recorded on the turn (wsm.Turn.Mirror),
	// handed over as it is recorded, so the live draw and every replay
	// (TurnMirrors) mirror the turn from one fact.
	MirrorTurn(ws ids.WorkspaceID, turn ids.TurnID, m *wsm.MirrorAddress)
	// SenderTurn records the workspace whose agent sent one turn's prompt, nil
	// for a prompt no agent sent: the sender the prompt queue recorded on the
	// turn (wsm.Turn.SentBy), handed over as it is recorded, so the live draw
	// and every replay (TurnSenders) head the prompt from one fact.
	SenderTurn(ws ids.WorkspaceID, turn ids.TurnID, sender *ids.WorkspaceID)
	// PromptHeader answers the header one turn's prompt is drawn under: the
	// sending workspace for an agent-sent prompt, nil for every other origin.
	// The prompt queue's mirror of an accepted prompt draws PromptRow with it,
	// as the resolver's own draw does (turnsender.go).
	PromptHeader(ws ids.WorkspaceID, turn ids.TurnID, origin conversationv1.PromptOrigin) *frontendv1.FeedAgentPromptAddress
	// SentPrompt draws, durably, the row a workspace's feed shows for a prompt
	// its agent sent another workspace (the daemon takes the send itself):
	// headed by the recipient's name then a right arrow, with how it fared
	// (turnsender.go).
	SentPrompt(sender ids.WorkspaceID, recipient, key string, said *conversationv1.UserSaid, fate SentFate)

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
	// UpsertDurablePinned is UpsertDurable for a row PINNED to its feed's
	// bottom (frontend.v1 FeedRowOrder.pinned): drawn below every placed row
	// for as long as it is published through this method. Its next
	// publication through UpsertDurable SETTLES it — it takes a fixed key just
	// after the feed's newest placed row, where it stood — once and for good.
	// A row already placed is never pinned again: the call is refused at
	// ERROR and the row keeps its place.
	UpsertDurablePinned(ws ids.WorkspaceID, feed feedid.Feed, row *frontendv1.FeedRow)
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
	// EditQuestionDraft applies one edit to a STANDING question batch's draft
	// — the record the open card is drawn from — and re-publishes the card
	// (questiondraft.go). When the edit answered the batch's last question
	// the outcome carries the answers to deliver and the batch is CLAIMED:
	// no further edit applies, and a typed answer no longer routes to it,
	// until the delivery settles the ask or ReleaseQuestionDelivery frees it.
	// Refuses ErrQuestionNotStanding and *UnservedQuestionValueError.
	EditQuestionDraft(ws ids.WorkspaceID, ask string, edit QuestionEdit) (QuestionDraftOutcome, error)
	// ReleaseQuestionDelivery frees a claimed batch whose delivery failed, so
	// the next edit that leaves it answered delivers it again.
	ReleaseQuestionDelivery(ws ids.WorkspaceID, ask string)
	// StandingAsk answers the OLDEST ask standing on the feed a submission
	// addresses (the root when target is nil, else the sub-feed headed by
	// target) — the ask a user's typed send answers.
	StandingAsk(ws ids.WorkspaceID, target *feedid.Ref) (StandingAsk, bool)
	// HostAsk answers the root feed's oldest standing ask as the host view
	// carries it (agentrepl.v1.HostWorkspace.ask), nil when none stands. It
	// never takes the feed's lock, so the host view's composition can read it
	// from anywhere (hostask.go).
	HostAsk(ws ids.WorkspaceID) *agentreplv1.HostAsk
	// WatchHostAsks yields every workspace whose HostAsk moved, from this call
	// on, until ctx is cancelled. Every emission follows the move it
	// announces, so a reader that recomposes on it reads the moved value.
	WatchHostAsks(ctx context.Context) <-chan ids.WorkspaceID

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
	// TurnBoundaries answers RollbackPrompts and FinalResponses as one
	// sequence in root-feed order, oldest first, each row with its kind. The
	// server walks the composer's turn-boundary step over this set.
	TurnBoundaries(ws ids.WorkspaceID) []TurnBoundary
	// RollbackTarget answers what rolling back to just before a prompt row
	// drops — its turn and every later reachable turn — and the prompt as
	// said; false when the row is not a prompt a rollback can reach.
	RollbackTarget(ws ids.WorkspaceID, row *frontendv1.FeedId) (RollbackTarget, bool)
	// RollbackChoices answers the prompts a rollback can reach, oldest first,
	// each with the context figure that stood just before it, for a reader to
	// choose among (agentrepl.v1 ListRollbackPrompts).
	RollbackChoices(ws ids.WorkspaceID) []RollbackChoice
	// PromptFollowed reports whether agent output (a response bubble or a
	// purple-titled card) is drawn after the prompt row in the root feed:
	// whether rolling back to it drops anything a reader has seen.
	PromptFollowed(ws ids.WorkspaceID, row *frontendv1.FeedId) bool
	// RollBackTurns removes turns from the feed for good (rollback.go). The
	// removal always happens; an error says only that recording it durably
	// failed, already logged and raised.
	RollBackTurns(ws ids.WorkspaceID, turns []ids.TurnID) error
	// HoldTurns withholds every row of turns a rollback is cutting away while
	// it runs (rollback.go), so a running turn's interrupted ending is never
	// drawn only to vanish. Every hold ends in RollBackTurns or ReleaseTurns.
	HoldTurns(ws ids.WorkspaceID, turns []ids.TurnID)
	// ReleaseTurns ends a hold for a rollback that did not happen, drawing the
	// rows withheld meanwhile in arrival order.
	ReleaseTurns(ws ids.WorkspaceID, turns []ids.TurnID)
	// WithholdDrawnTurns takes the rows already drawn for held TURNS off the
	// feed once the shim has made the cut, until the rollback's restart
	// settles (conversation.v1.SessionRollbackSettled, routed through
	// OnSessionUpdate), which removes them for good or draws them again.
	WithholdDrawnTurns(ws ids.WorkspaceID, turns []ids.TurnID)
	// OnSignedIn takes a completed sign-in for the workspace's account root
	// (signin.go): the failure rows of every drawn turn whose ending offers
	// the sign-in action leave the feed for good. The removal always happens;
	// an error says only that recording it durably failed, already logged and
	// raised.
	OnSignedIn(ws ids.WorkspaceID) error
	// LiveDetachedIn answers how many detached subagents and shells drawn in
	// the turns are still live: what a files-restoring rollback stops.
	LiveDetachedIn(ws ids.WorkspaceID, turns []ids.TurnID) int
	// SelectableText answers the text of one selectable root-feed row
	// (FeedRow.selectable: a prompt or a landed response bubble that is
	// neither a thinking bubble nor an interim response) as markdown, with
	// whether it is a prompt, and whether the feedid is selectable at all. A miss is a feedid the daemon does not
	// deem selectable — the submit path and the selection refuse it rather
	// than using an empty text.
	SelectableText(ws ids.WorkspaceID, id *frontendv1.FeedId) (RowText, bool)
	// PromptEntry answers what the entry answering LABEL of the root-feed user
	// prompt row ID says (frontend.v1.ProseEntry.text), as last published. A
	// miss is a row that is not such a prompt, or holds no entry answering
	// LABEL.
	PromptEntry(ws ids.WorkspaceID, id *frontendv1.FeedId, label string) (string, bool)
	// BubbleText answers the text of one landed root-feed bubble (a prompt or
	// a landed response bubble, a thinking one included) as markdown, with
	// whether it is a prompt, whether or not the reader can select it. A miss
	// is a feedid that names no such bubble.
	BubbleText(ws ids.WorkspaceID, id *frontendv1.FeedId) (RowText, bool)
	// SetMergeFold records the reader's fold on a merge bubble's head row and
	// restates the row with it (mergefold.go). False is a row that is not a
	// merge head this daemon holds.
	SetMergeFold(ws ids.WorkspaceID, id *frontendv1.FeedId, folded bool) bool
	// MarkMergeWorkspaceRemoved restates every switch in WS's feeds that takes
	// the user to MERGEWS as removed, and answers how many rows it restated
	// (mergeworkspace.go).
	MarkMergeWorkspaceRemoved(ws, mergeWS ids.WorkspaceID, removedAtMS int64) int
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
	// TurnMirrors answers the mirror each named turn was recorded with
	// (wsm.DB.TurnMirrors), for the turns that have one. A replay takes each
	// into the table the live draw mirrors by (turnmirror.go).
	//
	// nil adds no mirror, which is what a test that is not about mirrored
	// turns wants.
	TurnMirrors func(context.Context, ids.WorkspaceID, []ids.TurnID) (map[ids.TurnID]*wsm.MirrorAddress, error)
	// TurnSenders answers the workspace whose agent sent each named turn's
	// prompt (wsm.DB.TurnSenders), for the turns that have one. A replay takes
	// each into the table the live draw heads agent-sent prompts by
	// (turnsender.go).
	//
	// nil adds no sender, which is what a test that is not about agent-sent
	// prompts wants.
	TurnSenders func(context.Context, ids.WorkspaceID, []ids.TurnID) (map[ids.TurnID]ids.WorkspaceID, error)
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
	// MergeRow is told every merge bubble row the feed holds, as it stands
	// after each publication: a merge head on the root feed, and every tab
	// row on a merge's sub-feed, each with the merge's ledger id.
	// MergeRowRetired is told each one the feed retires, a reset's included.
	// They are the footer's ⇄ chip and panel's whole source, so the footer
	// draws exactly the merges the feed draws (mergerows.go).
	//
	// CALLED WITH THE RESOLVER'S LOCK HELD, as EntryPlaced is: the receiver
	// must never call back into this resolver. nil tells nobody, which is what
	// a test that is not about the footer wants.
	MergeRow        func(ws ids.WorkspaceID, lease ids.LeaseID, row *frontendv1.FeedRow)
	MergeRowRetired func(ws ids.WorkspaceID, lease ids.LeaseID, id string)
	// Commands answers the workspace's standing slash-command catalog
	// (commandcatalog.Catalogs, the one table ListSlashCommands also reads),
	// and false while none has been stated. A user-prompt row's `commands`
	// is stamped from it on every publication (commands.go). Called with the
	// resolver's lock held; the table never calls back into this resolver.
	//
	// nil knows no catalog and stamps no commands, which is what a test that
	// is not about slash commands wants. Production always wires it.
	Commands func(ids.WorkspaceID) (*conversationv1.SessionCommandCatalog, bool)
	// RolledBack is the durable record of rolled-back turns (wsm.DB). nil
	// records and loads nothing, which is what a test that is not about
	// surviving a restart wants.
	RolledBack RolledBackTurnStore
	// SignedIn is the durable record of turns whose account failure a sign-in
	// answered (wsm.DB). nil records and loads nothing, which is what a test
	// that is not about surviving a restart wants.
	SignedIn SignedInTurnStore
	// History reads the store pages a reader's request needs (book.go). nil
	// pages nothing: every feed serves exactly what it holds, which is what a
	// test that is not about loading history wants. Production always wires
	// it.
	History HistorySource
	// PeerDeliveries answers, through a workspace's shim, which sessions hold
	// the two ends of a cross-session message (peerdelivery.go). nil asks
	// nothing: an end no row drawn here names stays unresolved, which is what a
	// test that is not about the store's answer wants. Production always wires
	// it.
	PeerDeliveries PeerDeliverySource
	// VendorTasks answers, through a workspace's shim, which agent of its
	// session a vendor task id names (vendortask.go). nil asks nothing: a
	// message from an agent of the session keeps its sender's task id, which
	// is what a test that is not about the record's answer wants. Production
	// always wires it.
	VendorTasks VendorTaskSource
	// WorkspaceName answers a workspace's display name, the one the sidebar
	// draws, for the header of a cross-session message whose other end is that
	// workspace. nil names no workspace, which draws every other end
	// unresolved. Production always wires it.
	WorkspaceName func(ids.WorkspaceID) (string, error)
	// Go runs work the resolver starts OFF its lock (a peer delivery lookup).
	// Defaults to a new goroutine; a test runs the work when it chooses, so
	// it never waits on a goroutine it cannot see.
	Go func(func())
	// TailRetention is how many published rows a feed retains for a tail's
	// replay. Defaults to DefaultTailRetention.
	TailRetention int
	// ChessBoards is the daemon's chess board state (internal/chessboard),
	// which a board row's body is drawn from. nil draws no board: a board
	// frame arriving then is reported as undrawable, which is what a test that
	// is not about boards never sends. Production always wires it.
	ChessBoards ChessBoards
	// Pacing is the daemon's record of block arrival speeds, which puts a
	// reveal pace on every live prose and thinking bubble (revealpace.go). nil
	// measures nothing and serves no pace, which is what a test that is not
	// about pacing wants. Production always wires it.
	Pacing RevealPacing
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
