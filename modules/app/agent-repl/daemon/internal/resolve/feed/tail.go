package feed

import (
	"context"
	"fmt"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
	"agent-repld/internal/inflight"
	"agent-repld/internal/publish"
)

// The tail is publish's never-miss guarantee in the feed's spelling. The
// difference from a Topic is the PIN: OpenPage mints a token naming the
// publication its page ended at, and the tail begins exactly after it — so no
// row falls between the page and the stream, and none is drawn twice.

// tailSub is one tail's delivery: a counted publish.Mailbox, unbounded and
// drained by its own goroutine, so a slow reader never blocks an upsert, and
// every publication counts as work in flight until the reader acks it.
type tailSub = publish.Mailbox[Publication]

// tail is one open tail: the subscriber and the token it answers to.
type tail struct {
	r     *resolver
	f     *feedState
	sub   *tailSub
	token *agentreplv1.FeedWatchToken
}

// Token is the watch token this tail answers to.
func (t *tail) Token() *agentreplv1.FeedWatchToken { return t.token }

// Publications yields every publication from the pinned start, in order,
// closing only when ctx is cancelled. The reader calls ack once per
// publication it received, once it has written it.
func (t *tail) Publications(ctx context.Context) (<-chan Publication, func()) {
	// A tail ends ONLY on the reader's cancellation: a stream ending any other
	// way is a transport failure, which is exactly the distinction the feed's
	// liveness rests on.
	inflight.Standing(func() {
		t.sub.Run(ctx, func() {
			t.r.mu.Lock()
			delete(t.f.subs, t.sub)
			t.r.mu.Unlock()
		})
	})
	return t.sub.Out(), t.sub.Ack
}

// Tail opens the live row stream for one feed, pinned to the token minted by
// the OpenFeed that served this reader's page.
func (r *resolver) Tail(ctx context.Context, ws ids.WorkspaceID, feed feedid.Feed, token *agentreplv1.FeedWatchToken) (Tail, error) {
	r.mu.Lock()
	defer r.mu.Unlock()
	log := r.logger(ws)

	minted, ok := r.tokens[token.GetValue()]
	if !ok {
		log.Warn("daemon.feed.tail_unknown_token",
			"a tail was opened with a token this daemon never minted",
			dlog.Context{"token": token.GetValue()})
		return nil, ErrUnknownToken
	}
	key := r.feedKey(ws, feed)
	if minted.ws != ws || minted.feedKey != key {
		log.Warn("daemon.feed.tail_foreign_token",
			"a tail was opened with a token minted for another workspace or feed",
			dlog.Context{"token": token.GetValue(), "minted_feed": minted.feedKey, "asked_feed": key})
		return nil, ErrUnknownToken
	}

	s := r.state(ws)
	f := r.feed(s, feed)
	sub := publish.NewMailbox[Publication](inflight.Default)

	// The pin, honored exactly: every publication after the token's sequence
	// is replayed before the tail follows, so the seam between the page and
	// the stream neither gaps nor overlaps.
	if len(f.log) > 0 && f.log[0].seq > minted.afterSeq+1 {
		log.Warn("daemon.feed.tail_token_expired",
			"a tail's pinned start has fallen out of the retained publication log",
			dlog.Context{"token": token.GetValue(), "pinned_after": minted.afterSeq, "oldest_retained": f.log[0].seq})
		return nil, ErrTokenExpired
	}
	// THE REPLAY OBEYS THE SAME DELIVERY RULES THE LIVE PUSH DOES
	// (withheldFromPush), judged against the order as it stands now: the log
	// retains every publication, withheld ones included, and a tail opened
	// against an earlier pin must not be handed a row the push kept off the
	// wire.
	replayed, withheld := 0, 0
	for _, entry := range f.log {
		if entry.seq <= minted.afterSeq {
			r.logger(ws).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "entry.seq <= minted.afterSeq"})
			continue
		}
		if r.withheldPublication(s, f, entry.pub) {
			withheld++
			continue
		}
		sub.Enqueue(entry.pub)
		replayed++
	}
	f.subs[sub] = struct{}{}

	log.Debug("daemon.feed.tail_opened",
		"a feed tail was opened at its pinned start",
		dlog.Context{"feed": key, "pinned_after": minted.afterSeq, "replayed": replayed, "withheld": withheld})
	return &tail{r: r, f: f, sub: sub, token: token}, nil
}

// mintToken records a tail address pinned to a feed's current publication.
func (r *resolver) mintToken(ws ids.WorkspaceID, f *feedState) *agentreplv1.FeedWatchToken {
	r.tokenSeq++
	value := fmt.Sprintf("ft-%d", r.tokenSeq)
	r.tokens[value] = &watchToken{ws: ws, feedKey: f.key, afterSeq: f.seq}
	return &agentreplv1.FeedWatchToken{Value: value}
}
