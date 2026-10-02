package feed

import (
	"context"
	"errors"
	"fmt"

	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
)

// ErrTargetNotFound is a LoadThrough whose walk reached the conversation's
// start without drawing its target: no entry of the conversation corresponds
// to it. The pages already handed over stay loaded.
var ErrTargetNotFound = errors.New("feed: the walk reached the conversation's start without drawing the target row")

// LoadThrough answers LoadFeedThrough (endpoint_load_feed_through.proto):
// walk the reader's ROOT-feed walk older, one page at a time — each read from
// the store when the daemon does not hold it — until TARGET has been served,
// handing every page to EMIT as it is served. It answers the target as the
// daemon serves it.
//
// THE WALK IS THE READER'S ORDINARY WALK, ADVANCED: a GetFeedPage `next` after
// it continues from the oldest page it delivered. A reader with no walk
// standing begins one at the top, so its first page is the newest.
//
// It fails with ErrTargetNotFound when the walk reaches the start without the
// target, ErrHistoryUnavailable (wrapped) when a page could not be read, and
// EMIT's own error when the page could not be handed over. Pages already
// handed over stay loaded either way.
func (r *resolver) LoadThrough(ctx context.Context, ws ids.WorkspaceID, reader ReaderID, target *frontendv1.FeedId, emit func(*frontendv1.FeedPage) error) (*frontendv1.FeedId, error) {
	mu := r.loadMu(ws)
	mu.Lock()
	defer mu.Unlock()
	root := feedid.Feed{Root: true}
	r.beginWalkAtTop(ws, root, reader)

	step := pageStep{}
	pages := 0
	for {
		r.mu.Lock()
		if r.targetServed(ws, root, reader, target) {
			r.mu.Unlock()
			r.lockedLogger(ws).Info("daemon.feed.load_through_reached",
				"a walk to a target row reached it; every page between was handed to the reader",
				dlog.Context{"target": target.GetValue(), "reader": string(reader), "pages": pages})
			return target, nil
		}
		page, _, plan, err := r.stepPage(ws, root, reader, &step)
		r.mu.Unlock()
		if err != nil {
			return nil, err
		}
		if plan != nil {
			if _, err := r.load(ctx, *plan); err != nil {
				if !errors.Is(err, ErrNoHistorySource) {
					return nil, err
				}
				step.noSource = true
			}
			continue
		}
		if step.exhausted {
			r.lockedLogger(ws).Info("daemon.feed.load_through_not_found",
				"a walk to a target row reached the conversation's start without drawing it",
				dlog.Context{"target": target.GetValue(), "reader": string(reader), "pages": pages, "no_source": step.noSource})
			return nil, ErrTargetNotFound
		}
		if err := emit(page); err != nil {
			return nil, fmt.Errorf("feed: a page of the walk to %q could not be handed over: %w", target.GetValue(), err)
		}
		pages++
	}
}

// beginWalkAtTop stands a walk at the top of FEED for a reader that has none on
// it, so a LoadThrough's first page is the newest.
func (r *resolver) beginWalkAtTop(ws ids.WorkspaceID, feed feedid.Feed, reader ReaderID) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	f := r.feed(s, feed)
	if w, ok := s.readers[reader]; ok && w.standing && w.feedKey == f.key {
		return
	}
	s.readers[reader] = &walk{feedKey: f.key, standing: true, top: true}
	r.logger(ws).Info("daemon.feed.load_through_walk_begun",
		"a walk to a target row began with no walk standing; it starts at the newest page",
		dlog.Context{"feed": f.key, "reader": string(reader)})
}

// targetServed reports whether the reader's walk has served TARGET: it is a
// row of the feed that delivery serves, at or above the oldest row the walk
// has served. Called with r.mu held.
func (r *resolver) targetServed(ws ids.WorkspaceID, feed feedid.Feed, reader ReaderID, target *frontendv1.FeedId) bool {
	s := r.state(ws)
	f := r.feed(s, feed)
	w, ok := s.readers[reader]
	if !ok || w.top || w.oldest == nil {
		return false
	}
	id := target.GetValue()
	rank, held := f.rank[id]
	if !held || f.nonDurable[id] || rank.key < *w.oldest {
		return false
	}
	durable, _ := r.deliverable(s, f, "load_through")
	for _, delivered := range durable {
		if delivered == id {
			return true
		}
	}
	return false
}

// LoadOlder loads the ROOT feed's next older store page for a feature that
// reads held rows past the oldest one loaded (selection stepping). No reader's
// page delivers it, so it is a pushed load (loadPushed). False when nothing
// older can be loaded.
func (r *resolver) LoadOlder(ctx context.Context, ws ids.WorkspaceID) (bool, error) {
	return r.loadPushed(ctx, ws, feedid.Feed{Root: true}, false)
}
