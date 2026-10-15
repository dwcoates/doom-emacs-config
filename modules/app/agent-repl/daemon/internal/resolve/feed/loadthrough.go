package feed

import (
	"context"
	"errors"
	"fmt"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
)

// ErrTargetNotFound is a LoadThrough whose walk reached the conversation's
// start without drawing its target: no entry of the conversation corresponds
// to it. The pages already handed over stay loaded.
var ErrTargetNotFound = errors.New("feed: the walk reached the conversation's start without drawing the target row")

// LoadThrough answers LoadFeedThrough (endpoint_load_feed_through.proto):
// walk the ROOT feed older from directly above OLDESTHELD — the order key of
// the oldest row the reader holds — one page at a time, each read from the
// store when the daemon does not hold it, until TARGET has been served,
// handing every page to EMIT as it is served. It answers the target as the
// daemon serves it.
//
// THE WALK BEGINS WHERE A GetFeedPage `older_than` WOULD, so the pages join
// what the reader holds with no gap and no repeat. A reader holding no row
// (OLDESTHELD nil) begins at the top, so its first page is the newest.
//
// It fails with ErrTargetNotFound when the walk reaches the start without the
// target, ErrHistoryUnavailable (wrapped) when a page could not be read or no
// source is up to read the next one from, and EMIT's own error when the page
// could not be handed over. Pages already handed over stay loaded either way.
func (r *resolver) LoadThrough(ctx context.Context, ws ids.WorkspaceID, oldestHeld *string, target *frontendv1.FeedId, emit func(*frontendv1.FeedPage) error) (*frontendv1.FeedId, error) {
	mu := r.loadMu(ws)
	mu.Lock()
	defer mu.Unlock()
	root := feedid.Feed{Root: true}
	w := &walk{oldest: oldestHeld, top: oldestHeld == nil}

	step := pageStep{}
	pages := 0
	for {
		r.mu.Lock()
		if r.targetServed(ws, root, w, target) {
			r.mu.Unlock()
			r.lockedLogger(ws).Info("daemon.feed.load_through_reached",
				"a walk to a target row reached it; every page between was handed to the reader",
				dlog.Context{"target": target.GetValue(), "pages": pages})
			return target, nil
		}
		page, _, plan, err := r.stepPage(ws, root, w, &step)
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
				dlog.Context{"target": target.GetValue(), "pages": pages, "no_source": step.noSource})
			return nil, ErrTargetNotFound
		}
		if step.unsourced {
			// NO SOURCE IS NOT THE CONVERSATION'S START: the target may lie on a
			// page no shim is up to read, so the walk is history unavailable,
			// never not found.
			r.lockedLogger(ws).Info("daemon.feed.load_through_no_source",
				"a walk to a target row ran out of held rows with no source up to read older ones",
				dlog.Context{"target": target.GetValue(), "pages": pages})
			return nil, fmt.Errorf("%w: %w", ErrHistoryUnavailable, ErrNoHistorySource)
		}
		if err := emit(page); err != nil {
			return nil, fmt.Errorf("feed: a page of the walk to %q could not be handed over: %w", target.GetValue(), err)
		}
		pages++
	}
}

// targetServed reports whether walk W reaches TARGET: it is a row of the feed
// that delivery serves, at or above the oldest row the reader holds. Called
// with r.mu held.
func (r *resolver) targetServed(ws ids.WorkspaceID, feed feedid.Feed, w *walk, target *frontendv1.FeedId) bool {
	s := r.state(ws)
	f := r.feed(s, feed)
	if w.top || w.oldest == nil {
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
