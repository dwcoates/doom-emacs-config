package kernelevent

import (
	"agent-repld/internal/inflight"
	"context"
	"errors"
	"fmt"
	"os"
)

// ErrNothingToWatch is Follow's error once none of its paths exists.
var ErrNothingToWatch = errors.New("kernelevent: none of the followed paths exists")

// ErrUnwatchable is Watch.Add's error for a path the kernel cannot watch
// itself, a unix socket on Darwin. Its directory reports it made or removed,
// so Follow skips it as it skips a missing path.
var ErrUnwatchable = errors.New("kernelevent: the kernel cannot watch this kind of file")

// Follow reports changes under PATHS on the channel it answers, until ctx
// ends or the stop it answers is called: nil once per change (changes that
// land before the reader takes one are coalesced into it), or an error once,
// after which nothing more is sent.
//
// EVERY PATH IS RE-WATCHED AFTER EACH CHANGE, so a path made, removed or
// replaced later (a log rotated, a database's -wal file recreated, a
// directory made) is watched as soon as it exists. A missing path is
// skipped; once none of PATHS exists the error is ErrNothingToWatch.
//
// NO CHANGE IS MISSED. The first watch is registered before Follow returns,
// and each later one before the change that preceded it is reported, so a
// reader that checks its condition after Follow returns and again after
// every receive sees every change.
//
// The stop returns only once the watch has ended.
func Follow(ctx context.Context, paths ...string) (<-chan error, func()) {
	ctx, cancel := context.WithCancel(ctx)
	out := make(chan error, 1)
	done := make(chan struct{})
	w, err := watchExisting(paths)
	if err != nil {
		out <- err
		close(done)
		return out, cancel
	}
	inflight.Standing(func() {
		defer close(done)
		for {
			err := w.Wait(ctx)
			w.Close()
			if ctx.Err() != nil {
				return
			}
			if err == nil {
				w, err = watchExisting(paths)
			}
			if err != nil {
				select {
				case out <- err:
				case <-ctx.Done():
				}
				return
			}
			select {
			case out <- nil:
			default:
				// A change is already waiting to be read: this one is in it.
			}
		}
	})
	return out, func() {
		cancel()
		<-done
	}
}

// watchExisting watches every one of PATHS that exists.
func watchExisting(paths []string) (*Watch, error) {
	w, err := NewWatch()
	if err != nil {
		return nil, err
	}
	watched := 0
	for _, p := range paths {
		err := w.Add(p)
		if errors.Is(err, os.ErrNotExist) || errors.Is(err, ErrUnwatchable) {
			continue
		}
		if err != nil {
			w.Close()
			return nil, err
		}
		watched++
	}
	if watched == 0 {
		w.Close()
		return nil, fmt.Errorf("%w: %v", ErrNothingToWatch, paths)
	}
	return w, nil
}

// Source starts one followed report of changes: Follow's shape, which
// wsm.DB.Changes shares.
type Source func(ctx context.Context) (<-chan error, func())

// FollowPaths is Follow as a Source.
func FollowPaths(paths ...string) Source {
	return func(ctx context.Context) (<-chan error, func()) { return Follow(ctx, paths...) }
}

// Merge reports every change any of SOURCES reports, on Follow's terms: nil
// once per change (coalesced), or the first error any source reports, after
// which nothing more is sent. Every source is started before Merge returns,
// and the stop returns once every one has stopped.
func Merge(ctx context.Context, sources ...Source) (<-chan error, func()) {
	ctx, cancel := context.WithCancel(ctx)
	out := make(chan error, 1)
	stops := make([]func(), 0, len(sources))
	done := make(chan struct{}, len(sources))
	for _, src := range sources {
		changes, stop := src(ctx)
		stops = append(stops, stop)
		inflight.Standing(func() {
			defer func() { done <- struct{}{} }()
			for {
				select {
				case <-ctx.Done():
					return
				case err := <-changes:
					if err != nil {
						select {
						case out <- err:
						case <-ctx.Done():
						}
						cancel()
						return
					}
					select {
					case out <- nil:
					default:
					}
				}
			}
		})
	}
	return out, func() {
		cancel()
		for range sources {
			<-done
		}
		for _, stop := range stops {
			stop()
		}
	}
}
