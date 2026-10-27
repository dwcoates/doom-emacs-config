// Package netwatch hears this machine's network changes from the operating
// system, so whatever waits for the network to come back is told at once
// instead of finding out at its next periodic check.
//
// THE NETWORK COMING BACK IS HEARD, NOT POLLED FOR (owner ruling, 2026-10-27).
// The shim probes the API on a fixed beat while anything waits on it, and the
// fleet backs a vendor start off while the network is unreachable. Both wait
// out a period that a reconnect can end early. The daemon owns the ONE
// watcher of the operating system's network events (a route socket on macOS,
// rtnetlink on Linux): a blocking read the kernel wakes, so it costs nothing
// while the network is quiet. Every change that can bring the network up or
// take it down closes the channel Changed answered, and the waits select on
// that channel next to their period.
//
// THE PERIOD STAYS. An outage the API or an upstream resolver ends on its own
// side changes nothing on this machine, so only the period can see it end; a
// change makes the next check happen now.
package netwatch

import (
	"context"
	"errors"
	"fmt"
	"sync"

	"agent-repld/internal/dlog"
)

const opWatch = "daemon.netwatch"

// Change is one network event the operating system reported, for the record.
type Change struct {
	// Kind names what changed: "address_added", "address_removed",
	// "default_route_added", "default_route_removed" or
	// "default_route_changed".
	Kind string
	// Interface is the interface's index, or its name when the event names
	// one.
	Interface string
	// Detail is the address or gateway the event carries, when it carries
	// one.
	Detail string
}

// Source is the operating system's stream of network events.
type Source interface {
	// Next blocks until the operating system reports a change that can bring
	// the network up or take it down, and answers it. Every other event is
	// read and passed over inside Next. After Close it answers an error.
	Next() (Change, error)
	// Close ends the stream, waking a Next that is blocked.
	Close() error
}

// Watcher is the daemon's one listener to the operating system's network
// events.
type Watcher struct {
	open func() (Source, error)
	log  dlog.Logger

	mu sync.Mutex
	// changed is closed, and replaced, at every change.
	changed chan struct{}
}

// New builds the watcher over the source open answers when Run starts.
func New(open func() (Source, error), log dlog.Logger) (*Watcher, error) {
	switch {
	case open == nil:
		return nil, errors.New("netwatch: the watcher needs a source of the operating system's network events")
	case log == nil:
		return nil, errors.New("netwatch: the watcher needs a logger")
	}
	return &Watcher{open: open, log: log, changed: make(chan struct{})}, nil
}

// Changed answers a channel that is closed at the next change. A caller that
// takes the channel BEFORE it starts what the change would cut short sees
// every change from that moment on, however late it comes to select on it.
func (w *Watcher) Changed() <-chan struct{} {
	w.mu.Lock()
	defer w.mu.Unlock()
	return w.changed
}

// Run reads the source until ctx ends. A source that cannot be opened, or
// that fails while ctx is live, is recorded at ERROR and ends Run with the
// cause: the waits keep their period, and the daemon has lost the faculty of
// hearing the network come back, which is said out loud.
func (w *Watcher) Run(ctx context.Context) error {
	src, err := w.open()
	if err != nil {
		w.log.Error(opWatch, "the operating system's network events could not be opened; a reconnect is noticed only at each wait's next periodic check", dlog.Context{
			"cause": err.Error(),
		})
		return fmt.Errorf("netwatch: open the network events: %w", err)
	}
	// THE SOURCE IS CLOSED ONCE, by whichever comes first: the end of ctx,
	// which wakes the Next that is blocked, or Run returning on its own.
	var closeOnce sync.Once
	closeSrc := func() {
		closeOnce.Do(func() {
			if err := src.Close(); err != nil {
				w.log.Error(opWatch, "the operating system's network events could not be closed", dlog.Context{"cause": err.Error()})
			}
		})
	}
	defer closeSrc()
	stopped := make(chan struct{})
	defer close(stopped)
	go func() {
		select {
		case <-ctx.Done():
			closeSrc()
		case <-stopped:
		}
	}()
	w.log.Debug(opWatch, "listening for the operating system's network events", nil)
	for {
		change, err := src.Next()
		if err != nil {
			if ctx.Err() != nil {
				return nil
			}
			w.log.Error(opWatch, "the operating system's network events failed; a reconnect is noticed only at each wait's next periodic check", dlog.Context{
				"cause": err.Error(),
			})
			return fmt.Errorf("netwatch: read the network events: %w", err)
		}
		w.announce(change)
	}
}

// announce closes the standing channel and stands a fresh one.
func (w *Watcher) announce(change Change) {
	w.mu.Lock()
	close(w.changed)
	w.changed = make(chan struct{})
	w.mu.Unlock()
	w.log.Info(opWatch, "the network changed; whatever waits on it is checked now", dlog.Context{
		"kind": change.Kind, "interface": change.Interface, "detail": change.Detail,
	})
}

// Follow calls fn once for every run of changes until ctx ends. A change
// that lands while fn runs is never missed: the channel it closes is taken
// before fn starts, so fn runs once more after it. Changes that land while
// fn runs are folded into that one more call.
func Follow(ctx context.Context, changed func() <-chan struct{}, fn func(context.Context)) {
	next := changed()
	for {
		select {
		case <-ctx.Done():
			return
		case <-next:
		}
		next = changed()
		fn(ctx)
	}
}

// Silent is a source that reports nothing until it is closed: the mocked
// vendor's, so a suite's daemon never hears the host's real network.
func Silent() (Source, error) {
	return &silent{closed: make(chan struct{})}, nil
}

type silent struct {
	once   sync.Once
	closed chan struct{}
}

func (s *silent) Next() (Change, error) {
	<-s.closed
	return Change{}, errors.New("netwatch: the silent source was closed")
}

func (s *silent) Close() error {
	s.once.Do(func() { close(s.closed) })
	return nil
}
