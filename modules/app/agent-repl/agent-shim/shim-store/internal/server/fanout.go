package server

import "sync"

// DefaultWatchBuffer is the per-subscriber frame buffer when --watch-buffer is
// not given. It is SUBSTANTIALLY above the ~1k a daemon bounce can burst,
// because the alternative to buffering a burst is ending a healthy watch.
const DefaultWatchBuffer = 8192

// subscriber is one standing WatchAgentSession: the book it follows, the
// bounded channel the fan-out hands lines to, and the overflow signal that
// ends it.
type subscriber struct {
	agentID   string
	tokenHash string
	lines     chan LineWritten
	// overflow is CLOSED, never sent on, when this subscriber could not keep
	// up. A closed channel is readable forever, so the watch loop cannot miss
	// the signal no matter which branch of its select won a race.
	overflow chan struct{}
	// dropped is the line count lost at the moment of overflow, for the
	// warning record. Written once, under the fan-out's lock, before overflow
	// is closed; read only after observing that close.
	dropped int
}

// fanout is the registry of standing watchers.
//
// PUBLISHING NEVER BLOCKS. Every send is non-blocking under the registry lock:
// a subscriber that cannot keep up is unsubscribed and signalled, so one slow
// reader can never stall a writer's transaction acknowledgement.
type fanout struct {
	mu     sync.Mutex
	buffer int
	subs   map[*subscriber]struct{}
}

func newFanout(buffer int) *fanout {
	if buffer <= 0 {
		buffer = DefaultWatchBuffer
	}
	return &fanout{buffer: buffer, subs: map[*subscriber]struct{}{}}
}

// subscribe registers a watcher for one book.
//
// IT IS CALLED BEFORE THE REPLAY QUERY, under this lock, which is what makes
// the replay-to-live handoff gapless: any line committed after this point is
// either found by the replay, delivered on this channel, or both — and the
// watch loop dedupes the "both" case by write ordinal.
func (f *fanout) subscribe(agentID, tokenHash string) *subscriber {
	sub := &subscriber{
		agentID:   agentID,
		tokenHash: tokenHash,
		lines:     make(chan LineWritten, f.buffer),
		overflow:  make(chan struct{}),
	}
	f.mu.Lock()
	defer f.mu.Unlock()
	f.subs[sub] = struct{}{}
	return sub
}

// unsubscribe removes a watcher. It is idempotent, so the watch loop's defer
// is safe after an overflow already removed the subscriber.
func (f *fanout) unsubscribe(sub *subscriber) {
	f.mu.Lock()
	defer f.mu.Unlock()
	delete(f.subs, sub)
}

// publish hands each line to every subscriber of its book and returns the
// subscribers that overflowed, so the caller logs one warning per victim.
func (f *fanout) publish(lines []LineWritten) []*subscriber {
	if len(lines) == 0 {
		return nil
	}
	var overflowed []*subscriber
	f.mu.Lock()
	defer f.mu.Unlock()
	for sub := range f.subs {
		remaining := 0
		for i, line := range lines {
			if line.AgentID != sub.agentID {
				continue
			}
			select {
			case sub.lines <- line:
			default:
				// The buffer is full. Count what this batch could not place,
				// signal the subscriber, and drop it from the registry: a
				// watcher that fell behind recovers by re-opening with
				// known_through, never by being silently thinned.
				for _, rest := range lines[i:] {
					if rest.AgentID == sub.agentID {
						remaining++
					}
				}
				sub.dropped = remaining
				close(sub.overflow)
				delete(f.subs, sub)
				overflowed = append(overflowed, sub)
			}
			if remaining > 0 {
				break
			}
		}
	}
	return overflowed
}

// subscribers is the number of standing watchers. Diagnostics only.
func (f *fanout) subscribers() int {
	f.mu.Lock()
	defer f.mu.Unlock()
	return len(f.subs)
}
