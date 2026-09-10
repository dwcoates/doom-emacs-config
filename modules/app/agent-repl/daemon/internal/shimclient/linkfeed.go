package shimclient

import "sync"

// linkFeed publishes LinkState transitions to one consumer IN ORDER and
// without dropping any: a connectivity truth that skipped "dead" would leave
// a client painted as merely redialing forever. The queue is unbounded
// because the alphabet is four states and the transitions are few.
type linkFeed struct {
	mu      sync.Mutex
	pending []LinkState
	last    LinkState
	hasLast bool
	closed  bool
	wake    chan struct{}
	out     chan LinkState
	done    chan struct{}
}

// newLinkFeed starts the feed's pump.
func newLinkFeed() *linkFeed {
	f := &linkFeed{
		wake: make(chan struct{}, 1),
		out:  make(chan LinkState),
		done: make(chan struct{}),
	}
	go f.pump()
	return f
}

// states is the transition channel. It closes when the feed closes.
func (f *linkFeed) states() <-chan LinkState { return f.out }

// publish enqueues a transition, collapsing a repeat of the state already
// published (a transition to where we already are is not a transition).
func (f *linkFeed) publish(s LinkState) {
	f.mu.Lock()
	if f.closed || (f.hasLast && f.last == s) {
		f.mu.Unlock()
		return
	}
	f.last, f.hasLast = s, true
	f.pending = append(f.pending, s)
	f.mu.Unlock()

	select {
	case f.wake <- struct{}{}:
	default:
	}
}

// close ends the feed once everything already published has been delivered.
func (f *linkFeed) close() {
	f.mu.Lock()
	if f.closed {
		f.mu.Unlock()
		return
	}
	f.closed = true
	f.mu.Unlock()

	select {
	case f.wake <- struct{}{}:
	default:
	}
}

// pump delivers pending transitions in order, then closes out.
func (f *linkFeed) pump() {
	defer close(f.out)
	for {
		f.mu.Lock()
		if len(f.pending) == 0 {
			closed := f.closed
			f.mu.Unlock()
			if closed {
				return
			}
			<-f.wake
			continue
		}
		next := f.pending[0]
		f.pending = f.pending[1:]
		f.mu.Unlock()
		f.out <- next
	}
}
