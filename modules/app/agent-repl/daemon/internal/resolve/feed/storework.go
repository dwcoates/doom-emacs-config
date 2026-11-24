package feed

import (
	"context"
	"sync"
)

// THE RESOLVER NEVER HOLDS ITS MUTEX ACROSS A STATE-DATABASE CALL. The mutex
// is every workspace's feed's, and the state database has one connection
// that a handover keeps busy for seconds: on the 2026-11-23 19:29 handover
// UpsertDurable held the mutex 1110ms while its row write waited 1108ms for
// the connection, the replay held it while the pull references were read, and
// every workspace's feed opening waited 1.0-3.6s behind those holds
// (daemon.feed.lock_held, daemon.feed.opening_reads).
//
// Two doors keep the store off the mutex:
//
//   - READS are made before the mutex is taken. A workspace's facts the feed
//     loads once (its log sink, its durable rows, its answered turn sets, the
//     pull requests its text named) are loaded by prime (prime.go), which
//     every entry point naming a workspace calls first; under the mutex they
//     are only installed.
//   - WRITES, and every read whose need is only known under the mutex, are
//     queued on the hold that decides them (afterUnlock) and run in the order
//     the holds released, once the mutex is released. The goroutine that
//     released runs them, or waits for them when another goroutine is
//     already running the queue, so a caller returns only once its own work
//     is done: no write is left behind for a shutdown to lose, and two writes
//     of one row can never be reordered.
//
// A queued step that must settle its outcome in the feed takes the mutex
// through its storeHold (settle), never through Lock: work queued by a settle
// joins the queue being drained and is run by the same drainer, so no
// goroutine ever waits for a drain it is itself running.

// storeStep is one queued piece of store work. It runs OFF the resolver's
// mutex; it reaches the feed again only through HOLD.
type storeStep func(hold storeHold)

// storeHold is what a queued step settles through.
type storeHold struct {
	r *resolver
}

// settle runs FN under the resolver's mutex. Any work FN queues
// (afterUnlock) is run by the drain this step is part of, after this step.
func (h storeHold) settle(fn func()) {
	h.r.mu.Lock()
	defer h.r.mu.unlockInDrain()
	fn()
}

// afterUnlock queues STEP on the current hold: it runs once the mutex is
// released, after every step an earlier hold queued. Called with r.mu held.
func (r *resolver) afterUnlock(step storeStep) {
	if !r.mu.Held() {
		panic("feed: store work was queued outside a hold of the resolver's mutex")
	}
	r.mu.pending = append(r.mu.pending, step)
}

// storeQueue is the steps released holds queued, in release order, and
// whether a goroutine is draining them. Its own mutex guards it, never the
// resolver's.
type storeQueue struct {
	mu       sync.Mutex
	batches  []*storeBatch
	draining bool
}

// storeBatch is one released hold's steps, and the signal closed once they
// have all run.
type storeBatch struct {
	steps []storeStep
	done  chan struct{}
}

// submit hands BATCH to the queue and returns once it has run: draining the
// queue on this goroutine when no other goroutine is, else waiting for the
// drainer to run it.
func (q *storeQueue) submit(r *resolver, batch *storeBatch) {
	q.mu.Lock()
	q.batches = append(q.batches, batch)
	if q.draining {
		q.mu.Unlock()
		<-batch.done
		return
	}
	q.draining = true
	q.mu.Unlock()
	q.drain(r)
}

// enqueue hands BATCH to the queue a drain on this goroutine is already
// running, which runs it after every batch before it.
func (q *storeQueue) enqueue(batch *storeBatch) {
	q.mu.Lock()
	defer q.mu.Unlock()
	if !q.draining {
		panic("feed: a settle's store work was queued with no drain running it")
	}
	q.batches = append(q.batches, batch)
}

// drain runs every batch on the queue, in order, until it is empty. Exactly
// one drain runs at a time: the flag is cleared under the queue's mutex in
// the same critical section that finds the queue empty, so a batch handed
// over while the drain is finishing is never left behind.
func (q *storeQueue) drain(r *resolver) {
	for {
		q.mu.Lock()
		if len(q.batches) == 0 {
			q.draining = false
			q.mu.Unlock()
			return
		}
		batch := q.batches[0]
		q.batches = q.batches[1:]
		q.mu.Unlock()
		q.run(r, batch)
	}
}

// run runs BATCH's steps and signals it done, even when a step panics: a
// batch that never signalled would hold its waiter forever.
func (q *storeQueue) run(r *resolver, batch *storeBatch) {
	defer close(batch.done)
	for _, step := range batch.steps {
		step(storeHold{r: r})
	}
}

// storeOutcome is a queued write's answer, set once the hold that queued it
// is released and the write has run (writeAfterUnlock).
type storeOutcome struct {
	err error
}

// writeAfterUnlock queues WRITE on the current hold (afterUnlock) and answers
// its outcome, set once the hold is released. A write that fails is told to
// FAILED, one that succeeds to RECORDED (nil for none); both run off the
// lock, so they log through a logger resolved under it and raise through the
// topbar's own lock, never the resolver's. Called with r.mu held.
func (r *resolver) writeAfterUnlock(write func(context.Context) error, failed func(error), recorded func()) *storeOutcome {
	outcome := &storeOutcome{}
	r.afterUnlock(func(storeHold) {
		if err := write(context.Background()); err != nil {
			outcome.err = err
			failed(err)
			return
		}
		if recorded != nil {
			recorded()
		}
	})
	return outcome
}
