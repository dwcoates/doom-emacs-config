package db

import (
	"context"
	"database/sql"
	"sync"
)

// THE STORE IS THE SINGLE WRITER PROCESS, SO ITS WRITES ARE SERIALIZED IN
// PROCESS AND A BATCH IS NEVER REFUSED FOR A SIBLING'S LOCK.
//
// Nothing else opens this database by design — the store owns the file and
// serves every producer over its socket — so every writer SQLite could ever
// arbitrate between is one of this process's own goroutines. Leaving that
// arbitration to SQLite meant two of the store's connections both issuing
// BEGIN IMMEDIATE, one of them waiting out the whole `busy_timeout` and then
// being REFUSED: on 2026-09-13 the sidecar's full re-ingestion after a store
// reset met the shim mid-write and produced nine `store.db.write-batch` errors
// reading "begin write transaction: database is locked (5) (SQLITE_BUSY)",
// each one a batch handed back to its caller's retry, plus five
// `store.db.slow-query` warnings whose whole 5199ms was the 5000ms timeout
// being burned before the refusal.
//
// A queue answers what a timeout cannot: a batch WAITS ITS TURN, bounded only
// by its own request context, and then writes. A caller that hangs up while
// queued gets its own cancellation back, not a storage failure it would retry.
//
// THE QUEUE HAS TWO TIERS, AND INTERACTIVE ALWAYS GOES FIRST. The owner's rule
// (2026-09-23): an interactive write never queues behind a bulk one. An
// interrupt took 2m45s to reach the feed because the shim's turn ending sat in
// this one writer. So a released slot is handed to the oldest waiting
// INTERACTIVE writer whenever there is one, and to a BULK writer only when:
//
//   - no interactive writer is waiting, or
//   - InteractiveBurstBeforeBulk interactive writers in a row have been handed
//     the slot while a bulk writer waited.
//
// The second clause is the fairness rule: a steady stream of interactive writes
// cannot starve bulk forever, and the price is bounded because a bulk
// transaction is itself bounded (see bulkBounds in write.go). Either way, the
// longest an interactive write waits on bulk work is ONE bounded bulk
// transaction — the one already running when it arrived, or the one fairness
// lets through.
//
// THE REFUSAL PATH SURVIVES FOR AN OUTSIDE WRITER. Nothing else is supposed to
// open the file, but `sqlite3` at a shell, a stray second store racing the
// socket singleton check, or a backup tool all still can, and the DSN's
// busy_timeout plus the existing storage-failure refusal remain the answer for
// that. What the gate guarantees is only, and exactly, that the BUSY can never
// have come from this process.

// WriteClass is which queue a write takes into the one writer. It is stated by
// the CALLER — a producer's request names it, and the store's own maintenance
// names its own — and never inferred from what a write contains. The zero value
// is not a class, and a write carrying it is refused.
type WriteClass int

const (
	// WriteClassUnset is the zero value: no class was stated. Never queued.
	WriteClassUnset WriteClass = iota
	// WriteInteractive is live content a person is waiting on: the shim's turn
	// content, prompts, terminals and session updates.
	WriteInteractive
	// WriteBulk is background copying and upkeep: the sidecar's transcript and
	// spool ingestion, backfills, recoveries, and the store's own ledger sweep.
	WriteBulk
)

// String names the class the way every log record spells it.
func (c WriteClass) String() string {
	switch c {
	case WriteInteractive:
		return "interactive"
	case WriteBulk:
		return "bulk"
	default:
		return "unset"
	}
}

// valid reports whether c is a class the writer can queue.
func (c WriteClass) valid() bool { return c == WriteInteractive || c == WriteBulk }

// InteractiveBurstBeforeBulk is the fairness rule's N: once a bulk writer is
// waiting, the slot goes to it after this many consecutive interactive grants.
// A live turn writes one small batch per frame, so the cost to interactive is
// one bounded bulk transaction per N+1 grants at worst, and bulk is guaranteed
// that same share of the writer under any interactive load.
const InteractiveBurstBeforeBulk = 8

// writeWaiter is one writer queued on the slot. `granted` is closed when the
// slot has been handed to it, under the scheduler's lock.
type writeWaiter struct {
	class   WriteClass
	granted chan struct{}
}

// writeScheduler is the one writer's two-tier queue. The slot is handed
// DIRECTLY from the releasing writer to the next one under the lock, so there
// is no instant at which the slot is free while somebody is waiting — and so no
// race in which a newly arriving bulk writer could slip in ahead of a queued
// interactive one.
type writeScheduler struct {
	mu          sync.Mutex
	held        bool
	interactive []*writeWaiter
	bulk        []*writeWaiter
	// streak counts interactive grants made while a bulk writer was waiting.
	// It resets when bulk is granted and when no bulk writer is left waiting.
	streak int
}

// next picks who the slot goes to, under the lock. It returns nil when nobody
// is waiting.
func (s *writeScheduler) next() *writeWaiter {
	takeBulk := len(s.bulk) > 0 && (len(s.interactive) == 0 || s.streak >= InteractiveBurstBeforeBulk)
	switch {
	case takeBulk:
		w := s.bulk[0]
		s.bulk = s.bulk[1:]
		s.streak = 0
		return w
	case len(s.interactive) > 0:
		w := s.interactive[0]
		s.interactive = s.interactive[1:]
		if len(s.bulk) > 0 {
			s.streak++
		} else {
			s.streak = 0
		}
		return w
	default:
		return nil
	}
}

// release hands the slot to the next waiter, or frees it.
func (s *writeScheduler) release() {
	s.mu.Lock()
	defer s.mu.Unlock()
	if w := s.next(); w != nil {
		close(w.granted)
		return
	}
	s.held = false
}

// withdraw takes a waiter that gave up out of its queue, under the lock. It
// reports false when the waiter was no longer queued, which means the slot had
// already been handed to it.
func (s *writeScheduler) withdraw(w *writeWaiter) bool {
	queue := &s.interactive
	if w.class == WriteBulk {
		queue = &s.bulk
	}
	for i, queued := range *queue {
		if queued == w {
			*queue = append((*queue)[:i:i], (*queue)[i+1:]...)
			if len(s.bulk) == 0 {
				s.streak = 0
			}
			return true
		}
	}
	return false
}

// acquireWrite claims the process-wide write slot in the given class's queue,
// or reports the caller's own cancellation if the context ends while queued.
//
// An unset class is refused before anything is queued: a write with no class
// would have to be guessed into one, and a guess is how a bulk copy ends up
// ahead of a live turn. The context is checked next, so an already-canceled
// caller is answered deterministically.
func (d *DB) acquireWrite(ctx context.Context, class WriteClass) (release func(), err error) {
	if err := d.acquireSlot(ctx, class); err != nil {
		return nil, err
	}
	return d.releaseWrite, nil
}

// releaseWrite is every writer's release: it reads the WAL-index for the
// checkpoint job while it still holds the writer (checkpoint.go), then hands
// the slot on. The checkpoint itself releases through writes.release directly.
func (d *DB) releaseWrite() {
	d.observeWAL()
	d.writes.release()
}

// acquireSlot is acquireWrite without the release wrapper, for the one caller
// whose release must not wake the checkpoint job: the checkpoint itself.
func (d *DB) acquireSlot(ctx context.Context, class WriteClass) error {
	if !class.valid() {
		return invalidSitef(SiteWriteClassUnset, "write_class",
			"write_class is unset — every write states whether it is interactive or bulk, and the store never guesses")
	}
	if err := ctx.Err(); err != nil {
		return err
	}
	s := &d.writes
	s.mu.Lock()
	// The uncontended case takes the slot without ever announcing a queue, so
	// the hook below fires only for a writer that genuinely has to wait.
	if !s.held {
		s.held = true
		s.mu.Unlock()
		return nil
	}
	w := &writeWaiter{class: class, granted: make(chan struct{})}
	if class == WriteInteractive {
		s.interactive = append(s.interactive, w)
	} else {
		s.bulk = append(s.bulk, w)
	}
	s.mu.Unlock()
	if d.queuedForWrite != nil {
		d.queuedForWrite(class)
	}
	select {
	case <-w.granted:
		return nil
	case <-ctx.Done():
		s.mu.Lock()
		withdrawn := s.withdraw(w)
		s.mu.Unlock()
		if !withdrawn {
			// Granted in the same instant the caller gave up: the slot is
			// this caller's now, and it must be passed on or nobody writes
			// again.
			s.release()
		}
		return ctx.Err()
	}
}

// beginWrite is the ONE way a write transaction is opened in this package.
// It claims the write slot in the stated class's queue, then issues the DSN's
// BEGIN IMMEDIATE, and hands back the release the caller must defer BEFORE it
// defers its rollback — the slot is held until the transaction has ended, or
// the next writer would begin against a lock this one still holds and the gate
// would guarantee nothing.
func (d *DB) beginWrite(ctx context.Context, class WriteClass) (*sql.Tx, func(), error) {
	release, err := d.acquireWrite(ctx, class)
	if err != nil {
		return nil, nil, err
	}
	tx, err := d.sql.BeginTx(ctx, nil)
	if err != nil {
		release()
		return nil, nil, err
	}
	return tx, release, nil
}
