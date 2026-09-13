package db

import (
	"context"
	"database/sql"
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
// by its own request context, and then writes. The gate is a one-slot channel
// rather than a sync.Mutex precisely because a channel can be selected against
// ctx.Done() — a caller that hangs up while queued gets its own cancellation
// back, not a storage failure it would retry.
//
// THE REFUSAL PATH SURVIVES FOR AN OUTSIDE WRITER. Nothing else is supposed to
// open the file, but `sqlite3` at a shell, a stray second store racing the
// socket singleton check, or a backup tool all still can, and the DSN's
// busy_timeout plus the existing storage-failure refusal remain the answer for
// that. What the gate guarantees is only, and exactly, that the BUSY can never
// have come from this process.

// acquireWrite claims the process-wide write slot, or reports the caller's own
// cancellation if the context ends while queued.
//
// The context is checked FIRST so an already-canceled caller is answered
// deterministically: a bare select over a free gate and a done context picks
// between them at random.
func (d *DB) acquireWrite(ctx context.Context) (release func(), err error) {
	if err := ctx.Err(); err != nil {
		return nil, err
	}
	// The uncontended case takes the slot without ever announcing a queue, so
	// the hook below fires only for a writer that genuinely has to wait.
	select {
	case d.writeGate <- struct{}{}:
		return func() { <-d.writeGate }, nil
	default:
	}
	if d.queuedForWrite != nil {
		d.queuedForWrite()
	}
	select {
	case d.writeGate <- struct{}{}:
		return func() { <-d.writeGate }, nil
	case <-ctx.Done():
		return nil, ctx.Err()
	}
}

// beginWrite is the ONE way a write transaction is opened in this package.
// It claims the write slot, then issues the DSN's BEGIN IMMEDIATE, and hands
// back the release the caller must defer BEFORE it defers its rollback — the
// slot is held until the transaction has ended, or the next writer would begin
// against a lock this one still holds and the gate would guarantee nothing.
func (d *DB) beginWrite(ctx context.Context) (*sql.Tx, func(), error) {
	release, err := d.acquireWrite(ctx)
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
