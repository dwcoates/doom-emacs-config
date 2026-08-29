// Package server serves store.v1.ShimStore over a unix domain socket with
// Connect (binary and JSON codecs, HTTP/1.1 and h2c), mints and consumes watch
// tokens, and fans written page lines out to standing watchers.
//
// IT OWNS NO STORAGE. Everything durable is behind the Store interface below,
// which `*db.DB` satisfies through the adapter main.go wires. The split is what
// lets this package's behaviour — validation, refusal, tokens, the gapless
// replay-to-live handoff, overflow, shutdown — be tested without a database.
package server

import (
	"context"
	"errors"

	storev1 "agentrepl/proto/store/v1"
)

// LineWritten is one page line the storage layer committed: the book it
// belongs to, the line with its pointer, and the store-internal write ordinal
// that orders it against every other write.
//
// WriteSeq NEVER REACHES THE WIRE. It is the watch pin: a subscriber replays
// everything above the pin it was opened at, and the live fan-out dedupes
// against the replay by this ordinal.
type LineWritten struct {
	AgentID  string
	Line     *storev1.StoreLineAt
	WriteSeq uint64
}

// WriteResult is one batch's outcome. Written and Absorbed both mean the batch
// is durable: absorption of a replayed write_id is success, not a refusal.
type WriteResult struct {
	Written  int
	Absorbed int
	Lines    []LineWritten
}

// OpenedPage is the opening page plus the write ordinal the page was read at,
// taken inside the page's own read transaction so the tail that follows misses
// nothing and doubles nothing.
type OpenedPage struct {
	Page   *storev1.AgentSessionPage
	PinSeq uint64
}

// Store is the durable half of the store, as this package needs it.
//
// Every method takes a context and returns an error wrapping one of the three
// sentinels below; the server maps the sentinel to a failure detail and logs it
// exactly once.
type Store interface {
	WriteBatch(ctx context.Context, producer string, batch *storev1.EntryBatch) (WriteResult, error)
	OpenPage(ctx context.Context, agentID string, pageSize uint32, knownThrough *storev1.StoreItemPointer) (OpenedPage, error)
	ReadPage(ctx context.Context, agentID string, pageSize uint32, after *storev1.StoreItemPointer) (*storev1.ReadAgentPageSuccess, error)
	LinesSince(ctx context.Context, agentID string, afterSeq uint64) ([]LineWritten, error)
	LiveWork(ctx context.Context) (*storev1.GetLiveWorkSuccess, error)
	Cursors(ctx context.Context, fileID *string) ([]*storev1.CursorState, error)
	Close() error
}

// The three storage-layer sentinels, matched with errors.Is.
//
// They are declared HERE rather than imported from internal/db so this package
// has no dependency on the storage implementation; main.go's adapter translates
// the db package's own sentinels onto these.
var (
	// ErrInvalid is a request the storage layer refuses on its own validation.
	ErrInvalid = errors.New("store: invalid request")
	// ErrStalePointer is a pointer that names no position in the named book.
	ErrStalePointer = errors.New("store: stale pointer")
	// ErrStorage is a database failure: the transaction committed nothing.
	ErrStorage = errors.New("store: storage failure")
)
