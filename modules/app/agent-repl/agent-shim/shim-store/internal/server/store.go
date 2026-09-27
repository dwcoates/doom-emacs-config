// Package server serves store.v1.ShimStore over a unix domain socket with
// Connect (binary and JSON codecs, HTTP/1.1 and h2c), mints and consumes watch
// tokens, and fans written page lines out to standing watchers.
//
// IT OWNS NO STORAGE. Everything durable is behind the Store interface below,
// which `*db.DB` satisfies directly — the result types and the three sentinels
// are ALIASES of internal/db's, so there is exactly one spelling of the
// contract and no adapter to drift. The interface still exists so this
// package's behaviour — validation, refusal, tokens, the gapless
// replay-to-live handoff, overflow, shutdown — is tested with a fake and no
// SQLite at all.
package server

import (
	"context"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-store/internal/db"
)

// The storage layer's result types, aliased so `*db.DB` satisfies Store with
// no conversion anywhere.
//
// LineWritten.WriteSeq NEVER REACHES THE WIRE: it is the watch pin, and the
// live fan-out dedupes against the replay by it. WriteResult.Absorbed is not a
// lesser success — a replayed batch whose write_ids all landed before is the
// same durable answer. OpenedPage.PinSeq is the ordinal the opening page was
// read at, taken inside the page's own read transaction so the tail that
// follows misses nothing and doubles nothing.
type (
	LineWritten    = db.LineWritten
	BashRowWritten = db.BashRowWritten
	WriteResult    = db.WriteResult
	SkippedEntry   = db.SkippedEntry
	OpenedPage     = db.OpenedPage
	BashRunReplay  = db.BashRunReplay
	// WriteClass is which queue a write takes into the store's one writer.
	WriteClass = db.WriteClass
)

// The write classes, re-exported under this package's own names.
const (
	WriteInteractive = db.WriteInteractive
	WriteBulk        = db.WriteBulk
)

// Store is the durable half of the store, as this package needs it.
//
// Every method takes a context and returns an error wrapping one of the three
// sentinels below; the server maps the sentinel to a failure detail and logs it
// exactly once.
type Store interface {
	// WriteBatch writes one batch in the stated class. ON FAILURE THE RESULT
	// STILL CARRIES WHAT COMMITTED: a bulk batch is split into bounded
	// transactions, and the lines its leading ones made durable must still be
	// published to live watchers.
	WriteBatch(ctx context.Context, producer string, class WriteClass, batch *storev1.EntryBatch, shapes []*storev1.ShapeObservation) (WriteResult, error)
	OpenPage(ctx context.Context, agentID string, pageSize uint32, knownThrough *storev1.StoreItemPointer) (OpenedPage, error)
	ReadPage(ctx context.Context, agentID string, pageSize uint32, after *storev1.StoreItemPointer) (*storev1.ReadAgentPageSuccess, error)
	LinesSince(ctx context.Context, agentID string, afterSeq uint64) ([]LineWritten, error)
	BashRun(ctx context.Context, runID string) (BashRunReplay, error)
	LiveWork(ctx context.Context, session string) (*storev1.GetLiveWorkSuccess, error)
	// AgentByVendorTask answers which agent of `session`'s lineage a vendor
	// task locator names; `found` false with no error is the not-found answer.
	AgentByVendorTask(ctx context.Context, session, vendorTaskID string) (agentID string, found bool, err error)
	Cursors(ctx context.Context, fileID *string) ([]*storev1.CursorState, error)
	ResidueShapes(ctx context.Context, kind *string, limit uint32, includeExample bool) ([]*storev1.ResidueShapeRow, error)
	Close() error
}

// The storage-layer sentinels, matched with errors.Is and re-exported
// under this package's own names so a reader of the refusal mapping below does
// not have to cross packages to see what is being matched.
var (
	// ErrInvalid is a request the storage layer refuses on its own validation.
	ErrInvalid = db.ErrInvalid
	// ErrStalePointer is a pointer that names no position in the named book.
	ErrStalePointer = db.ErrStalePointer
	// ErrStorage is a database failure: the transaction committed nothing.
	ErrStorage = db.ErrStorage
	// ErrUnknownAgent is a well-formed agent id naming no book of this store.
	ErrUnknownAgent = db.ErrUnknownAgent
)

// The database satisfies the contract as written — asserted here so a
// divergence is a compile error in this package rather than a wiring error in
// main.
var _ Store = (*db.DB)(nil)
