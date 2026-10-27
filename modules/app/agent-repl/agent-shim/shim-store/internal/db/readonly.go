package db

import (
	"context"

	storev1 "agentrepl/proto/store/v1"
)

// ReadOnly is the store as a reader other than its producers sees it: the page
// reads its read-only face serves (storeread.v1), and nothing else.
//
// THE DAEMON'S RELATIONSHIP WITH THE STORE IS READ-ONLY (owner ruling,
// 2026-10-27), and this type is where the store holds to it. It can write
// nothing, twice over: its method set is these three reads, pinned by
// TestReadOnlyExposesOnlyPageReads so a write cannot be added beside them
// unnoticed, and each runs in a read transaction on the read pool, which
// SQLite itself refuses a write on (`query_only`). A verb that would change
// the store belongs to its producers' service, never here.
type ReadOnly struct {
	db *DB
}

// ReadOnly is the database's read-only face.
func (d *DB) ReadOnly() ReadOnly {
	return ReadOnly{db: d}
}

// NewestPage is a book's newest page. STATED is the caller holding the
// producer's word that the book's agent is real, so an absent book is the
// empty page it is rather than an unknown agent (a vouched open).
func (r ReadOnly) NewestPage(ctx context.Context, agentID string, stated bool) (*storev1.AgentSessionPage, error) {
	opening := Repaint()
	if stated {
		opening = opening.Vouched()
	}
	opened, err := r.db.OpenPage(ctx, agentID, opening)
	if err != nil {
		return nil, err
	}
	return opened.Page, nil
}

// PageBefore is the page of a book placed strictly before a served pointer's
// line (ReadPage).
func (r ReadOnly) PageBefore(ctx context.Context, agentID string, after *storev1.StoreItemPointer) (*storev1.ReadAgentPageSuccess, error) {
	return r.db.ReadPage(ctx, agentID, after)
}

// PageThrough is a book's newest lines placed at or before an instant: the
// book as it stood then (ReadPageThrough).
func (r ReadOnly) PageThrough(ctx context.Context, agentID string, throughAtMs int64) (*storev1.ReadAgentPageSuccess, error) {
	return r.db.ReadPageThrough(ctx, agentID, throughAtMs)
}
