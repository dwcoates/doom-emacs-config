package main

import (
	"context"
	"errors"
	"fmt"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-store/internal/db"
	"agentrepl/shim-store/internal/server"
)

// storeAdapter is the ONE seam between the database and the service.
//
// internal/server declares the storage contract in its own vocabulary and
// imports nothing of internal/db, so the service's validation, refusal,
// tokening, fan-out and shutdown behaviour is testable with a fake and no
// SQLite at all. This adapter is where the db package's structurally identical
// result types and its three sentinels are carried across; it is deliberately
// mechanical, and adding logic here would put behaviour outside both packages'
// tests.
type storeAdapter struct{ db *db.DB }

var _ server.Store = storeAdapter{}

func (a storeAdapter) WriteBatch(ctx context.Context, producer string, batch *storev1.EntryBatch) (server.WriteResult, error) {
	result, err := a.db.WriteBatch(ctx, producer, batch)
	if err != nil {
		return server.WriteResult{}, adaptError(err)
	}
	lines := make([]server.LineWritten, 0, len(result.Lines))
	for _, line := range result.Lines {
		lines = append(lines, server.LineWritten{AgentID: line.AgentID, Line: line.Line, WriteSeq: line.WriteSeq})
	}
	return server.WriteResult{Written: result.Written, Absorbed: result.Absorbed, Lines: lines}, nil
}

func (a storeAdapter) OpenPage(ctx context.Context, agentID string, pageSize uint32, knownThrough *storev1.StoreItemPointer) (server.OpenedPage, error) {
	opened, err := a.db.OpenPage(ctx, agentID, pageSize, knownThrough)
	if err != nil {
		return server.OpenedPage{}, adaptError(err)
	}
	return server.OpenedPage{Page: opened.Page, PinSeq: opened.PinSeq}, nil
}

func (a storeAdapter) ReadPage(ctx context.Context, agentID string, pageSize uint32, after *storev1.StoreItemPointer) (*storev1.ReadAgentPageSuccess, error) {
	page, err := a.db.ReadPage(ctx, agentID, pageSize, after)
	if err != nil {
		return nil, adaptError(err)
	}
	return page, nil
}

func (a storeAdapter) LinesSince(ctx context.Context, agentID string, afterSeq uint64) ([]server.LineWritten, error) {
	written, err := a.db.LinesSince(ctx, agentID, afterSeq)
	if err != nil {
		return nil, adaptError(err)
	}
	lines := make([]server.LineWritten, 0, len(written))
	for _, line := range written {
		lines = append(lines, server.LineWritten{AgentID: line.AgentID, Line: line.Line, WriteSeq: line.WriteSeq})
	}
	return lines, nil
}

func (a storeAdapter) LiveWork(ctx context.Context) (*storev1.GetLiveWorkSuccess, error) {
	live, err := a.db.LiveWork(ctx)
	if err != nil {
		return nil, adaptError(err)
	}
	return live, nil
}

func (a storeAdapter) Cursors(ctx context.Context, fileID *string) ([]*storev1.CursorState, error) {
	cursors, err := a.db.Cursors(ctx, fileID)
	if err != nil {
		return nil, adaptError(err)
	}
	return cursors, nil
}

func (a storeAdapter) Close() error { return a.db.Close() }

// adaptError carries a db sentinel onto the server's, keeping the original
// message as the failure detail. An unclassified error stays unclassified and
// is reported by the server as a database failure — never softened.
func adaptError(err error) error {
	switch {
	case errors.Is(err, db.ErrInvalid):
		return fmt.Errorf("%w: %s", server.ErrInvalid, err)
	case errors.Is(err, db.ErrStalePointer):
		return fmt.Errorf("%w: %s", server.ErrStalePointer, err)
	case errors.Is(err, db.ErrStorage):
		return fmt.Errorf("%w: %s", server.ErrStorage, err)
	default:
		return err
	}
}
