package wsm

import (
	"context"
	"database/sql"
	"errors"
	"time"

	"agent-repld/internal/dlog"
)

// This file is the durable half of a HIDDEN WORKSPACE'S CACHE LAPSE
// (conversation.v1.SessionCacheLapse, docs/protobuf-design/hidden-cache-lapse.md):
// a hidden workspace's keep-alives are held, so its prompt cache lapses on its
// own clock, and the shim states when, and whether the cold gate will ask
// after it. The daemon keeps the projection on the workspace's row so the
// hidden section counts down to it and the daemon stands the session down at
// it, across a daemon restart and whether or not the shim still runs.

// cacheLapseDDL is the layout-48 addition. It adds:
//
//   - workspaces.cache_lapses_at: when the hidden workspace's cache lapses,
//     in nanoseconds; NULL when no projection is known, which every existing
//     row is.
//   - workspaces.cache_lapse_gated_tokens: the context a resume after the
//     lapse re-reads when the cold gate will ask; NULL when it will not, or
//     when no projection is known.
//
// IT IS ADDITIVE: the previous build's writes name neither column, so a row it
// writes keeps NULL, which is "no projection known". The fresh-file schema
// appends this same text (schema.go).
const cacheLapseDDL = `
ALTER TABLE workspaces ADD COLUMN cache_lapses_at INTEGER;
ALTER TABLE workspaces ADD COLUMN cache_lapse_gated_tokens INTEGER;
`

// CacheLapse is a hidden workspace's projected cache lapse.
type CacheLapse struct {
	// LapsesAt is when the cache lapses. At or before now, it is cold.
	LapsesAt time.Time
	// GatedTokens is the context a resume after the lapse re-reads when the
	// cold gate will ask for it; nil when the context is under the gate's
	// floor and the resume continues without asking.
	GatedTokens *uint64
}

// Lapsed reports whether the cache has lapsed at NOW.
func (l CacheLapse) Lapsed(now time.Time) bool {
	return !now.Before(l.LapsesAt)
}

// decodeCacheLapse rebuilds a row's projected lapse. A projection belongs to a
// hidden workspace alone (SetHidden clears it, and SetCacheLapse writes only a
// hidden row), and a gated verdict without a lapse is no projection at all, so
// either is a corrupt row.
func decodeCacheLapse(ws Workspace, lapsesAt, gated sql.NullInt64) (*CacheLapse, error) {
	if !lapsesAt.Valid {
		if gated.Valid {
			return nil, &DecodeError{Table: "workspaces", Row: string(ws.ID), Field: "cache_lapse_gated_tokens", Err: errors.New("a gated verdict belongs to a projected lapse")}
		}
		return nil, nil
	}
	if !ws.Hidden {
		return nil, &DecodeError{Table: "workspaces", Row: string(ws.ID), Field: "cache_lapses_at", Err: errors.New("only a hidden workspace carries a projected cache lapse")}
	}
	lapse := &CacheLapse{LapsesAt: fromNanos(lapsesAt.Int64)}
	if gated.Valid {
		if gated.Int64 <= 0 {
			return nil, &DecodeError{Table: "workspaces", Row: string(ws.ID), Field: "cache_lapse_gated_tokens", Err: errors.New("a gated resume re-reads a positive context")}
		}
		tokens := uint64(gated.Int64)
		lapse.GatedTokens = &tokens
	}
	return lapse, nil
}

// SetCacheLapse records a hidden workspace's projected cache lapse, replacing
// the one that stood. It answers whether it was recorded: a workspace that is
// not hidden takes none, IN THE SAME STATEMENT that reads its visibility, so a
// projection that arrives after the user showed the workspace again can never
// land on it. An unknown workspace is refused (ErrNotFound).
func (s *store) SetCacheLapse(ctx context.Context, id WorkspaceID, lapse CacheLapse) (bool, error) {
	var gated any
	fields := dlog.Context{"workspace": string(id), "lapses_at": lapse.LapsesAt}
	if lapse.GatedTokens != nil {
		gated = int64(*lapse.GatedTokens)
		fields["gated_tokens"] = *lapse.GatedTokens
	}
	recorded := false
	err := s.write(ctx, "daemon.wsm.set_cache_lapse", fields, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx,
			`UPDATE workspaces SET cache_lapses_at = ?, cache_lapse_gated_tokens = ? WHERE id = ? AND hidden = 1`,
			nanos(lapse.LapsesAt), gated, id)
		if err != nil {
			return err
		}
		n, err := res.RowsAffected()
		if err != nil {
			return err
		}
		if n == 1 {
			recorded = true
			return nil
		}
		var exists bool
		if err := tx.QueryRowContext(ctx, `SELECT EXISTS (SELECT 1 FROM workspaces WHERE id = ?)`, id).Scan(&exists); err != nil {
			return err
		}
		if !exists {
			return requireOneRow(res, "wsm: workspace "+string(id))
		}
		return nil
	})
	return recorded, err
}
