package geometry

import (
	"context"
	"fmt"
	"sort"
)

// List returns every recorded geometry, ordered by workspace key.
//
// It is the read side of the map for callers that ask "which workspaces does
// agent-repl know about?" rather than "where does THIS workspace merge to?".
// The ordering is deterministic so a caller rendering a numbered menu over the
// result gives the same number to the same workspace on every run.
//
// A row that fails to scan is an ERROR, never a skipped row: a half-read map
// presented as the whole map is how a workspace silently disappears from a
// listing that a human then treats as exhaustive.
func (s *Store) List(ctx context.Context) ([]Record, error) {
	rows, err := s.db.QueryContext(ctx, `
		SELECT workspace, source_branch, source_dir, target_dir, origin
		FROM workspace_merge_geometry
	`)
	if err != nil {
		s.logf("geometry: list FAILED: %v", err)
		return nil, fmt.Errorf("geometry: list: %w", err)
	}
	defer func() { _ = rows.Close() }()

	var out []Record
	for rows.Next() {
		var rec Record
		var origin string
		if err := rows.Scan(&rec.Workspace, &rec.SourceBranch, &rec.SourceDir, &rec.TargetDir, &origin); err != nil {
			s.logf("geometry: list SCAN FAILED: %v", err)
			return nil, fmt.Errorf("geometry: list scan: %w", err)
		}
		rec.Origin = Origin(origin)
		out = append(out, rec)
	}
	if err := rows.Err(); err != nil {
		s.logf("geometry: list ITERATION FAILED: %v", err)
		return nil, fmt.Errorf("geometry: list iterate: %w", err)
	}
	sort.Slice(out, func(i, j int) bool { return out[i].Workspace < out[j].Workspace })
	return out, nil
}
