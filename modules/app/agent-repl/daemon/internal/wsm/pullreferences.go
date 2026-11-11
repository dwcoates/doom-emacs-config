package wsm

import (
	"context"
	"database/sql"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/pullrefs"
)

// pullReferencesDDL is the layout-136 addition: every pull request a
// workspace's text named by its full address or by its repository, as the
// feed learned it (pullrefs.Known), so a bare `#<number>` it writes later links
// to that pull request rather than into the workspace's own repository. Kept
// so a restarted daemon resolves them as the one before it did, without
// reading every earlier response again. IT IS ADDITIVE: one new table the
// previous build never names.
const pullReferencesDDL = `
CREATE TABLE pull_references (
  workspace_id TEXT NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  owner        TEXT NOT NULL CHECK (owner <> ''),
  repo         TEXT NOT NULL CHECK (repo <> ''),
  number       TEXT NOT NULL CHECK (number GLOB '[1-9]*' AND number NOT GLOB '*[^0-9]*'),
  learned_at   INTEGER NOT NULL,
  PRIMARY KEY (workspace_id, owner, repo, number)
);
`

// RecordPullReference records that workspace ID's text named REF, learned AT.
// A pull request recorded before keeps its first instant, so the order
// PullReferences answers is when each was first named.
func (s *store) RecordPullReference(ctx context.Context, id WorkspaceID, ref pullrefs.Ref, at time.Time) error {
	fields := dlog.Context{"workspace": string(id), "owner": ref.Owner, "repo": ref.Repo, "number": ref.Number}
	return s.write(ctx, "daemon.wsm.record_pull_reference", fields, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx,
			`INSERT INTO pull_references (workspace_id, owner, repo, number, learned_at) VALUES (?, ?, ?, ?, ?)
			 ON CONFLICT(workspace_id, owner, repo, number) DO NOTHING`,
			id, ref.Owner, ref.Repo, ref.Number, at.UnixMilli())
		return err
	})
}

// PullReferences answers every pull request workspace ID's text named, in the
// order it was first named.
func (s *store) PullReferences(ctx context.Context, id WorkspaceID) ([]pullrefs.Ref, error) {
	var out []pullrefs.Ref
	err := s.read(ctx, "daemon.wsm.pull_references", dlog.Context{"workspace": string(id)}, func(ctx context.Context) error {
		rows, err := s.q(ctx).QueryContext(ctx,
			`SELECT owner, repo, number FROM pull_references WHERE workspace_id = ? ORDER BY learned_at, rowid`, id)
		if err != nil {
			return err
		}
		defer rows.Close()
		var loaded []pullrefs.Ref
		for rows.Next() {
			var ref pullrefs.Ref
			if err := rows.Scan(&ref.Owner, &ref.Repo, &ref.Number); err != nil {
				return err
			}
			loaded = append(loaded, ref)
		}
		if err := rows.Err(); err != nil {
			return err
		}
		out = loaded
		return nil
	})
	return out, err
}
