package wsm

import (
	"context"
	"database/sql"
	"errors"

	"agent-repld/internal/dlog"
)

// THE VENDOR'S OWN TITLE for a workspace's conversation (its ai-title), kept
// so a workspace whose agent never set a summary still has words for
// `workspace find` once it is closed: the topbar draws the vendor's title in
// the summary's place while none stands, and before this the title lived only
// in the topbar's live state, gone with the session.
//
// IT IS NO SUMMARY. A workspace with a vendor title and no summary still owes
// one (summaries.go), so the two are kept apart and a reader prefers the
// summary.

// workspaceVendorTitlesDDL is the table, as both a fresh file and the
// layout-185 step create it.
const workspaceVendorTitlesDDL = `
CREATE TABLE workspace_vendor_titles (
  workspace_id TEXT PRIMARY KEY REFERENCES workspaces(id) ON DELETE CASCADE,
  title        TEXT NOT NULL
);
`

// errEmptyVendorTitle refuses an empty vendor title: no title is the absence
// of a row, never an empty one.
var errEmptyVendorTitle = errors.New("a vendor title is never empty; no title is no row")

// PutVendorTitle records the vendor's title for a workspace's conversation,
// replacing any it had. An unknown workspace is ErrNotFound; an empty title is
// a defect in the caller and is refused rather than stored.
func (s *store) PutVendorTitle(ctx context.Context, id WorkspaceID, title string) error {
	const op = "daemon.wsm.put_vendor_title"
	fields := dlog.Context{"workspace": string(id), "title": title}
	if title == "" {
		s.log.Error(op, "refused an empty vendor title", withError(fields, errEmptyVendorTitle))
		return errEmptyVendorTitle
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		var existing string
		if err := scanWorkspaceRow(ctx, tx, id, "id", &existing); err != nil {
			return err
		}
		_, err := tx.ExecContext(ctx,
			`INSERT INTO workspace_vendor_titles (workspace_id, title) VALUES (?, ?)
			 ON CONFLICT(workspace_id) DO UPDATE SET title = excluded.title`,
			id, title)
		return err
	})
}

// DropVendorTitle deletes a workspace's vendor title, reporting whether one
// stood. A workspace with none is not an error.
func (s *store) DropVendorTitle(ctx context.Context, id WorkspaceID) (bool, error) {
	var dropped bool
	err := s.write(ctx, "daemon.wsm.drop_vendor_title", dlog.Context{"workspace": string(id)}, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `DELETE FROM workspace_vendor_titles WHERE workspace_id = ?`, id)
		if err != nil {
			return err
		}
		dropped, err = touchedRow(res)
		return err
	})
	return dropped, err
}

// ListVendorTitles loads every stored vendor title by workspace,
// all-or-nothing: a workspace with none is absent, and an empty stored title
// fails the whole read as a DecodeError naming its workspace.
func (s *store) ListVendorTitles(ctx context.Context) (map[WorkspaceID]string, error) {
	var out map[WorkspaceID]string
	err := s.read(ctx, "daemon.wsm.list_vendor_titles", dlog.Context{}, func(ctx context.Context) error {
		rows, err := listRows(ctx, s.q(ctx), `SELECT workspace_id, title FROM workspace_vendor_titles`,
			func(row interface{ Scan(...any) error }) (summaryRow, error) {
				var r summaryRow
				if err := row.Scan(&r.id, &r.summary); err != nil {
					return summaryRow{}, err
				}
				if r.summary == "" {
					return summaryRow{}, &DecodeError{Table: "workspace_vendor_titles", Row: string(r.id), Err: errEmptyVendorTitle}
				}
				return r, nil
			})
		if err != nil {
			return err
		}
		out = make(map[WorkspaceID]string, len(rows))
		for _, r := range rows {
			out[r.id] = r.summary
		}
		return nil
	})
	if err != nil {
		return nil, err
	}
	return out, nil
}
