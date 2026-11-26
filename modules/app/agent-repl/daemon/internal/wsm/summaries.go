package wsm

import (
	"context"
	"database/sql"
	"errors"

	"agent-repld/internal/dlog"
)

// THE WORKSPACE SUMMARY the topbar draws: the long summary and the one-sentence
// short summary the workspace's own agent set to state its task
// (agentrepl.v1.SetWorkspaceSummary). It is kept here so a daemon restart
// draws every workspace's summary at once.
//
// NO ROW IS THE SUMMARY OWED. A new workspace has none, and a /clear drops the
// one that stood (DropWorkspaceSummary), because the conversation it described
// is gone. While none stands the daemon asks the workspace's agent for one, so
// "owed" is never a second fact that could disagree with the row.

// workspaceSummariesLayout56DDL is the layout-56 step exactly as it shipped,
// when the daemon distilled the summary itself and kept the digest it was
// distilled from beside it. Layout 110 dropped those columns
// (workspaceSummariesDropDigestDDL); a test pins that the migrated table
// matches a fresh one.
const workspaceSummariesLayout56DDL = `
CREATE TABLE workspace_summaries (
  workspace_id    TEXT PRIMARY KEY REFERENCES workspaces(id) ON DELETE CASCADE,
  title           TEXT NOT NULL,
  -- The digest the title was distilled from. All three are NULL together once
  -- a /clear has reset the cadence, and set together otherwise.
  digest_hash     TEXT,
  digest_prompts  INTEGER,
  digest_boundary INTEGER
);
`

// workspaceSummariesDDL is the table as a fresh file creates it (schema.go).
const workspaceSummariesDDL = `
CREATE TABLE workspace_summaries (
  workspace_id    TEXT PRIMARY KEY REFERENCES workspaces(id) ON DELETE CASCADE,
  title           TEXT NOT NULL
);
`

// workspaceSummariesDropDigestDDL is the layout-110 step: the summary is the
// workspace agent's own, so no digest stands beside it.
//
// THE TABLE IS REBUILT, NOT ALTERED. SQLite's DROP COLUMN rewrites the
// table's stored CREATE text, and the comment layout 56 wrote inside it
// leaves that text unparsable once the digest columns go. So the old table
// steps aside, the fresh one is created from the very text a fresh file uses,
// and every title is copied across.
const workspaceSummariesDropDigestDDL = `
ALTER TABLE workspace_summaries RENAME TO workspace_summaries_layout56;
` + workspaceSummariesDDL + `
INSERT INTO workspace_summaries (workspace_id, title) SELECT workspace_id, title FROM workspace_summaries_layout56;
DROP TABLE workspace_summaries_layout56;
`

// workspaceSummariesLongDDL is the layout-217 addition: the long summary
// beside the short one (`title`). NULL for a summary set before long
// summaries existed; never empty. IT IS ADDITIVE: a nullable column the
// previous build never names.
const workspaceSummariesLongDDL = `
ALTER TABLE workspace_summaries ADD COLUMN long_summary TEXT CHECK (long_summary IS NULL OR long_summary <> '');
`

// WorkspaceSummary is one workspace's stored summary.
type WorkspaceSummary struct {
	// Long is the long summary, two to four sentences. Empty only for a
	// summary set before long summaries existed, which the workspace still
	// owes (wssummary).
	Long string
	// Short is the one-sentence short summary the topbar draws as its title.
	// Never empty in a stored summary.
	Short string
}

// Complete reports whether both parts stand.
func (w WorkspaceSummary) Complete() bool {
	return w.Long != "" && w.Short != ""
}

// errEmptySummary refuses an empty summary: no summary is the absence of a
// row, never an empty one.
var errEmptySummary = errors.New("a summary is never empty; no summary is no row")

// errEmptyLongSummary refuses a summary put with no long summary: every
// summary put now carries both parts.
var errEmptyLongSummary = errors.New("a summary is put with both parts; its long summary is never empty")

// PutWorkspaceSummary records a workspace's summary, both parts, replacing any
// it had. An unknown workspace is ErrNotFound. The summary's FORMAT is the
// caller's to validate (the workspace verbs); an empty part is a defect in
// the caller and is refused rather than stored.
func (s *store) PutWorkspaceSummary(ctx context.Context, id WorkspaceID, summary WorkspaceSummary) error {
	const op = "daemon.wsm.put_workspace_summary"
	fields := dlog.Context{"workspace": string(id), "short_summary": summary.Short, "long_summary": summary.Long}
	switch {
	case summary.Short == "":
		s.log.Error(op, "refused an empty workspace summary", withError(fields, errEmptySummary))
		return errEmptySummary
	case summary.Long == "":
		s.log.Error(op, "refused a workspace summary with no long summary", withError(fields, errEmptyLongSummary))
		return errEmptyLongSummary
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		var existing string
		if err := scanWorkspaceRow(ctx, tx, id, "id", &existing); err != nil {
			return err
		}
		_, err := tx.ExecContext(ctx,
			`INSERT INTO workspace_summaries (workspace_id, title, long_summary) VALUES (?, ?, ?)
			 ON CONFLICT(workspace_id) DO UPDATE SET title = excluded.title, long_summary = excluded.long_summary`,
			id, summary.Short, summary.Long)
		return err
	})
}

// DropWorkspaceSummary deletes a workspace's summary, reporting whether one
// stood: the conversation it described is gone. A workspace with no summary
// is not an error.
func (s *store) DropWorkspaceSummary(ctx context.Context, id WorkspaceID) (bool, error) {
	var dropped bool
	err := s.write(ctx, "daemon.wsm.drop_workspace_summary", dlog.Context{"workspace": string(id)}, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `DELETE FROM workspace_summaries WHERE workspace_id = ?`, id)
		if err != nil {
			return err
		}
		dropped, err = touchedRow(res)
		return err
	})
	return dropped, err
}

// summaryColumns is every column a summary row is read from, in scanSummary's
// order: the ONE column list both summary reads share.
const summaryColumns = `workspace_id, title, long_summary`

// scanSummary reads one stored summary row of summaryColumns. An empty stored
// short summary is a DecodeError naming its workspace; a NULL long summary is
// one set before long summaries existed.
func scanSummary(row interface{ Scan(...any) error }) (WorkspaceID, WorkspaceSummary, error) {
	var (
		id    WorkspaceID
		short string
		long  sql.NullString
	)
	if err := row.Scan(&id, &short, &long); err != nil {
		return "", WorkspaceSummary{}, err
	}
	if short == "" {
		return "", WorkspaceSummary{}, &DecodeError{Table: "workspace_summaries", Row: string(id), Err: errEmptySummary}
	}
	return id, WorkspaceSummary{Long: long.String, Short: short}, nil
}

// WorkspaceSummaryOf loads a workspace's summary; the bool is false when none
// stands. A stored empty short summary is a DecodeError.
func (s *store) WorkspaceSummaryOf(ctx context.Context, id WorkspaceID) (WorkspaceSummary, bool, error) {
	var (
		out   WorkspaceSummary
		found bool
	)
	err := s.read(ctx, "daemon.wsm.workspace_summary", dlog.Context{"workspace": string(id)}, func(ctx context.Context) error {
		_, summary, err := scanSummary(s.q(ctx).QueryRowContext(ctx,
			`SELECT `+summaryColumns+` FROM workspace_summaries WHERE workspace_id = ?`, id))
		if errors.Is(err, sql.ErrNoRows) {
			out, found = WorkspaceSummary{}, false
			return nil
		}
		if err != nil {
			return err
		}
		out, found = summary, true
		return nil
	})
	if err != nil {
		return WorkspaceSummary{}, false, err
	}
	return out, found, nil
}

// summaryRow is one workspace's stored summary, as listRows reads it.
type summaryRow struct {
	id      WorkspaceID
	summary WorkspaceSummary
}

// ListWorkspaceSummaries loads every stored summary by workspace,
// all-or-nothing: a workspace with none is absent from the map, and one
// corrupt row fails the whole read as a DecodeError rather than being skipped.
// It is what a read over many workspaces (FindWorkspaces) takes, open and
// closed alike, in one query.
func (s *store) ListWorkspaceSummaries(ctx context.Context) (map[WorkspaceID]WorkspaceSummary, error) {
	var out map[WorkspaceID]WorkspaceSummary
	err := s.read(ctx, "daemon.wsm.list_workspace_summaries", dlog.Context{}, func(ctx context.Context) error {
		rows, err := listRows(ctx, s.q(ctx), `SELECT `+summaryColumns+` FROM workspace_summaries`,
			func(row interface{ Scan(...any) error }) (summaryRow, error) {
				id, summary, err := scanSummary(row)
				return summaryRow{id: id, summary: summary}, err
			})
		if err != nil {
			return err
		}
		out = make(map[WorkspaceID]WorkspaceSummary, len(rows))
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
